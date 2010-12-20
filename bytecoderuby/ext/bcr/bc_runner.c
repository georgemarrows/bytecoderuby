#include "ruby.h"
#include "node.h"

#include "bc_runner.h"
#include "ruby_adds.h"

#include <setjmp.h>
#include <stdarg.h>

/* useful Ruby objects, populated in Init_Runner */
static VALUE rb_eLocalJumpError;
static VALUE mBytecode;
static VALUE cRunner;
static VALUE cWriter;
static VALUE cJumpTarget;
static ID to_ary;

/* debugging */
#define DEBUG_FLAG 0

#define DEBUG if (DEBUG_FLAG)
#define TRACE if (DEBUG_FLAG) bc_trace

#define TRC_FRAME    (1 << 0)
#define TRC_RUN      (1 << 1)
#define TRC_HANDLER  (1 << 2)
#define TRC_GC       (1 << 3)
#define TRC_C        (1 << 4)
#define TRC_COMPILE  (1 << 5)
#define TRC_STACK    (1 << 6)

#define TRACE_FLAGS (TRC_FRAME)
#define TRACE_FLAGS (TRC_FRAME | TRC_RUN | TRC_HANDLER | TRC_C | TRC_STACK)

#define HUMAN_PC(pc) ((pc) - current_frame->code->bytecode)

#define ASSERT_FLAG 1
#define ASSERT(cond, msg) if (ASSERT_FLAG && !(cond))                \
                          rb_bug("ASSERTION FAILED at line %d in %s" \
                                 "\n\t*** %s ***",                   \
                                 __LINE__,                           \
                                 __FILE__,                           \
                                 msg)

static void
bc_trace(int type, char *format, ...) {
  va_list args;
  int index;

  static char* trace_names[] = { "FRAME  ",
				 "RUN    ",
				 "HANDLER",
				 "GC     ",
				 "C      ",
				 "COMPILE",
				 "STACK  " };

  static int trace_types[] = { TRC_FRAME,
			       TRC_RUN,
			       TRC_HANDLER,
			       TRC_GC,
			       TRC_C,
			       TRC_COMPILE,
			       TRC_STACK };

  if ( !(type & TRACE_FLAGS) ) return;

  for (index = 0; index < 32; index++) {
    if (trace_types[index] == type)
      break;
  }

  va_start(args, format);
  fprintf(stderr, "%s ", trace_names[index]);
  vfprintf(stderr, format, args);
  va_end(args);
}

/* arbitrary constants to distinguish between bytecode and C frames */
#define FRAME_BYTECODE 0x1345234c
#define FRAME_C        0x98a767f1

/* Possible types of handler
   Must match declarations in writer.rb */
#define HANDLER_TYPE_RESCUE 1
#define HANDLER_TYPE_ENSURE 2

/* Possible jump types
   Must match the corresponding TAG_ defines from Ruby's eval.c */
#define TAG_NONE        0x0
#define TAG_RETURN	0x1
#define TAG_BREAK	0x2
#define TAG_NEXT	0x3
#define TAG_RETRY	0x4
#define TAG_REDO	0x5
#define TAG_RAISE	0x6
#define TAG_THROW	0x7
#define TAG_FATAL	0x8
#define TAG_GOTO_E      0x9

static char* jump_type_names[] = { "nothing",
				   "TAG_RETURN",
				   "TAG_BREAK",
				   "TAG_NEXT",
				   "TAG_RETRY",
				   "TAG_REDO",
				   "TAG_RAISE",
				   "TAG_THROW",
				   "TAG_FATAL",
				   "TAG_GOTO_E"  };


/*********************** Key structures *****************************/ 
typedef struct handler {

  /* Handlers for exceptions, ensures etc */

  /* These values are offsets from start of the bytecode */
  int start_pc;         /* start of region handled */
  int end_pc;           /* end of region handled */
  int handler_pc;       /* location to jump to if match */

  int handler_type;     /* whether an ensure or rescue handler */
  int storage;          /* where to find temporary storage for this handler */

} Handler;


typedef struct code {
  /* the bytecode itself */
  int *bytecode;

  /* the length of the bytecode */
  int bytecode_len;

  /* number of locals required (including arguments) */
  int num_locals;

  /* number of arguments */
  int num_args;   

  /* number of optional arguments -
     negative signals that method has a remainder argument */
  int num_opt_args;

  /* jump points to skip optional arg defaulting if an arg
     is supplied */
  unsigned char *opt_args;

  /* Jump points for exception handlers etc */
  Handler *handlers;

  /* number of jump_data blocks required by this code 
     (= nesting depth of rescues and ensures) */
  int num_jump_data;

} Code;


typedef struct closure {

  /* the code */
  Code *code;

  /* the environment */
  struct env *env;

} Closure;


typedef struct env {

  /* back pointer to previous environment */
  struct env *prev_env;

  /* pointer to our owner frame. Set to 0 when the frame becomes dead */
  struct frame *frame;

  /* true if the env has been moved to the heap */
  int on_heap_p;

  /* the value of self in this environment */
  VALUE self;

  /* the block captured by this environment */
  struct closure block;

  /* our locals - these run on beyond the end of the struct */
  VALUE locals;

} Env;



struct frame_common {

  int type;

  /* the previous frame in the call stack */
  struct frame *prev_frame;


};


typedef struct cframe {

  struct frame_common f;

  /* the Ruby block associated with this method call */
  Closure block;

} CFrame;


typedef struct frame { 

  /* fields common between bytecode and C frames
     NB *must* be first element of this struct because
     of casts */
  struct frame_common f;

  /* the code being run in this frame */
  Code *code;

  int ary_scatter_checkp;

  /* the pc to restart when execution returns to this frame */
  int   *pc;

  /* the stack entry to restart at when execution returns to this frame */
  VALUE *sp;

  /* the environment in use by this frame, which can be either in the
     frame (default) or on the heap (if captured in a closure) */
  Env *envp;

  /* storage for information about the current handler that is running */
  struct jump_data *jump_data;

  /* Followed by in order ..
     1. The jump_data.
     2. Initial environment for ths frame (could be moved to heap).
     3. The environment's local variables.
     4. Value stack. */

} Frame;


typedef struct jump_data {

  /* .. stack height to enter the handler */
  VALUE * sp;

  /* .. where to go on a rejump */
  int jump_type;

  Frame * tgt_frame;

  int * tgt_pc;

  /* .. what handlers to visit on the way */
  int handler_types;

  /* .. and what to stick on the stack when you get there */
  VALUE return_val;

} JumpData;


/* a thread - there's only one of these at the moment */
struct thread {

  /* storage for the thread's call frames */
  char *frame_stack;

  /* the current C frame - only valid if we're in the course 
     of a call to a C method */
  CFrame *current_frame;

  /* flag to show whether bytecode is being run */
  int on_bc_stack;

  /* global storage for jump_data - it all gets create here initially
     and then copied into handler specific storage as required */
  JumpData jump_data;

  /* used to transfer the correct jump_data across C calls */
  JumpData *current_jump_data;

} thread;


/******************************************************************
 ** Helpers
 ******************************************************************/

/* value stack manipulation */

#define PUSH_STACK(val) *(sp++) = (val)

#define POP_STACK() (*(--sp))


static char* bc_inspect(VALUE s) {
  if (rb_obj_is_kind_of(s, rb_cProc)) {
    /* FIXME need to_s for BCR procs */
    return "Proc";
  } else {
    return STR2CSTR(rb_funcall(s, rb_intern("inspect"), 0));
  }
}



/******************************************************************
 ** 'Methods' for key structures
 ******************************************************************/

/** Frame *********************************************************/

/* macros to make use of frame_alloc_* a bit clearer */
#define NO_BLOCK        closure_new(0, 0)
#define NO_ENV          ((Env *) 0)
#define NO_FRAME        ((Frame *) 0)

static JumpData *
frame_jump_data_start(Frame *frame) {

  /* Return location of the jump data for FRAME, viz on the stack 
     immediately after the frame's fixed struct */

  return (JumpData *) (frame + 1);

}

static Env *
frame_env_start(Frame *frame) {

  /* Return location of the initial environment for FRAME, viz on the
     stack, immediately after the frame's jump data */

  int num_jump_data = frame->code->num_jump_data;

  return (Env *) (frame_jump_data_start(frame) + num_jump_data);
}

static VALUE *
frame_stack_start(Frame *frame) {
  
  /* Return location for the initial value stack pointer for FRAME,
     viz immediately after the frame's environment, which needs space
     for the frames locals */

  int num_locals = frame->code->num_locals;

  VALUE *start_of_locals = (VALUE *) (frame_env_start(frame) + 1);

  return start_of_locals + num_locals;
}

/*
  Frame maintenance
 */
static void
frame_init_bc(Frame *frame, Frame *prev_frame, VALUE recv, 
	      Closure closure, Closure block) {

  frame->f.type             = FRAME_BYTECODE;
  frame->f.prev_frame       = prev_frame;
  frame->code               = closure.code;
  frame->jump_data          = frame_jump_data_start(frame);
  frame->envp               = frame_env_start(frame);
  frame->envp->frame        = frame;
  frame->envp->self         = recv;
  frame->envp->prev_env     = closure.env;
  frame->envp->block        = block;
  frame->envp->on_heap_p    = 0;
  frame->ary_scatter_checkp = 0;

}


static Frame *
frame_alloc_bc_on_bc( Frame *current_frame, 
		      VALUE recv,
		      Closure closure,
		      Closure block, 
		      int **pc, VALUE **sp, VALUE *oldsp, VALUE **locals) {

  Frame *frame = (Frame *) oldsp;

  /* store current frame pc & sp so that we can continue there on
     return to frame */
  current_frame->pc = *pc;
  current_frame->sp = *sp;

  frame_init_bc(frame, current_frame, recv, closure, block);

  *pc        = closure.code->bytecode;
  *sp        = frame_stack_start(frame);
  *locals    = &(frame->envp->locals);

  TRACE(TRC_FRAME, "entering (new) frame %p\n", frame);

  return frame;
}


static Frame *
frame_alloc_bc_on_c( CFrame *current_frame, 
		     VALUE recv,
		     Closure closure,
		     Closure block) {

  Frame *frame  = (Frame *) (current_frame + 1); 

  frame_init_bc(frame, (Frame *) current_frame, recv, closure, block);

  /* store initial pc & sp so that they can be picked up when frame
     is entered */
  frame->pc           = closure.code->bytecode;
  frame->sp           = frame_stack_start(frame);

  return frame;
}

 
static CFrame *
frame_alloc_c_on_bc( Frame *current_frame,
		     Closure block,
		     int *pc, VALUE *sp, VALUE *oldsp) {

  CFrame *frame;

  current_frame->pc = pc;
  current_frame->sp = sp;

  frame = (CFrame *) oldsp;
  frame->f.type           = FRAME_C;     
  frame->f.prev_frame     = current_frame;
  frame->block = block;

  return frame;
}


static void
frame_setup_args(VALUE *locals, Code *code, 
		 int argc, VALUE *argv, int **pc) {
  int argc_errorp;
  int num_reqd_args = code->num_args;
  int num_opt_args  = code->num_opt_args;
  int has_rest_argp = (num_opt_args < 0);

  if (has_rest_argp) 
    num_opt_args = -(num_opt_args + 1);

  /* arg count checks */
  argc_errorp = (argc != num_reqd_args);

  if (has_rest_argp) {

    argc_errorp = (argc < num_reqd_args);

  } else if (num_opt_args) {

    int too_few  = argc < num_reqd_args;
    int too_many = argc > num_reqd_args + num_opt_args;
    argc_errorp = too_few || too_many;
    /* adjustment to get correct error message */
    if (too_many) num_reqd_args += num_opt_args; 

  }

  if (argc_errorp)
    rb_raise(rb_eArgError, 
	     "wrong number of arguments(%d for %d)",
	     argc, num_reqd_args);

  /* normal args */
  MEMCPY(locals, argv, VALUE, num_reqd_args);
  argc   -= num_reqd_args;
  argv   += num_reqd_args;
  locals += num_reqd_args;

  /* optional args */
  if (num_opt_args) {
    int n = (argc < num_opt_args) ? argc : num_opt_args;

    MEMCPY(locals, argv, VALUE, n);
    argc   -= n;
    argv   += n;
    locals += num_opt_args;

    *pc += code->opt_args[n];
  } 

  if (has_rest_argp) {
    /* make an array of the rest args */
    if (argc > 0)
      *locals = rb_ary_new4(argc, argv);
    else
      *locals = rb_ary_new2(0);
  }  

}  


static void
frame_reenter(Frame *frame, int **pc, VALUE **sp, VALUE **locals) {
  TRACE(TRC_FRAME, "(re)entering frame %p, env %p, prev env %p\n", 
	       frame, frame->envp, frame->envp->prev_env);
  *pc        = frame->pc;
  *sp        = frame->sp;
  *locals    = &(frame->envp->locals);
}

static void
frame_hbody_enter(Frame * frame, int storage, VALUE *sp) {
  JumpData *jump_data = &frame->jump_data[storage];
  jump_data->jump_type = TAG_NONE;
  jump_data->sp        = sp;
}

static void
frame_enter_handler(Frame *frame, Handler *handler, JumpData *jump_data,
		    int **pc, VALUE **sp, VALUE **locals, int *handler_type) {
  JumpData *jd = &(frame->jump_data[handler->storage]);

  TRACE(TRC_HANDLER,"\t- running handler\n");


  *handler_type = handler->handler_type;

  *pc     = frame->code->bytecode + handler->handler_pc;
  *sp     = jd->sp;
  *locals = &(frame->envp->locals);

  *jd = *jump_data;
}


static void 
frame_leaving(Frame *frame) {
  /* Called when FRAME is about to die */
  frame->envp->frame = NO_FRAME;
}


static Handler *
find_handler_tgt_pc(Frame *frame, int *tgt_pc, int handler_types) {

  Handler *handler;
  int offset_pc, offset_tgt_pc;
  
  handler = frame->code->handlers;

  if (!handler) return (Handler *) 0;

  offset_pc     = frame->pc - frame->code->bytecode;
  offset_tgt_pc = tgt_pc    - frame->code->bytecode;

  for( handler = frame->code->handlers;
       handler->handler_pc;   /* until we hit sentinel */
       handler++ ) {
    
    TRACE(TRC_HANDLER,"\thandler %d -> %d type %x\n",
	  handler->start_pc, handler->end_pc, handler->handler_type);
    
    /* valid handler if current pc is inside and the target pc is
       outside the region covered by the handler */
    if ( (handler->handler_type & handler_types) &&
	 (offset_pc >= handler->start_pc) &&
	 (offset_pc <=  handler->end_pc  ) &&
	 ((offset_tgt_pc <= handler->start_pc) ||
	  (offset_tgt_pc >= handler->end_pc)) ) {
      
      TRACE(TRC_HANDLER,"\t\t- matches - jumping to pc %d\n", handler->handler_pc);
      return handler;
    }
    
    TRACE(TRC_HANDLER,"\t\t- doesn't match\n");
  }

  /* none matched */
  TRACE(TRC_HANDLER,"\t- no match in frame\n");
  return (Handler *) 0;

}


static Handler *
find_handler(Frame *frame, int handler_types) {

  /* FIXME - merge with find_handler_tgt_pc() ? */

  Handler *handler;
  int offset_pc;
  
  handler = frame->code->handlers;

  if (!handler) return (Handler *) 0;

  offset_pc = frame->pc - frame->code->bytecode;

  for( handler = frame->code->handlers;
       handler->handler_pc;   /* until we hit sentinel */
       handler++ ) {
    
    TRACE(TRC_HANDLER,"\thandler %d -> %d type %x\n",
	  handler->start_pc, handler->end_pc, handler->handler_type);
    
    /* valid handler if current pc is inside the region covered 
       by the handler */
    if ( (handler->handler_type & handler_types) &&
	 (offset_pc >= handler->start_pc) &&
	 (offset_pc <=  handler->end_pc  ) ) {
      
      TRACE(TRC_HANDLER,"\t\t- matches - jumping to pc %d\n", handler->handler_pc);
      return handler;
    }
    
    TRACE(TRC_HANDLER,"\t\t- doesn't match\n");
  }

  /* none matched */
  TRACE(TRC_HANDLER,"\t- no match in frame\n");
  return (Handler *) 0;

}



#define CURRENT_PC ((int *) -1)


static Frame *
frame_unroll_stack(Frame *current_frame, 
		   JumpData *jump_data,
		   int *handler_type,
		   int **pc, VALUE **sp, VALUE **locals,
		   struct tag *_tagp) {

  /* 
     Unrolls the call stack starting at CURRENT_FRAME. The unrolling
     will stop when:
     a) TGT_FRAME is hit
        i) if TGT_PC is CURRENT_PC then we reenter this frame and 
	execution continues from the saved PC. Used by return and break.
	ii) otherwise we jump to TGT_PC (which must be a valid pc in 
	TGT_FRAME), running any handlers specified by HANDLER_TYPES on 
	the way. Used by GOTO_E
     b) We hit a C frame
        We call bc_jump_tag(JUMP_TYPE) to longjmp over the C code
     c) We hit a bytecode frame with a handler that is in scope (ie the
        program counter for the frame lies in the range given by
	the handler) and whose handler type matches those specified by
	HANDLER_TYPES.
	We reenter this frame, and reset the program counter so that
	execution continues from the the point given by the matched handler.
	HANDLER_TYPE is set to show the type of handler hit.
	Used by all jump types (to run ensure blocks) and raise/rescue to
	find handling rescue clauses.
  */

  Frame *frame;
  int offset_pc;
  Handler *handler;

  TRACE(TRC_HANDLER,"Start: current %p handler types %x\n",
	current_frame, jump_data->handler_types);

  TRACE(TRC_HANDLER,"Start: target frame %p target pc %d\n",
	jump_data->tgt_frame, 
	jump_data->tgt_pc == CURRENT_PC ? -1 
	                     : jump_data->tgt_pc - jump_data->tgt_frame->code->bytecode);

  /* save away in case handler is in current_frame */
  current_frame->pc = *pc; 
  current_frame->sp = *sp; 

  /* Run back through the frames looking for the
     first with a matching handler */
  for( frame = current_frame;
       ;
       frame = frame->f.prev_frame) {

    TRACE(TRC_HANDLER,"Frame is %p pc %d\n", 
	  frame,
	  (frame->f.type == FRAME_C) ? -1 : 
	  (frame->pc - frame->code->bytecode));

    if (frame == jump_data->tgt_frame) {
      
      if (jump_data->tgt_pc == CURRENT_PC) {
	
	TRACE(TRC_HANDLER,"\t- hit target frame - continue running at current pc\n");
	frame_reenter(frame, pc, sp, locals);
	return frame;

      } else {

	TRACE(TRC_HANDLER,"\t- hit target frame - checking for handlers\n");

	handler = find_handler_tgt_pc(frame, jump_data->tgt_pc, jump_data->handler_types);

	if (handler) {
	  /* found one - visit it */
	  frame_enter_handler( frame, handler, jump_data,
			       pc, sp, locals, handler_type );
	} else {
	  /* couldn't find one - go direct to the tgt pc */
	  TRACE(TRC_HANDLER,"\t- jumping direct to target\n");
	  frame_reenter(frame, pc, sp, locals);
	  *pc = jump_data->tgt_pc;
	}

	return frame;

      }

    } else if (frame->f.type == FRAME_C) {

      /* intermediate C frame */
      TRACE(TRC_HANDLER,"\t- C frame - jumping\n");

      /* pop the current bc_run tag (so we don't just jump back into
         the same invocation of bc_run) and jump to next C-level
         handler */
      thread.current_jump_data = jump_data;
      bc_pop_tag(_tagp);
      bc_jump_tag(jump_data->jump_type);

      /* not reached */

    } else if (!frame->code->handlers) {
      
      /* intermediate bytecode frame without handlers - skip it */
      TRACE(TRC_HANDLER,"\t- no handlers - try next frame\n");
      goto leaving_frame;

    } else {

      /* intermediate bytecode frame with handlers - see if any match */
      offset_pc = frame->pc - frame->code->bytecode;
      
      handler = find_handler(frame, jump_data->handler_types);
      if (handler) {
	/* found one - visit it */
	frame_enter_handler( frame, handler, jump_data,
			     pc, sp, locals, handler_type);
	return frame;
      } 

      /* none of the handlers matched */	
      goto leaving_frame;

    }


  leaving_frame:

    /* leave the bytecode frame - either there were no handlers for
       the frame, or none of them matched. */

    frame_leaving(frame);

    /* and loop to try the next frame */

  }

  /* not reached - we will always hit a C frame at the very least */

}


/** Env *********************************************************/

static Env *
env_from_static_chain(Env *env, int count) {
  /* Returns the environment COUNT frames back up the static chain from
     FRAME */
  for (;;) {
    ASSERT( env != NO_ENV, "static chain exhausted");
    if (count == 0)
      return env;
    count -= 1;
    env = env->prev_env;
  }
}


static Frame *
frame_from_static_chain(Frame *frame, int count) {
  /* Returns the frame COUNT frames back up the static chain from
     FRAME */
  TRACE(TRC_FRAME, "frame_from_static_chain: frame %p, count %d\n",
	frame, count);

  return env_from_static_chain(frame->envp,count)->frame;
}


static int
code_env_size(Code *code) {
  /* Number of bytes to allocate to hold an Env for CODE */
  int num_locals = code->num_locals;
  return sizeof(Env) + sizeof(VALUE) * (num_locals-1);
}


void
env_ensure_on_heap(Env **envp) {

  /* Ensure **ENVP (and all linked environments) are on the heap, not
     the stack. Modify *ENVP to point to the new environment if
     necessary.  */

  Env * env;
  Env * new_env;
  Frame * frame;
  int bytes;

  env = *envp;

  if ((env == NO_ENV) || (env->on_heap_p)) {
    /* end of chain or env already on heap */
    TRACE(TRC_FRAME,"Env %p already on heap\n", env);
    return;
  }

  frame = env->frame;

  bytes = code_env_size(frame->code);
  new_env = (Env *) ALLOC_N(char, bytes);
  MEMCPY(new_env, env, char, bytes);

  *envp = new_env;

  TRACE(TRC_FRAME, "Env %p moving to %p\n", env, new_env);

  frame->envp        = new_env;  
  new_env->on_heap_p = 1;
  env_ensure_on_heap(&new_env->block.env);
  env_ensure_on_heap(&new_env->prev_env);
}

static Closure *
env_get_block(Env *env) {
  return &(env->block);
}

static Closure
closure_new(Code *code, Env *env) {

  Closure closure;
  closure.code = code;
  closure.env  = env;

  return closure;
}

static VALUE
closure_to_proc(Closure *closure, VALUE klass) {
  /* Make a proc from CLOSURE, using KLASS (presumably a sub-class of
     Proc) as the proc's class. Return the new proc. */
  Closure *proc_body;

  env_ensure_on_heap(&closure->env);

  proc_body = ALLOC(Closure);
  *proc_body = *closure;

  TRACE(TRC_FRAME,"proc_from_closure - made proc: code %p, env %p\n", 
	      proc_body->code, proc_body->env);

  /* FIXME gc needed! */
  return Data_Wrap_Struct(klass, 0, 0, proc_body);
}


static void
jump_data_set( int jump_type, int handler_types,
	       Frame *tgt_frame, int *tgt_pc, VALUE return_val ) {
  JumpData *jump_data = &thread.jump_data;
  jump_data->jump_type     = jump_type;
  jump_data->handler_types = handler_types;
  jump_data->tgt_frame     = tgt_frame;
  jump_data->tgt_pc        = tgt_pc;
  jump_data->return_val    = return_val;

}


/******************************************************************
 ** Method finding and creation 
 ******************************************************************/
static NODE* 
bc_find_method( VALUE recv, ID method_id, 
		int argc, VALUE *argv,
		int superp, int private_okp,
		VALUE self) {
  VALUE klass;
  NODE *nd;
  int noex;
  char *msg;
  /*
  FIXME
  Check argument counts for -2 methods (works OK for -1,0,1,..)
  */
  
  klass = CLASS_OF(recv);
  if (superp) klass = RCLASS(klass)->super;

  /* FIXME sort out these last args to rb_get_method_body* */
  nd = rb_get_method_body_in_cache(klass, recv, method_id, 
				   argc, argv, &noex);

  if (!nd) {
    nd = rb_get_method_body( &klass, &method_id, &noex);
    if (!nd) {
      /* FIXME call method_missing */
      if (superp) {
	msg = "super: no superclass method `%s'";
      } else {
	msg = "undefined method `%s' for ""%s"":%s";
      }
      goto error;
    }
  }

  if ((noex & NOEX_PRIVATE) && !private_okp) {
    msg = "private method `%s' called for %s";
    goto error;
  }
  
  /* copied from Ruby's eval.c */
  if ((noex & NOEX_PROTECTED)) {
    VALUE defined_class = klass;
    if (TYPE(defined_class) == T_ICLASS)
      defined_class = RBASIC(defined_class)->klass;
    if (!rb_obj_is_kind_of(self, defined_class)) {
      msg = "protected method `%s' called for %s";
      goto error;
    }
  }
  
  return nd;

 error:
  rb_raise(rb_eNoMethodError, msg,  //NameError, msg,
	   rb_id2name(method_id), 
	   bc_inspect(recv),
	   rb_class2name(klass));
}


void
bc_define_method(VALUE klass, ID name, Code *method) {
  NODE *node;

  /* FIXME private, protected, public etc */
  node = rb_node_newnode( NODE_BFUNC, 
			  (VALUE) method, 
			  (VALUE) method->num_args, 
			  (VALUE) 0); 
  rb_add_method( klass, name, node, NOEX_PUBLIC);
}


int
bc_obj_is_proc(obj) {
  return rb_obj_is_kind_of(obj, rb_cProc);
}

/******************************************************************
 ** Run bytecode
 ******************************************************************/
static VALUE 
bc_run(Frame *current_frame) {
  VALUE *locals, *sp;
  int *pc;
  int i;

  JumpData *jump_data;
  
  struct tag _tag;    

  frame_reenter(current_frame, &pc, &sp, &locals);

  /* Set up protection to catch exceptions, breaks, returns etc 
     These can be raised by C methods we call, or by our own helper
     functions (eg bc_find_method).
     We must be very careful to remove this protection whenever we
     leave this function, or we'll segv. This is done by the various calls 
     to bc_pop_tag().
   */
  {
    int jump_type;
   
    bc_push_tag(&_tag);
    jump_type = setjmp(_tag.buf);
    
    TRACE(TRC_HANDLER,"Setjmp jump type %s:\n", jump_type_names[jump_type]);

    if (jump_type) {
      jump_data = thread.current_jump_data;
      goto jump_handlers;
    }

  }


  /* Actually run the bytecode */
  TRACE(TRC_RUN,"About to run .. sp: %p, code: %p, num_locals: %d\n", 
	    sp, 
	    current_frame->code,
	    current_frame->code->num_locals);

  for(;;) {

    DEBUG {
      VALUE *bos, *s;
      bos = frame_stack_start(current_frame);
      for (s = bos; s < sp; s++) {
	TRACE(TRC_STACK,"stack %d\t%p\t%x\n", s - bos, s, (int) *s);
	/* FIXME stack elements as strings would be nice
	   but bc_call isn't prepared for the bc_inspect C call if
	   inspect is implemented in bytecode */
	//printf("stack %d\t%s\n", s - bos, bc_inspect(*s));
      }
    }

    TRACE(TRC_RUN,"pc: %d\t", pc - current_frame->code->bytecode);


    ASSERT(sp >= frame_stack_start(current_frame),
	   "stack underflow");

    switch (*pc) {
    case LD_IMM:                         
      {
	VALUE val = (VALUE) pc[1];
	TRACE(TRC_RUN,"LD_IMM %s\n", bc_inspect(val));               
        PUSH_STACK( val );
        pc += 2;                         
        break;                           
      }                                  
    case LD_LOC:
      {
	int local_id = pc[1];
	TRACE(TRC_RUN,"LD_LOC\t%d\n", local_id);               
	ASSERT(local_id < current_frame->code->num_locals,
	       "LD_LOC local_id too large");
        PUSH_STACK( locals[local_id] );
        pc += 2;                         
        break;                           
      }                                  
    case LD_LOC_L:
      {
	int local_id = pc[1];
	int count    = pc[2];

	Env *env = env_from_static_chain(current_frame->envp, count);
	VALUE *chain_locals = &(env->locals); 

	TRACE(TRC_RUN,"LD_LOC_L\t%d\t%d\n", local_id, count); 
	//	ASSERT(local_id < frame->code->num_locals,
	//       "LD_LOC_L local_id too large");
        PUSH_STACK( chain_locals[local_id] );
        pc += 3;                         
        break;                           
      }                                  
    case ST_LOC:                         
      {
	int local_id = pc[1];
	TRACE(TRC_RUN,"ST_LOC\t%d\n", local_id);
	ASSERT(local_id < current_frame->code->num_locals,
	       "ST_LOC local_id too large");
	locals[local_id] = POP_STACK();
        pc += 2;                         
        break;                           
      } 
    case ST_LOC_L:
      {
	int local_id = pc[1];
	int count    = pc[2];

	Env *env = env_from_static_chain(current_frame->envp, count);
	VALUE *chain_locals = &(env->locals); 

	TRACE(TRC_RUN,"ST_LOC_L\t%d\t%d\n", local_id, count);               
	//	ASSERT(local_id < frame->code->num_locals,
	//       "ST_LOC_L local_id too large");
        chain_locals[local_id] = POP_STACK();
        pc += 3;                         
        break;                           
      }                                  
    case DUP:                            
      {
        TRACE(TRC_RUN,"DUP\n");                  
        PUSH_STACK( sp[-1] );
        pc += 1;
        break;
      }
    case POP:
      {
        TRACE(TRC_RUN,"POP\n");
	(void) POP_STACK();   /* (void) to avoid warnings */
        pc += 1;
        break;
      }
    case SWAP:
      {
        VALUE tmp;
        TRACE(TRC_RUN,"SWAP\n");
	tmp    = sp[-1];
	sp[-1] = sp[-2];
	sp[-2] = tmp;
        pc += 1;
        break;
      }
    case RUN:
      {
	Code *code = (Code *) pc[1];
	VALUE recv = POP_STACK();

        TRACE(TRC_RUN,"RUN <code>\n");
	pc += 2;

	current_frame = frame_alloc_bc_on_bc(current_frame, 
					     recv,
					     closure_new(code, NO_ENV),
					     NO_BLOCK,
					     &pc, &sp, sp, &locals);

	break;
      }
    case CALL:
      {
        int method_id        = pc[1];
        int argc             = pc[2];
	int superp           = pc[3];
	int private_okp      = pc[4];
	int ampersand_argp   = pc[5];
	Code *code_for_block = (Code *) pc[6];

	VALUE recv, *argv, *oldsp;
	NODE *nd;
	Closure block;

        TRACE(TRC_RUN, "CALL %s %d %s %s (%s) <%s>\n", 
	       rb_id2name(method_id), argc, 
	       superp         ? "super" : "normal",
	       private_okp    ? "private" : "public",
 	       ampersand_argp ? "ampersand arg" : "no ampersand_arg",
	       code_for_block ? "block" : "no block");
        pc   += 7;

	oldsp = sp;

	/* -- normal and scatter args -- */
	if (argc >= 0) {
	  /* normal args only */
	  sp  -= argc + 1;
	  recv =  *sp;
	  argv =  sp + 1;
	} else {
	  /* normal + scatter args - glue normal args to array */
	  VALUE ary;
	  int num_args, ary_len;

	  ary = sp[-1];
	  if (TYPE(ary) != T_ARRAY) {  /* try to convert to array */
	    /* FIXME use splat function */
	    if (NIL_P(ary))
	      ary =  rb_ary_new3(1, Qnil);
	    else
	      ary = rb_Array(ary);
	  }

	  ary_len  = RARRAY(ary)->len;
	  num_args = -(argc + 1);
	  sp     -= num_args + 2;  /* +1 recv, +1 scatter ary */
	  recv     = *sp;
	  argc     = num_args + ary_len;
	  argv     = ALLOC_N(VALUE, argc); /* FIXME when does this get freed? */
	  MEMCPY(argv,            sp+1,            VALUE, num_args);
	  MEMCPY(argv + num_args, RARRAY(ary)->ptr, VALUE, ary_len);
	}

	/* -- blocks and ampersand args -- */
	if (ampersand_argp) {
	  VALUE proc = POP_STACK();
	  Closure *block_ptr;
	  
	  if (proc == Qnil) {
	    /* no block passed */
	    block = closure_new(0, 0);
	  } else {
	    /* convert to proc if necessary */
	    if (!bc_obj_is_proc(proc)) {
	      VALUE b = rb_check_convert_type(proc, T_DATA, "Proc", "to_proc");
	      if (!bc_obj_is_proc(b)) {
		rb_raise(rb_eTypeError, 
			 "wrong argument type %s (expected Proc)",
			 rb_obj_classname(proc));
	      }
	      proc = b;
	    }

	    /* get hold of the closure */
	    Data_Get_Struct(proc, Closure, block_ptr);
	    block = *block_ptr;
	  }
	} else {
	  block = closure_new(code_for_block, 
			      current_frame->envp);
	}

	/* -- find method to call -- */
	nd = bc_find_method(recv, method_id, 
			    argc, argv, 
			    superp, private_okp,
			    current_frame->envp->self);

	/* -- and call it -- */
	switch nd_type(nd) {
	case NODE_CFUNC: { 
	  VALUE retval;

	  CFrame *new_frame = frame_alloc_c_on_bc( current_frame,
						   block,
						   pc, sp, oldsp );

	  thread.current_frame = new_frame;

	  retval = call_cfunc(nd->nd_cfnc, recv, 
			      nd->nd_argc, argc, argv);
	  PUSH_STACK(retval);

	  /* in case our environment has been moved off the stack */
	  locals = &(current_frame->envp->locals);

	  break;
	}
	case NODE_BFUNC: {
	  /* bytecode method - need to interpret it */
	  Code *code = (Code *) nd->nd_cfnc;
	  Frame *new_frame;

	  new_frame = frame_alloc_bc_on_bc(
				current_frame, 
				recv,
				closure_new(code, NO_ENV),
				block,
				&pc, &sp, oldsp, &locals);

	  DEBUG {
            for (i=0; i < code->num_locals; i++)
	      locals[i] = Qnil;
          }

	  frame_setup_args(locals, code, argc, argv, &pc);

	  DEBUG {
            for (i=0; i < code->num_locals; i++)
	      TRACE(TRC_STACK, "local %d\t%s\n", i, bc_inspect(locals[i]));
	  }

	  current_frame = new_frame;
	  break;
	}
	default:
	  rb_bug("Can't call functions that aren't C or bytecode");
	  break;
	}

        break;
      }
    case RETURN:
      {
	int count = pc[1];
	Frame *tgt_frame;

	TRACE(TRC_RUN,"RETURN %d\n", count);

	/* move on to next instruction so that we don't get caught by
	   an incorrect ensure */
	pc += 1;

	tgt_frame = frame_from_static_chain(current_frame, count)->f.prev_frame;
	jump_data_set( TAG_RETURN, HANDLER_TYPE_ENSURE,
		       tgt_frame, CURRENT_PC, POP_STACK());

	jump_data = &thread.jump_data;

	goto jump_handlers;
      }
    case BREAK:
      {
        Frame *tgt_frame;

	TRACE(TRC_RUN,"BREAK\n");

	/* move on to next instruction so that we don't get caught by
	   an incorrect ensure */
	pc += 1;
	
	/* Jump back to the lexical enclosing code if there is one */
	tgt_frame = frame_from_static_chain(current_frame, 1);
	if (!tgt_frame) {
	  /* otherwise return to caller - we must be in a proc
	     whose static chain is no longer on the call stack */
	  tgt_frame = current_frame->f.prev_frame;
	} 

	jump_data_set( TAG_BREAK, HANDLER_TYPE_ENSURE,
		       tgt_frame, CURRENT_PC, POP_STACK());

	jump_data = &thread.jump_data;

	goto jump_handlers;
      }
    case GOTO:
      {
        int *target = (int *) pc[1];
	TRACE(TRC_RUN,"GOTO %d\n", HUMAN_PC(target));
        pc = target; 
        break;
      }
    case GOTO_E:
      {
        int *target = (int *) pc[1];
        TRACE(TRC_RUN,"GOTO_E %d\n", HUMAN_PC(target));

	jump_data_set( TAG_GOTO_E, HANDLER_TYPE_ENSURE,
		       current_frame, target, Qnil);	

	jump_data = &thread.jump_data;

	goto jump_handlers;
      }
    case IF:
      {
        int *target = (int *) pc[1]; 
        TRACE(TRC_RUN,"IF %d\n", HUMAN_PC(target));
        if ( RTEST(POP_STACK()) ) {
          pc = target;
        } else {
          pc += 2;
        }
        break;
      }
    case IF_NOT:
      {
        int *target = (int *) pc[1];
        TRACE(TRC_RUN,"IF_NOT %x\n", target);
        if ( !RTEST(POP_STACK()) ) {
          pc = target;
        } else {
          pc += 2;
        }
        break;
      }
    case YIELD:
      {
        int argc  = pc[1];  
	int count = pc[2];
	Frame *new_frame;
	Env *block_owner_env;
	Closure *block;

	VALUE *argv, *oldsp;

        TRACE(TRC_RUN,"YIELD %d\n", argc);

        pc += 3;

	block_owner_env = env_from_static_chain(current_frame->envp, count);

	block = env_get_block(block_owner_env);

	if (!block->code) {
	  rb_raise(rb_eLocalJumpError, "no block given");
	}

	oldsp = sp;
	sp  -= argc; 
	argv = sp;   

	new_frame = frame_alloc_bc_on_bc(current_frame, 
					 block->env->self,
					 *block,
					 NO_BLOCK,
					 &pc, &sp, oldsp, &locals);

	for (i=0; i < argc; i++)
	  locals[i] = argv[i];
	  
	current_frame = new_frame;
	break;
      }
    case DEFN:
      {
        ID           name = pc[1];
        Code *method = (Code *) pc[2];
	VALUE klass;

        TRACE(TRC_RUN,"DEFN %s\n", rb_id2name(name));

        klass = POP_STACK();
        Check_Type(klass, T_CLASS);
	bc_define_method(klass, name, method);

        pc += 3;
        break;
      }
    case ARY_SCATTER:
      {
        int num_elems = pc[1];
        int flags     = pc[2];
        VALUE val;
        int gather, push_value;
	int i = 0, len;

	/* check is used to implement arg count checking for procs */
        int check = current_frame->ary_scatter_checkp;


        pc += 3;

        gather     = (flags >> 1) & 1;
        push_value = (flags >> 0) & 1;

	/* reset so blocks in the proc don't get arg checks */
	current_frame->ary_scatter_checkp = 0;

        TRACE(TRC_RUN,"ARY_SCATTER %d %d %d\n", num_elems, gather, push_value);

	/* This code is adapted from massign in Ruby's eval.c */
        val = POP_STACK();

        if (TYPE(val) != T_ARRAY) {
	  if (rb_respond_to(val, to_ary)) {
	    VALUE ary = rb_funcall(val, to_ary, 0);
	    if (TYPE(ary) != T_ARRAY) {
	      rb_raise(rb_eTypeError, "%s#to_ary should return Array",
		       rb_class2name(CLASS_OF(val)));
	    }
	    val = ary;
	  } else {
	    val = rb_ary_new3(1, val);
	  }
	}

	if (push_value) PUSH_STACK(val);

	len = RARRAY(val)->len;

	if (check) {
	  int errorp = gather ? (len < num_elems) : (len != num_elems);
	  if (errorp) 
	    rb_raise( rb_eArgError, "wrong number of arguments (%d for %d)", 
		      len, num_elems);
	}

	for (i=0; num_elems && i<len; i++, num_elems--) {
	  PUSH_STACK( RARRAY(val)->ptr[i] );
	}

	while (num_elems--) PUSH_STACK(Qnil);

	if (gather) {
	  if (i<len) {
	    PUSH_STACK( rb_ary_new4(len-i, RARRAY(val)->ptr+i) );
	  } else {
	    PUSH_STACK( rb_ary_new2(0) );
	  }
	} 

	break;
      }
    case LD_IVAR:
      {
        ID var_id = (ID) pc[1];	

	TRACE(TRC_RUN,"LD_IVAR %s\n", rb_id2name(var_id));

	PUSH_STACK(rb_ivar_get(current_frame->envp->self, var_id));

	pc += 2;
	break;
      }
    case ST_IVAR:
      {
        ID var_id = (ID) pc[1];	

	TRACE(TRC_RUN,"ST_IVAR %s \n", rb_id2name(var_id));

	rb_ivar_set(current_frame->envp->self, var_id, POP_STACK());

	pc += 2;
	break;
      }
   case LD_CVAR:
      {
        ID var_id = (ID) pc[1];	
        VALUE klass = CLASS_OF( current_frame->envp->self);

	TRACE(TRC_RUN,"LD_CVAR %s class=%s\n", rb_id2name(var_id),  rb_class2name(klass));
        
	PUSH_STACK(rb_cvar_get(klass, var_id));

	pc += 2;
	break;
      }
    case ST_CVAR:
      {
        ID var_id = (ID) pc[1];	
        VALUE klass = CLASS_OF( current_frame->envp->self);

	TRACE(TRC_RUN,"ST_CVAR %s class=%s\n", rb_id2name(var_id), rb_class2name(klass));

	rb_cvar_set(klass, var_id, POP_STACK(), Qfalse);

	pc += 2;
	break;
      }
    case DEF_CVAR:
      {
        ID var_id = (ID) pc[1];	
	VALUE klass;
	klass = POP_STACK();
        Check_Type(klass, T_CLASS);

	TRACE(TRC_RUN,"DEF_CVAR %s class=%s\n", rb_id2name(var_id), rb_class2name(klass) );
	
	rb_cvar_set(klass, var_id, POP_STACK(), Qtrue);

	pc += 2;
	break;
      }
    case LD_GVAR:
      {
        ID var_id = (ID) pc[1];	

	TRACE(TRC_RUN,"LD_GVAR %s\n", rb_id2name(var_id));

	/* FIXME avoid overhead of rb_global_entry each time */
	PUSH_STACK(rb_gvar_get(rb_global_entry(var_id)));

	pc += 2;
	break;
      }
    case ST_GVAR:
      {
        ID var_id = (ID) pc[1];	

	TRACE(TRC_RUN,"ST_GVAR %s\n", rb_id2name(var_id));

	/* FIXME avoid overhead of rb_global_entry each time */
	rb_gvar_set(rb_global_entry(var_id), POP_STACK());

	pc += 2;
	break;
      }
    case DEFS:
      {
        ID           name = pc[1];
        Code *method = (Code *) pc[2];
	VALUE obj;

        TRACE(TRC_RUN,"DEFS %s\n", rb_id2name(name));

        obj = POP_STACK();
	bc_define_method(rb_singleton_class(obj), name, method);

        pc += 3;
        break;
      }
    case MAKE_PROC:
      {
	Closure *block = &current_frame->envp->block;

        TRACE(TRC_RUN,"MAKE_PROC\n");

	if (block->code) {
	  PUSH_STACK(closure_to_proc(block, rb_cProc));
	} else {
	  PUSH_STACK(Qnil);
	}

	pc += 1;
	break;
      }
    case LD_SELF:
      {
	TRACE(TRC_RUN,"LD_SELF\n");
	PUSH_STACK(current_frame->envp->self);
	pc += 1;
	break;
      }
    case JUMP_REHANDLE:
      {
	int handler_type;
	int jump_type;
	int storage = pc[1];

	TRACE(TRC_RUN, "JUMP_REHANDLE %d\n", storage);

	jump_data = &current_frame->jump_data[storage]; 

	/* move on to next instruction so that we don't get caught by
	   an incorrect ensure */
	pc += 2;

      jump_handlers:

	jump_type = jump_data->jump_type;

	TRACE(TRC_HANDLER, "Jump_Rehandle jump type: %s\n", 
	                   jump_type_names[jump_type]);

	if (jump_type == TAG_NONE) {
	  TRACE(TRC_HANDLER, "- nothing to do\n");
	  break;
	}

	current_frame = frame_unroll_stack( current_frame, 
					    jump_data,
					    &handler_type,
					    &pc, &sp, &locals,
					    &_tag);

	switch (jump_type) {
	case TAG_RETURN:
	case TAG_BREAK:
	  {
	    VALUE return_val = jump_data->return_val;

	    if (current_frame->f.type == FRAME_C) {
	      /* This C frame must be the target frame of a return, or 
		 we'd have jumped over it. We never break to a C frame. */
	      TRACE(TRC_HANDLER,"returning to C frame\n");
	      thread.current_frame = (CFrame*) current_frame; 
	      bc_pop_tag(&_tag);

	      return return_val;

	    } else if (current_frame == jump_data->tgt_frame) {

	      TRACE(TRC_HANDLER,"rejoining target frame\n");

	      PUSH_STACK(return_val);

	    } else {
	      TRACE(TRC_HANDLER,"running ensure\n");
	    }

	    break;
	  }
	case TAG_RAISE:
	  {
	    if (handler_type == HANDLER_TYPE_RESCUE) {
	      PUSH_STACK(bc_current_exception()); 
	    }
	    break;
	  }
	case TAG_THROW:
	case TAG_GOTO_E:
	  {
	    break;
	  }
	default:
	  {
	    ASSERT(0, "unknown jump_type");
	    /* not reached */
	    break;
	  }
	}

	break;
      }
    case HBODY_ENTER:
      {
	int storage = pc[1];
        TRACE(TRC_RUN,"HBODY_ENTER %d\n", storage);

	frame_hbody_enter(current_frame, storage, sp);

	pc += 2;
	break;
      }
      
    default:
      {
	TRACE(TRC_RUN, "** unknown bytecode **\n");
	ASSERT(0, "unknown bytecode");
	/* not reached */
      }
    }
  }
}


/******************************************************************
 ** Compilation
 ******************************************************************/
static Handler *
bc_compile_handlers(VALUE handlers) {
  VALUE handler_ary, handler;
  int i, size;
  Handler *result;

  handler_ary = rb_iv_get(handlers, "@handlers");
  size        = RARRAY(handler_ary)->len;

  if (!size) return 0;   /* no handlers */

  result = ALLOC_N(Handler, size + 1);  /* +1 for sentinel handler at end */

  for ( i=0; i<size; i++ ) {
    handler = rb_ary_entry(handler_ary, (long) i);
    result[i].start_pc     = FIX2INT(rb_iv_get(handler, "@start_pc"));
    result[i].end_pc       = FIX2INT(rb_iv_get(handler, "@end_pc"));
    result[i].handler_pc   = FIX2INT(rb_iv_get(handler, "@handler_pc"));
    result[i].handler_type = FIX2INT(rb_iv_get(handler, "@handler_type"));
    result[i].storage      = FIX2INT(rb_iv_get(handler, "@storage"));
  }

  result[i].handler_pc = 0;  /* set sentinel value */

  return result;
}


/* forward decl for recursive functions */
static Code *
bc_compile_code(VALUE writer);


static int *
bc_compile_bytecode(VALUE writer) {
  
  VALUE bc_ary, val;
  int i, size ;
  int *bc;

  /* Ensure writer has compiled itself */
  rb_funcall(writer, rb_intern("compile"), 0);

  bc_ary = rb_iv_get(writer, "@result");
  size   = RARRAY(bc_ary)->len;

  TRACE(TRC_COMPILE,"Size is %d\n", size);
  bc  = ALLOC_N(int, size);
  
  TRACE(TRC_COMPILE,"Bytecode is at: %x\n",bc);
  
  for (i=0; i < size; i++) {
    val = rb_ary_entry(bc_ary, (long) i);
    TRACE(TRC_COMPILE,"Entry %d: %s\n", i, 
	  rb_obj_is_kind_of(val, cWriter) ? "Writer" : bc_inspect(val));

    if (FIXNUM_P(val)) {
      
      bc[i] = FIX2INT(val);
      
    } else if (SYMBOL_P(val)) {
      
      bc[i] = SYM2ID(val);

    } else if (rb_obj_is_kind_of(val, cJumpTarget)) {

      TRACE(TRC_COMPILE, "JumpTarget %d\t%p\n",
	                 FIX2INT(rb_iv_get(val, "@target")),
			 (bc + FIX2INT(rb_iv_get(val, "@target")))); 
      bc[i] = (int) (bc + FIX2INT(rb_iv_get(val, "@target"))); 

    } else if (rb_obj_is_kind_of(val, cWriter)) {
      
      bc[i] = (int) bc_compile_code(val);

    } else if ((val == Qfalse) || (val == Qnil)) {

      bc[i] = 0;

    } else if (val == Qtrue) {

      bc[i] = 1;

    } else {

      /* Otherwise val is an immediate value wrapped in an array
      Unwrap it */
      bc[i] = (int) rb_ary_entry(val, (long) 0);

    }
  }  

  /* FIXME plenty more validity checks could go here! */
  if (bc[size-2] != RETURN) { /* -2 to skip over parameter to RETURN */
    rb_raise(rb_eStandardError, "bytecode doesn't end in RETURN");
  }

  return bc;

}


static void
bc_compile_args(VALUE writer, Code *code) {

  VALUE opt_args_ary, val;
  int i, num_offsets;
  
  code->num_locals   = FIX2INT(rb_iv_get(writer, "@num_locals"));
  code->num_args     = FIX2INT(rb_iv_get(writer, "@num_args"));
  
  /* Copy @opt_args to C array, if @opt_args isnt nil.
     Element N>0 is the PC offset to jump to to skip
     initialisation for the first N-1 args */
  opt_args_ary = rb_iv_get(writer, "@opt_args_jump_points");
  if (opt_args_ary == Qnil) {
    
    code->num_opt_args = 0;
    code->opt_args     = (unsigned char*) 0;
    
  } else {
    
    num_offsets = RARRAY(opt_args_ary)->len;
    code->num_opt_args = num_offsets - 1;
    code->opt_args     = ALLOC_N(unsigned char, num_offsets);
    
    for (i=0; i < num_offsets; i++) {
      val = rb_ary_entry(opt_args_ary, (long) i);
      code->opt_args[i] = FIX2INT(val);
    }
    
  }
  
  if (RTEST(rb_iv_get(writer, "@rest_arg")))
    code->num_opt_args = -(code->num_opt_args + 1);
}


static Code *
bc_compile_code(VALUE writer) {
  Code *code = ALLOC(Code);

  code->bytecode = bc_compile_bytecode(writer);
  code->bytecode_len = RARRAY(rb_iv_get(writer, "@result"))->len;

  code->num_jump_data = FIX2INT(rb_iv_get(writer, "@num_jump_data"));

  code->handlers = bc_compile_handlers(rb_iv_get(writer, "@handlers"));

  bc_compile_args(writer, code);
  
  return code;
}

/******************************************************************
 ** Functions implementing MRI <-> BCR interface
 ******************************************************************/
static int
bc_block_given_p() {

  CFrame *cframe;

  if (!thread.on_bc_stack) {
    return 0; 
  }

  TRACE(TRC_C,"bc_block_given_p\n");

  cframe = thread.current_frame;

  if (cframe->f.type != FRAME_C) {
    rb_warning("bc_block_given_p but frame not C");
    return 0;
  }
  
  if (!cframe->block.code) {
    return 0;
  }

  return 1;
}


static VALUE
bc_yield( VALUE val ) {

  CFrame *cframe;
  Frame *frame;
  Closure block;

  if (!thread.on_bc_stack) {
    return Qundef; 
  }

  cframe = thread.current_frame;

  TRACE(TRC_C,"bc_yield: cframe is %p\n", cframe);

  if (cframe->f.type != FRAME_C) {
      rb_warning("bc_yield but frame not C");
      return Qundef;
  }
  
  block = cframe->block;

  if (!block.code) {
    TRACE(TRC_C,"bc_yield: no block given\n");
    rb_raise(rb_eLocalJumpError, "no block given");
  }

  frame = frame_alloc_bc_on_c( cframe,
			       block.env->self,
			       block,
			       NO_BLOCK);

  /* set the block's argument */
  frame->envp->locals = val;

  return bc_run(frame);
}  


static VALUE
bc_call(Code *code, VALUE recv, 
	int argc_top_level, VALUE *argv) {
  CFrame *cframe;
  Frame *frame;

  TRACE(TRC_C,"method call\n");

  cframe = thread.current_frame;

  ASSERT(cframe->f.type == FRAME_C, "bc_call but frame not C");

  frame = frame_alloc_bc_on_c( cframe, 
			       recv,
			       closure_new(code, NO_ENV),
			       NO_BLOCK);

  /* FIXME what about gather and optional args? */
  MEMCPY(&(frame->envp->locals), argv, VALUE, argc_top_level);

  return bc_run(frame);
}


VALUE
bc_stop(VALUE val) {
  TRACE(TRC_C,"leaving BCR\n");
  thread.on_bc_stack = 0;
  return Qnil;
}


static void
bc_raise(VALUE exc) {
  if (!thread.on_bc_stack) return;

  thread.current_jump_data = &thread.jump_data;
  jump_data_set( TAG_RAISE,
		 HANDLER_TYPE_RESCUE | HANDLER_TYPE_ENSURE,
		 NO_FRAME,
		 CURRENT_PC,
		 Qnil);
}


static void
bc_throw(ID tag, VALUE value) {
  if (!thread.on_bc_stack) return;

  thread.current_jump_data = &thread.jump_data;
  jump_data_set( TAG_THROW,
		 HANDLER_TYPE_ENSURE,
		 NO_FRAME,
		 CURRENT_PC,
		 Qnil);
}


extern VALUE ruby_top_self; /* from object.c */

static VALUE
bc_run_from_writer(VALUE runner, VALUE writer) {
  Code *code;
  CFrame *cframe;
  Frame *frame;
  VALUE recv    = ruby_top_self; 

  cframe = (CFrame *) thread.frame_stack;
  cframe->f.type = FRAME_C;

  thread.current_frame = cframe;

  code = bc_compile_code(writer);

  frame = frame_alloc_bc_on_c( cframe, 
			       recv,
			       closure_new(code, NO_ENV),
			       NO_BLOCK);

  TRACE(TRC_C,"entering BCR\n");
  thread.on_bc_stack = 1;
  thread.jump_data.jump_type = TAG_NONE;
  return rb_ensure(bc_run, (VALUE) frame, bc_stop, Qnil);

}

/******************************************************************
 ** Procs
 ******************************************************************/
static VALUE
bc_proc_new(VALUE klass) {
  Closure *closure = &(thread.current_frame->block);

  /* FIXME - garbage collection? */
  
  if (!thread.on_bc_stack) return Qundef; 

  ASSERT(thread.current_frame->f.type == FRAME_C,
	 "proc_new but frame not C");
  
  if (!closure->code) 
    rb_raise(rb_eArgError, "tried to create Proc object without a block");

  return closure_to_proc(closure, klass);
}


static VALUE
bc_proc_call( VALUE proc, VALUE args ) {

  Closure *closure;
  CFrame *cframe;
  Frame *frame;

  if (!thread.on_bc_stack) return Qundef;  

  cframe = thread.current_frame;

  TRACE(TRC_FRAME,"proc_call: cframe is %p\n", cframe);

  ASSERT(cframe->f.type == FRAME_C, "proc_call but frame not C");

  Data_Get_Struct(proc, Closure, closure);

  frame = frame_alloc_bc_on_c( cframe,
			       closure->env->self,			       
			       *closure,
  			       NO_BLOCK);

  /* force proc-style arg checking */
  frame->ary_scatter_checkp = 1;

  if (RARRAY(args)->len == 1) {
    frame->envp->locals = rb_ary_entry(args, (long) 0);
  } else {
    frame->envp->locals = args; 
  }

  return bc_run(frame);
}

/******************************************************************
 ** Garbage collection
 ******************************************************************/
static void
bc_gc() {

  /* mark thread
     mark stack */

  TRACE(TRC_GC,"start\n");

  if (thread.current_frame) {
    rb_gc_mark_locations( (VALUE *) thread.frame_stack,
			  (VALUE *) (thread.current_frame + 1));
  }

  rb_gc_mark(thread.jump_data.return_val);

}


static void
bc_gc_mark_block( void *obj ) {
  Code *code = (Code *) obj;
  VALUE *bytecode      = (VALUE *) code->bytecode;

  TRACE(TRC_GC,"mark_block %p\n", obj);

  rb_gc_mark_locations( bytecode, bytecode + code->bytecode_len);

  /* FIXME need to free bytecode, opt_args and handlers when Code
     is freed. How to tell when Code is freed? */

}


/******************************************************************
 ** Constant and class helper functions
 ******************************************************************/
static VALUE
bc_bcr_const_get( VALUE owner_object, 
		  VALUE name_as_sym ) {

  ID name_as_id = SYM2ID(name_as_sym);
  VALUE owner_klass;

  if ( (TYPE(owner_object) == T_CLASS ) || 
       (TYPE(owner_object) == T_MODULE)    ) {
    owner_klass = owner_object;
  } else {
    owner_klass = rb_obj_class(owner_object);
  }

  return rb_const_get(owner_klass, name_as_id);
  
}


static VALUE
bc_get_or_make_class( VALUE owner_object, 
		      VALUE name_as_sym, 
		      VALUE zsuper         ) {

  VALUE klass = 0;
  VALUE owner_klass;
  ID name_as_id = SYM2ID(name_as_sym);

  /* 
     In TOPLEVEL_BINDING, class can be run with owner_object 
     as the 'main' object; in nested classes, owner_object will
     be the surrounding class. In either case, get the class to
     create the object under.
  */
  if ( (TYPE(owner_object) == T_CLASS ) || 
       (TYPE(owner_object) == T_MODULE)    ) {
    owner_klass = owner_object;
  } else {
    owner_klass = rb_obj_class(owner_object);
  }

  if (rb_const_defined_at(owner_klass, name_as_id)) {
    klass = rb_const_get(owner_klass, name_as_id);
  }

  if (klass) {
    if (TYPE(klass) != T_CLASS) {
      rb_raise(rb_eTypeError, "%s is not a class",
	       rb_id2name(name_as_id));
    }
    /*
    if (zsuper) {
      tmp = rb_class_real(RCLASS(klass)->super);
      if (tmp != super) {
	goto override_class;
      }
    }
    */
    if (rb_safe_level() >= 4) {
      rb_raise(rb_eSecurityError, "extending class prohibited");
    }
    rb_clear_cache();
  }
  else {
    //  override_class:
    if (NIL_P(zsuper)) zsuper = rb_cObject;
    klass = rb_define_class_under(owner_klass, 
				  rb_id2name(name_as_id),
				  zsuper );

  }

  return klass;
  
}



/******************************************************************
 ** Initialisation
 ******************************************************************/
void Init_bcr_runtime() {

  thread.frame_stack = ALLOC_N(char, 10000);

  mBytecode = rb_define_module("Bytecode");

  cRunner   = rb_define_class_under(mBytecode, "Runner", rb_cObject);

  bcr_hooks.call          = (BCRCall) &bc_call;
  bcr_hooks.yield         = &bc_yield;
  bcr_hooks.block_given_p = &bc_block_given_p;
  bcr_hooks.proc_new      = &bc_proc_new;
  bcr_hooks.proc_call     = &bc_proc_call;
  bcr_hooks.gc            = &bc_gc;
  bcr_hooks.gc_mark_block = &bc_gc_mark_block;
  bcr_hooks.raise         = &bc_raise;
  bcr_hooks.throw         = &bc_throw;

  rb_define_singleton_method( cRunner, "run_from_writer", 
			      bc_run_from_writer, 1);

  /* methods for Writer & JumpTarget are added in Ruby code */
  cWriter   = rb_define_class_under(mBytecode, "Writer", rb_cObject);

  cJumpTarget = rb_define_class_under(cWriter, "JumpTarget", rb_cObject);
  
  /* ruby.h doesn't make this available */
  rb_eLocalJumpError = rb_const_get(rb_cObject, 
				    rb_intern("LocalJumpError"));

  /* BCR helper methods on Object */
  rb_define_method( rb_cObject, "get_or_make_class",
		    bc_get_or_make_class, 2);

  rb_define_method( rb_cObject, "bcr_const_get",
		    bc_bcr_const_get, 1);

  to_ary = rb_intern("to_ary");
}

