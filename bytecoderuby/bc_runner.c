#include "ruby.h"
#include "node.h"

#include "bc_runner.h"
#include "ruby_adds.h"

#include <setjmp.h>

/* useful Ruby objects, populated in Init_Runner */
VALUE rb_eLocalJumpError, rb_eArgumentError;
VALUE mBytecode;
VALUE cRunner;
VALUE cWriter;
ID to_ary;

/* debugging */
#define DEBUG_FLAG 0
#define DEBUG if (DEBUG_FLAG)
#define TRACE if (DEBUG_FLAG) printf

/*********** Key structures ********************/ 
typedef struct handler {
  /* Handlers for exceptions etc 
     All values are offsets from start of the bytecode */
  int start_pc;         /* start of region handled */
  int end_pc;           /* end of region handled */
  int handler_pc;      /* location to jump to if match */
} Handler;


typedef struct codeblock {
  /* the bytecode itself */
  int *bytecode;

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

} CodeBlock;


typedef struct closure {
  CodeBlock *codeblock;
  struct frame *env;
} Closure;


/* random constants to distinguish between bytecode and C frames */
#define FRAME_BYTECODE 0x1345234c
#define FRAME_C        0x98a767f1


typedef struct cframe {
  int type;

  /* the previous frame in the call stack */
  struct frame *prev_frame;

  /* the Ruby block associated with this method call */
  Closure block;

} CFrame;


typedef struct frame { 
  CFrame f;

  /* the code being run in this frame */
  CodeBlock *codeblock;

  /* the value of self for this frame */
  VALUE self;  

  /* the previous frame in the static chain */
  struct frame *static_chain_prev_frame;

  /* the pc to restart when execution returns to this frame */
  int   *pc;

  /* the stack entry to restart at when execution returns to this frame */
  VALUE *sp;

  /* local variables (these run on beyond the end of the 
     frame struct) */
  VALUE locals; 

  /* value stack runs on after the locals */

} Frame;


/* a thread - there's only one of these at the moment */
struct thread {

  /* storage for the thread's call frames */
  char *frame_stack;

  /* the current C frame - only valid if we're in the course 
     of a call to a C method */
  CFrame *current_frame;

  /* flag to show whether bytecode is being run */
  int on_bc_stack;

} thread;



/* value stack manipulation */

#define PUSH_STACK(val) *(sp++) = (val)

#define POP_STACK() (*(--sp))


/******************************************************************
 ** Method finding and creation 
 ******************************************************************/
static NODE* 
bc_find_method( VALUE klass, ID method_id, int num_args, 
		VALUE *argv, VALUE recv) {
  NODE *nd;
  
  /*
  FIXME
  Check argument counts for -2 methods (works OK for -1,0,1,..)
  */
  
  /* FIXME sort out these last args to rb_get_method_body* */
  nd = rb_get_method_body_in_cache(klass, recv, method_id, num_args, argv, 1);

  if (!nd) {
    nd = rb_get_method_body( &klass, &method_id, 0);
    if (!nd) {
      /* FIXME call method_missing */
      rb_raise(rb_eNameError, "undefined method `%s' for `%s'",
	       rb_id2name(method_id), rb_class2name(klass));
    }
  }

  return nd;
}


void
bc_define_method(VALUE klass, ID name, CodeBlock *method) {
  NODE *node;

  /* FIXME private, protected, public etc */
  node = rb_node_newnode(NODE_BFUNC, method, method->num_args, 0); 
  rb_add_method( klass, name, node, NOEX_PUBLIC);
}


/******************************************************************
 ** Stack manipulation
 ******************************************************************/

/* returns the bottom of the stack for frame FRAME */
static VALUE *
frame_stack_start(Frame *frame) {
  int num_locals = frame->codeblock->num_locals;
  int sz         = sizeof(Frame) + sizeof(VALUE) * (num_locals-1);

  return (VALUE *) ( ((char *) (frame)) + sz);
}

/*
  Frame maintenance
  Will need revisiting when frames are potentially heap-allocated
  (because captured in a closure).
 */
// 
static Frame *
frame_alloc_bc_on_bc( Frame *current_frame, CodeBlock *codeblock, 
		      CodeBlock *block,     Frame *block_env,
		      Frame *static_chain_prev_frame, VALUE recv,
		      int **pc, VALUE **sp, VALUE *oldsp, VALUE **locals) {

  Frame *new_frame;

  current_frame->pc = *pc;
  current_frame->sp = *sp;

  new_frame = (Frame *) oldsp;
  new_frame->f.type                  = FRAME_BYTECODE;
  new_frame->f.prev_frame            = current_frame;
  new_frame->codeblock               = codeblock;
  new_frame->f.block.codeblock       = block;      
  new_frame->f.block.env             = block_env;
  new_frame->static_chain_prev_frame = static_chain_prev_frame;
  new_frame->self                    = recv;

  *pc        = codeblock->bytecode;
  *sp        = frame_stack_start(new_frame);
  *locals    = &(new_frame->locals);

  return new_frame;
}


static void
frame_reenter(Frame *frame, int **pc, VALUE **sp, VALUE **locals) {
  *pc        = frame->pc;
  *sp        = frame->sp;
  *locals    = &(frame->locals);
}


static Frame *
frame_alloc_bc_on_c( CFrame *current_frame, 
		     CodeBlock *codeblock, VALUE recv,
		     CodeBlock *block,     Frame *block_env,
		     Frame *static_chain_prev_frame ) {

  Frame  *frame  = (Frame *) (current_frame + 1); 

  frame->f.type                  = FRAME_BYTECODE;
  frame->f.prev_frame            = (Frame *) current_frame;
  frame->codeblock               = codeblock;
  frame->f.block.codeblock       = block;
  frame->f.block.env             = block_env;
  frame->static_chain_prev_frame = static_chain_prev_frame;
  frame->self                    = recv;

  frame->pc           = codeblock->bytecode;
  frame->sp           = frame_stack_start(frame);

  return frame;
}

 
static CFrame *
frame_alloc_c_on_bc( Frame *current_frame,
		     CodeBlock *block, Frame *block_env,
		     VALUE *oldsp) {

  CFrame *new_frame = (CFrame *) oldsp;
  new_frame->type            = FRAME_C;     
  new_frame->prev_frame      = current_frame;
  new_frame->block.codeblock = block;      
  new_frame->block.env       = block_env;
  
  return new_frame;
}


static void
frame_setup_args(VALUE *locals, CodeBlock *codeblock, 
		 int argc, VALUE *argv, int **pc) {
  int argc_errorp;
  int num_reqd_args = codeblock->num_args;
  int num_opt_args  = codeblock->num_opt_args;
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
    rb_raise(rb_eArgumentError, 
	     "wrong # of arguments(%d for %d)",
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

    *pc += codeblock->opt_args[n];
  } 

  if (has_rest_argp) {
    /* make an array of the rest args */
    if (argc > 0)
      *locals = rb_ary_new4(argc, argv);
    else
      *locals = rb_ary_new2(0);
  }  

}  


static Frame *
frame_from_static_chain(Frame *frame, int count) {
  /* Returns the frame COUNT frames back up the static chain from
     FRAME */
  for (;;) {
    /* FIXME - proper exception (or rb_bug?) */
    if (frame == (Frame *) 0)
      rb_raise(rb_eStandardError, "static chain exhausted");
    if (count == 0)
      return frame;
    count -= 1;
    frame = frame->static_chain_prev_frame;
  }
}


static Frame *
frame_find_handler(Frame *current_frame,
		   int **pc, VALUE **sp, VALUE **locals) {
  /* Find the handler nearest to CURRENT_FRAME, and return its 
     frame and the PC at which to start execution. Also set LOCALS
     correctly for the frame found. Returns 0 if no frame with
     the required handler */

  Frame *frame;
  int offset_pc;
  Handler *handler;

  /* save away in case handler is in current_frame */
  current_frame->pc = *pc; 
  current_frame->sp = *sp; 

  /* Run back through the frames looking for the
     first with a matching handler */
  for( frame = current_frame;
       frame->f.type == FRAME_BYTECODE;
       frame = frame->f.prev_frame) {

    TRACE("Handle: frame %x pc %d\n", 
	  frame,
	  frame->pc - frame->codeblock->bytecode);

    if (!frame->codeblock->handlers) {
      /* no exception handlers for this frame */
      TRACE("Handle: no handlers\n");
      continue;
    }

    offset_pc = frame->pc - frame->codeblock->bytecode;
	  
    for( handler = frame->codeblock->handlers;
	 handler->handler_pc;   /* until we hit sentinel */
	 handler++ ) {

      TRACE("Handle: handler %d -> %d ",
	    handler->start_pc, handler->end_pc);

      if ( (offset_pc >= handler->start_pc) &&
	   (offset_pc <= handler->end_pc  ) ) {

	TRACE("MATCH - jumping to pc %d\n", handler->handler_pc);
	*pc     = frame->codeblock->bytecode + handler->handler_pc;
	*locals = &(frame->locals);
	*sp     = frame->sp;
	return frame;
      }

      TRACE("NO MATCH\n");
    }
  }

  TRACE("Handle: no handler found\n");
  return (Frame *) 0;
}


static char* inspect(VALUE s) {
  return STR2CSTR(rb_funcall(s, rb_intern("inspect"), 0));
}


/******************************************************************
 ** Run bytecode
 ******************************************************************/
static VALUE 
bc_run(Frame *current_frame) {
  VALUE *locals, *sp;
  int *pc;
  int i;

  frame_reenter(current_frame, &pc, &sp, &locals);

  TRACE("About to run .. sp: %x\n", sp);

  for(;;) {

    DEBUG {
      VALUE *bos, *s;
      bos = frame_stack_start(current_frame);
      if (sp < bos) printf("!!!! STACK UNDERFLOW !!!!\n");
      for (s = bos; s < sp; s++) {
	//printf("stack %d\t%x\t%x\n", s - bos, s, *s);
	/* FIXME stack elements as strings would be nice
	   but bc_call isn't prepared for the inspect C call if
	   inspect is implemented in bytecode */
	printf("stack %d\t%s\n", s - bos, inspect(*s));
      }
    }

  bytecode_dispatch:
    TRACE ("pc: %d\t", pc - current_frame->codeblock->bytecode);

    switch (*pc) {
    case LD_IMM:                         
      {
	VALUE val = (VALUE) pc[1];
	TRACE("LD_IMM %s\n", inspect(val));               
        PUSH_STACK( val );
        pc += 2;                         
        break;                           
      }                                  
    case LD_LOC:
      {
	int local_id = pc[1];
	TRACE("LD_LOC\t%d\n", local_id);               
        PUSH_STACK( locals[local_id] );
        pc += 2;                         
        break;                           
      }                                  
    case LD_LOC_L:
      {
	int local_id = pc[1];
	int count    = pc[2];
	Frame *frame = frame_from_static_chain(current_frame, count);
	VALUE *chain_locals = &(frame->locals);
	TRACE("LD_LOC_L\t%d\t%d\n", local_id, count); 
        PUSH_STACK( chain_locals[local_id] );
        pc += 3;                         
        break;                           
      }                                  
    case ST_LOC:                         
      {
	int local_id = pc[1];
	TRACE("ST_LOC\t%d\n", local_id);
	locals[local_id] = POP_STACK();
        pc += 2;                         
        break;                           
      } 
    case ST_LOC_L:
      {
	int local_id = pc[1];
	int count    = pc[2];
	Frame *frame = frame_from_static_chain(current_frame, count);
	VALUE *chain_locals = &(frame->locals);
	TRACE("ST_LOC_L\t%d\t%d\n", local_id, count);               
        chain_locals[local_id] = POP_STACK();
        pc += 3;                         
        break;                           
      }                                  
    case DUP:                            
      {
        TRACE("DUP\n");                  
        PUSH_STACK( sp[-1] );
        pc += 1;
        break;
      }
    case POP:
      {
        TRACE("POP\n");
        POP_STACK();
        pc += 1;
        break;
      }
    case SWAP:
      {
        VALUE tmp;
        TRACE("SWAP\n");
	tmp     = sp[-1];
	sp[-1] = sp[-2];
	sp[-2] = tmp;
        pc += 1;
        break;
      }
    case RUN:
      {
	CodeBlock *codeblock = (CodeBlock *) pc[1];
	VALUE recv = POP_STACK();

        TRACE("RUN <code>\n");
	pc += 2;

	current_frame = frame_alloc_bc_on_bc(current_frame, codeblock, 
					     0, 0,
					     0, recv,
					     &pc, &sp, sp, &locals);

	break;
      }
    case CALL:
      {
        int method_id = pc[1];
        int argc      = pc[2];
	int superp    = pc[3];
	CodeBlock *block = (CodeBlock *) pc[4];

	VALUE recv, *argv, result, *oldsp;
	NODE *nd;

        TRACE( "CALL %s %d %s\n", 
	       rb_id2name(method_id), argc, 
	       superp ? "super" : "normal");
        pc   += 5;

	oldsp = sp;

	if (argc >= 0) {
	  /* normal args only */
	  sp  -= argc + 1;
	  recv =  *sp;
	  argv =  sp + 1;
	} else {
	  /* normal + scatter args - glue normal args to array */
	  VALUE ary;
	  int num_args, ary_len;
	  ary      = sp[-1];
	  ary_len  = RARRAY(ary)->len;
	  num_args = -(argc + 1);
	  sp     -= num_args + 2;  /* +1 recv, +1 scatter ary */
	  recv     = *sp;
	  argc     = num_args + ary_len;
	  argv     = ALLOC_N(VALUE, argc);
	  MEMCPY(argv,            sp+1,            VALUE, num_args);
	  MEMCPY(argv + num_args, RARRAY(ary)->ptr, VALUE, ary_len);
	}

	{
	  VALUE klass = CLASS_OF(recv);
	  if (superp) klass = RCLASS(klass)->super;
	  nd = bc_find_method(klass, method_id, argc, argv, recv);
	}

	switch nd_type(nd) {
	case NODE_CFUNC: { 
	  VALUE retval;
	  struct tag _tag;
	  int state;
	  Frame *frame;

	  CFrame *new_frame = frame_alloc_c_on_bc( current_frame,
						   block,
						   current_frame,
						   oldsp );
	  thread.current_frame = new_frame;

	  /* protect C call with MRI tags so that we catch exceptions */
	  /* FIXME there are more efficient ways to do this than 
	     setting up tags & jump bufs for every C call */
	  bc_push_tag(&_tag);
	  state = setjmp(_tag.buf);
	  if (state == 0) {
	    retval = call_cfunc(nd->nd_cfnc, recv, 
				nd->nd_argc, argc, argv);
	    PUSH_STACK(retval);
	  }
	  bc_pop_tag(&_tag);
	  
	  /* no exception */
	  if (!state) break;

	  /* FIXME this code currently assumes exceptions only -
	     could also be throws, breaks etc */

	  /* try and find a handler for the exception */
	  frame = frame_find_handler(current_frame,
				     &pc, &sp, &locals);
	  
	  if (frame) {
	    PUSH_STACK(bc_current_exception());
	    current_frame = frame;
	    goto bytecode_dispatch;
	  } else {
	    TRACE("reraising C exception\n");
	    bc_jump_tag(state);
	  }
	  
	  /* not reached */
	}
	case NODE_BFUNC: {
	  /* bytecode method - need to interpret it */
	  CodeBlock *codeblock = (CodeBlock *) nd->nd_cfnc;
	  Frame *new_frame;

	  new_frame = frame_alloc_bc_on_bc(current_frame, codeblock, 
					   block, current_frame,
					   0, recv,
					   &pc, &sp, oldsp, &locals);

	  DEBUG {
            for (i=0; i < codeblock->num_locals; i++)
	      locals[i] = Qnil;
          }

	  frame_setup_args(locals, codeblock, argc, argv, &pc);

	  DEBUG {
            for (i=0; i < codeblock->num_locals; i++)
	      printf("local %d\t%s\n", i, inspect(locals[i]));
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
	Frame * frame;
	VALUE return_val;

	TRACE("RETURN %d\n", count);

	frame = frame_from_static_chain(current_frame, count)->f.prev_frame;

	/* were we called from C? */
	if (frame->f.type == FRAME_C) {
	  thread.current_frame = (CFrame *) frame;
	  return POP_STACK();
	}

	/* return to previous frame */
	return_val = sp[-1]; 
	current_frame = frame;
	frame_reenter(current_frame, &pc, &sp, &locals);
	PUSH_STACK(return_val);

	break;
      }
    case GOTO:
      {
        int offset = pc[1];
        TRACE("GOTO %d\n", offset);
        pc += offset;
        break;
      }
    case IF:
      {
        int offset = pc[1];
        TRACE("IF %d\n", offset);
        if ( RTEST(POP_STACK()) ) {
          pc += offset;
        } else {
          pc += 2;
        }
        break;
      }
    case IF_NOT:
      {
        int offset = pc[1];
        TRACE("IF_NOT %d\n", offset);
        if ( !RTEST(POP_STACK()) ) {
          pc += offset;
        } else {
          pc += 2;
        }
        break;
      }
    case YIELD:
      {
	CodeBlock *codeblock;
        int argc  = pc[1];  
	int count = pc[2];
	Frame *new_frame, *block_owner_frame, *static_chain_prev_frame;

	VALUE *argv, *oldsp;
	NODE *nd;

        TRACE("YIELD %d\n", argc);

        pc += 3;

	block_owner_frame = frame_from_static_chain(current_frame, count);

	codeblock               = block_owner_frame->f.block.codeblock;
	static_chain_prev_frame = block_owner_frame->f.block.env;

	if (!codeblock) {
	  rb_raise(rb_eLocalJumpError, "no block given");
	}

	oldsp = sp;
	sp  -= argc; 
	argv = sp;   

	new_frame = frame_alloc_bc_on_bc(current_frame, codeblock, 
					 0, 0,
					 static_chain_prev_frame,
					 static_chain_prev_frame->self,
					 &pc, &sp, oldsp, &locals);

	for (i=0; i < argc; i++)
	  locals[i] = argv[i];
	  
	current_frame = new_frame;
	break;
      }
    case DEFN:
      {
        ID           name = pc[1];
        CodeBlock *method = (CodeBlock *) pc[2];
	VALUE klass;

        TRACE("DEFN %s\n", rb_id2name(name));

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
        int check = 0;   /* for future use! - not tested yet */
	int i = 0, len;

        pc += 3;

        gather     = (flags >> 1) & 1;
        push_value = (flags >> 0) & 1;

        TRACE("ARY_SCATTER %d %d %d\n", num_elems, gather, push_value);

	/* This code is lifted from massign in Ruby's eval.c */
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
	for (i=0; num_elems && i<len; i++, num_elems--) {
	  PUSH_STACK( RARRAY(val)->ptr[i] );
	}

	if (check && num_elems) goto arg_error;

	while (num_elems--) PUSH_STACK(Qnil);

	if (gather) {
	  if (i<len) {
	    PUSH_STACK( rb_ary_new4(len-i, RARRAY(val)->ptr+i) );
	  } else {
	    PUSH_STACK( rb_ary_new2(0) );
	  }
	} else if (check && i < len) {
	  goto arg_error;
	}

	break;
	
      arg_error:
	i += num_elems;
	rb_raise(rb_eArgError, "wrong # of arguments (%d for %d)", len, i);

        break;
      }
    case BREAK:
      {
	VALUE return_val;
        TRACE("BREAK\n");
	
	/* Jump back to the lexical enclosing code */
	/* FIXME run protect blocks */
	/* FIXME check static_chain_prev_frame is on the call stack - needed for procs */
	/* FIXME doesn't unroll C stack in [1,2].each {|a| break} */
	return_val = sp[-1]; 

	current_frame = current_frame->static_chain_prev_frame;	

	/* FIXME correct message? */
	if (!current_frame)
	  rb_raise(rb_eLocalJumpError, "break called out of block");

	frame_reenter(current_frame, &pc, &sp, &locals);

	PUSH_STACK(return_val);
        break;
      }
    case LD_IVAR:
      {
        ID var_id = (ID) pc[1];	

	TRACE("LD_IVAR %s\n", rb_id2name(var_id));

	PUSH_STACK(rb_ivar_get(current_frame->self, var_id));

	pc += 2;
	break;
      }
    case ST_IVAR:
      {
        ID var_id = (ID) pc[1];	

	TRACE("ST_IVAR %s\n", rb_id2name(var_id));

	rb_ivar_set(current_frame->self, var_id, POP_STACK());

	pc += 2;
	break;
      }
    case LD_GVAR:
      {
        ID var_id = (ID) pc[1];	

	TRACE("LD_GVAR %s\n", rb_id2name(var_id));

	rb_bug("ld_gvar not yet implemented");
	/*	PUSH_STACK(rb_gvar_get(var_id)); */

	pc += 2;
	break;
      }
    case DEFS:
      {
        ID           name = pc[1];
        CodeBlock *method = (CodeBlock *) pc[2];
	VALUE obj;

        TRACE("DEFS %s\n", rb_id2name(name));

        obj = POP_STACK();
	bc_define_method(rb_singleton_class(obj), name, method);

        pc += 3;
        break;
      }
    case LD_SELF:
      {
	TRACE("LD_SELF\n");
	PUSH_STACK(current_frame->self);
	pc += 1;
	break;
      }
    case REHANDLE:
      {
	VALUE exception;
	Frame *frame;

	TRACE("REHANDLE\n");

	exception = POP_STACK();

	frame = frame_find_handler(current_frame,
				   &pc, &sp, &locals);

	if (frame) {

	  PUSH_STACK(exception);
	  current_frame = frame;
	  goto bytecode_dispatch;

	} else {

	  TRACE("rehandle: reraising C exception\n");
	  rb_exc_raise(exception);
	}

	/* not reached */

	break;
      }
    default:
      {
	rb_raise(rb_eStandardError, "unknown bytecode");
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
    result[i].start_pc   = FIX2INT(rb_iv_get(handler, "@start_pc"));
    result[i].end_pc     = FIX2INT(rb_iv_get(handler, "@end_pc"));
    result[i].handler_pc = FIX2INT(rb_iv_get(handler, "@handler_pc"));
  }

  result[i].handler_pc = 0;  /* set sentinel value */

  return result;
}


/* forward decl for recursive functions */
static CodeBlock *
bc_compile_codeblock(VALUE writer);


static int *
bc_compile_bytecode(VALUE writer) {
  
  VALUE bc_ary, val;
  int i, size ;
  int *bc;

  /* Ensure writer has compiled itself */
  rb_funcall(writer, rb_intern("compile"), 0);

  bc_ary = rb_iv_get(writer, "@result");
  size   = RARRAY(bc_ary)->len;

  //  TRACE("Size is %d\n", size);
  bc  = ALLOC_N(int, size);
  
  //  TRACE("Bytecode is at: %x\n",bc);
  
  for (i=0; i < size; i++) {
    //  TRACE("Entry %d: ", i);
    val = rb_ary_entry(bc_ary, (long) i);

    if (FIXNUM_P(val)) {
      
      //  TRACE("fixnum %d\n", FIX2INT(val));
      bc[i] = FIX2INT(val);
      
    } else if (SYMBOL_P(val)) {
      
      //  TRACE("symbol %d\n", val);
      bc[i] = SYM2ID(val);

    } else if (rb_obj_is_kind_of(val, cWriter)) {

      //  TRACE("Writer\n"); 
      bc[i] = (int) bc_compile_codeblock(val);

    } else {

      /* Otherwise val is an immediate value wrapped in an array
      Unwrap it */
      //  TRACE("Unwrapping immediate\n");
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
bc_compile_args(VALUE writer, CodeBlock *codeblock) {

  VALUE opt_args_ary, val;
  int i, num_offsets;
  
  codeblock->num_locals   = FIX2INT(rb_iv_get(writer, "@num_locals"));
  codeblock->num_args     = FIX2INT(rb_iv_get(writer, "@num_args"));
  
  /* Copy @opt_args to C array, if @opt_args isnt nil.
     Element N>0 is the PC offset to jump to to skip
     initialisation for the first N-1 args */
  opt_args_ary = rb_iv_get(writer, "@opt_args_jump_points");
  if (opt_args_ary == Qnil) {
    
    codeblock->num_opt_args = 0;
    codeblock->opt_args     = (unsigned char*) 0;
    
  } else {
    
    num_offsets = RARRAY(opt_args_ary)->len;
    codeblock->num_opt_args = num_offsets - 1;
    codeblock->opt_args     = ALLOC_N(unsigned char, num_offsets);
    
    for (i=0; i < num_offsets; i++) {
      val = rb_ary_entry(opt_args_ary, (long) i);
      codeblock->opt_args[i] = FIX2INT(val);
    }
    
  }
  
  if (RTEST(rb_iv_get(writer, "@rest_arg")))
    codeblock->num_opt_args = -(codeblock->num_opt_args + 1);
}


static CodeBlock *
bc_compile_codeblock(VALUE writer) {
  CodeBlock *codeblock = ALLOC(CodeBlock);

  codeblock->bytecode = bc_compile_bytecode(writer);
  codeblock->handlers = bc_compile_handlers(rb_iv_get(writer, "@handlers"));

  bc_compile_args(writer, codeblock);
  
  return codeblock;
}


static VALUE
bc_define_bytecode_method0(VALUE klass, VALUE name_as_sym, VALUE writer)
{
  CodeBlock *cb = bc_compile_codeblock(writer);
  NODE *node    = rb_node_newnode(NODE_BFUNC, cb, cb->num_args, 0); 

  rb_add_method( klass, 
                 SYM2ID(name_as_sym),
		 node,
		 NOEX_PUBLIC);

  return Qnil;
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

  TRACE("CCC block_given_p\n");

  cframe = thread.current_frame;

  if (cframe->type != FRAME_C) {
    rb_bug("bc_block_given_p but frame not C");
  }
  
  if (!cframe->block.codeblock) {
    return 0;
  }

  return 1;
}


static VALUE
bc_yield( VALUE val ) {

  CFrame *cframe;
  Frame *frame;
  CodeBlock *codeblock;
  Frame *static_chain_prev_frame;

  if (!thread.on_bc_stack) {
    return Qundef; 
  }

  TRACE("CCC yield\n");

  cframe = thread.current_frame;

  if (cframe->type != FRAME_C) {
      rb_bug("bc_yield but frame not C");
  }
  
  if (!cframe->block.codeblock) {
    rb_raise(rb_eLocalJumpError, "no block given");
  }

  codeblock               = cframe->block.codeblock;
  static_chain_prev_frame = cframe->block.env;

  frame = frame_alloc_bc_on_c( cframe,
			       codeblock, 
			       static_chain_prev_frame->self,
			       0, 0,
			       static_chain_prev_frame);  

  frame->locals                  = val;

  return bc_run(frame);
}  


static VALUE
bc_call(CodeBlock *codeblock, VALUE recv, 
	int argc_top_level, VALUE *argv) {
  CFrame *cframe;
  Frame *frame;

  TRACE("CCC method call\n");

  cframe = thread.current_frame;

  if (cframe->type != FRAME_C) {
      rb_bug("bc_call but frame not C");
  }

  frame = frame_alloc_bc_on_c(cframe, codeblock, recv, 0, 0, 0);  

  MEMCPY(&(frame->locals), argv, VALUE, argc_top_level);

  return bc_run(frame);
}


VALUE
bc_stop(VALUE val) {
  TRACE("CCC leaving bc\n");
  thread.on_bc_stack = 0;
  return Qnil;
}


extern VALUE ruby_top_self; /* from object.c */

static VALUE
bc_run_from_writer(VALUE runner, VALUE writer) {
  CodeBlock *codeblock;
  CFrame *cframe;
  Frame *frame;
  VALUE recv    = ruby_top_self; 

  cframe = (CFrame *) thread.frame_stack;
  cframe->type = FRAME_C;

  thread.current_frame = cframe;

  codeblock = bc_compile_codeblock(writer);

  frame = frame_alloc_bc_on_c(cframe, codeblock, recv, 0, 0, 0);  

  TRACE("CCC entering bc\n");
  thread.on_bc_stack = 1;
  return rb_ensure(bc_run, (VALUE) frame, bc_stop, Qnil);

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
  override_class:
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
void Init_Runner() { 

  thread.frame_stack          = ALLOC_N(char, 10000);

  mBytecode = rb_define_module("Bytecode");

  cRunner   = rb_define_class_under(mBytecode, "Runner", rb_cObject);

  bcr_hooks.call          = (BCRCall) &bc_call;
  bcr_hooks.yield         = &bc_yield;
  bcr_hooks.block_given_p = &bc_block_given_p;

  rb_define_singleton_method( cRunner, "run_from_writer", 
			      bc_run_from_writer, 1);

  /* methods for Writer are actually added in Ruby code */
  cWriter   = rb_define_class_under(mBytecode, "Writer", rb_cObject);
  
  rb_eLocalJumpError = rb_const_get(rb_cObject, 
				    rb_intern("LocalJumpError"));

  rb_eArgumentError = rb_const_get(rb_cObject, 
				   rb_intern("ArgumentError"));

  rb_define_method( rb_cClass, "define_bytecode_method0", 
  		    bc_define_bytecode_method0, 2);

  /* new class defintion method */
  rb_define_method( rb_cObject, "get_or_make_class",
		    bc_get_or_make_class, 2);

  rb_define_method( rb_cObject, "bcr_const_get",
		    bc_bcr_const_get, 1);

  to_ary = rb_intern("to_ary");
}

