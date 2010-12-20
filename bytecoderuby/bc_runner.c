#include "ruby.h"
#include "node.h"

#include "bc_runner.h"
#include "ruby_adds.h"

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


typedef struct cframe {
  int type;

  /* the previous frame in the call stack */
  struct frame *prev_frame;

  /* the Ruby block associated with this method call */
  Closure block;

} CFrame;


/* FIXME move stack into frame? */
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

  /* local variables (these run on beyond the end of the 
     frame struct) */
  VALUE locals; 

} Frame;

#define FRAME_BYTECODE 1
#define FRAME_C        0

/* frame stack */
char *frame_stack;

struct thread {
  CFrame *current_frame;
  VALUE *tos;
  int on_bc_stack;
} thread;

/* The size of a frame (in bytes) that can hold num_locals locals. 
   -1 for the initial local declared in the Frame */
#define FRAME_SIZE(num_locals) (sizeof(Frame) + sizeof(VALUE) * ((num_locals) -1)) 


/* value stack */
VALUE *stack;

#define PUSH_STACK(val) *(tos++) = (val)

#define POP_STACK() (*(--tos))


/* FIXME - inline this for handy performance boost */
static NODE* 
bc_find_method( ID method_id, VALUE recv, int num_args, 
		VALUE *argv) {
  VALUE klass;
  NODE *nd;
  
  /*
  FIXME
  Check argument counts for -2 methods (works OK for -1,0,1,..)
  */
  
  klass = CLASS_OF(recv);

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




/*
  Frame maintenance
  Will need revisiting when frames are potentially heap-allocated
  (because captured in a closure).
 */
static Frame *
frame_alloc( Frame *current_frame, CodeBlock *codeblock, 
	     int **pc, VALUE **locals) {

  Frame *new_frame;
  int sz;

  current_frame->pc = *pc;

  sz         = FRAME_SIZE(current_frame->codeblock->num_locals);
  new_frame  = (Frame *) (((char *) current_frame) + sz);
  new_frame->codeblock    = codeblock;
  new_frame->f.prev_frame = current_frame;
  new_frame->f.type       = FRAME_BYTECODE;

  *locals    = &(new_frame->locals);
  *pc        = codeblock->bytecode;

  return new_frame;
}

void
frame_reenter(Frame *frame, int **pc, VALUE **locals) {
  *locals    = &(frame->locals);
  *pc        = frame->pc;
}

void
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

    argc_errorp = (argc < num_reqd_args) ||
                  (argc > num_reqd_args + num_opt_args);
    /* hack to get the correct error message */
    if (argc_errorp && (argc > num_reqd_args + num_opt_args))
      num_reqd_args += num_opt_args; 

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
		   int **pc, VALUE **locals) {
  /* Find the handler nearest to CURRENT_FRAME, and return its 
     frame and the PC at which to start execution. Also set LOCALS
     correctly for the frame found. Returns 0 if no frame with
     the required handler */

  Frame *frame;
  int offset_pc;
  Handler *handler;

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


static VALUE 
bc_run(Frame *current_frame) {
  VALUE *locals, *tos = thread.tos;
  int *pc;
  int i;

  frame_reenter(current_frame, &pc, &locals);

  TRACE("About to run ..\n");

  for(;;) {

    DEBUG {
      /* print stack */
      VALUE *s;
      if (tos < stack) printf("!!!! STACK UNDERFLOW !!!!\n");
      for (s = stack; s < tos; s++) {
	printf("stack %d\t%s\n", s -stack, inspect(*s));
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
        PUSH_STACK( tos[-1] );
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
	tmp     = tos[-1];
	tos[-1] = tos[-2];
	tos[-2] = tmp;
        pc += 1;
        break;
      }
    case CALL:
      {
        int method_id = pc[1];
        int argc      = pc[2]; 
	CodeBlock *block = (CodeBlock *) pc[3];

	VALUE recv, *argv, result;
	NODE *nd;

        TRACE("CALL %s %d\n", rb_id2name(method_id), argc);
        pc   += 4;

	if (argc >= 0) {
	  /* normal args only */
	  tos  -= argc + 1;
	  recv =  *tos;
	  argv =  tos + 1;
	} else {
	  /* normal + scatter args - glue normal args to array */
	  VALUE ary;
	  int num_args, ary_len;
	  ary      = tos[-1];
	  ary_len  = RARRAY(ary)->len;
	  num_args = -(argc + 1);
	  tos     -= num_args + 2;  /* +1 recv, +1 scatter ary */
	  recv     = *tos;
	  argc     = num_args + ary_len;
	  argv     = ALLOC_N(VALUE, argc);
	  MEMCPY(argv,            tos+1,            VALUE, num_args);
	  MEMCPY(argv + num_args, RARRAY(ary)->ptr, VALUE, ary_len);
	}

	nd = bc_find_method(method_id, recv, argc, argv);

	switch nd_type(nd) {
	case NODE_CFUNC: { 
	  CFrame *new_frame;
	  int sz;

	  sz         = FRAME_SIZE(current_frame->codeblock->num_locals);
	  new_frame  = (CFrame *) (((char *) current_frame) + sz);
	  new_frame->block.codeblock = block;      
	  new_frame->block.env       = current_frame;
	  new_frame->prev_frame      = current_frame;
	  new_frame->type            = FRAME_C;

	  thread.tos           = tos;

	  thread.current_frame = new_frame;
	  PUSH_STACK(call_cfunc(nd->nd_cfnc, recv, 
				nd->nd_argc, argc, argv));
	  break;  
	}
	case NODE_BFUNC: {
	  /* bytecode method - need to interpret it */
	  CodeBlock *codeblock = (CodeBlock *) nd->nd_cfnc;
	  Frame *new_frame;

	  new_frame = frame_alloc(current_frame, codeblock, &pc, &locals);
	  new_frame->f.block.codeblock       = block;      
	  new_frame->f.block.env             = current_frame;
	  new_frame->static_chain_prev_frame = 0;
	  new_frame->self                    = recv;

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
	  /*
	    Don't know how to deal with any others .. pass back to eval.c 
	    The method'll be in the cache so won't be too expensive to look up
	    FIXME not implemented
	  */
	  /* rb_call( klass, recv, method_id, argc, argv, 1); */
	  return Qnil;
	  break;
	}

        break;
      }
    case RETURN:
      {
	int count = pc[1];
	Frame * frame;

	TRACE("RETURN %d\n", count);

	frame = frame_from_static_chain(current_frame, count)->f.prev_frame;

	/* were we called from C? */
	if (frame->f.type == FRAME_C) {
	  thread.current_frame = (CFrame *) frame;
	  return POP_STACK();
	}

	/* return to previous frame - result should be on top 
	   of the stack - just where we need it */
	current_frame = frame;
	frame_reenter(current_frame, &pc, &locals);

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

	VALUE *argv;
	NODE *nd;

        TRACE("YIELD %d\n", argc);

        pc += 3;

	block_owner_frame = frame_from_static_chain(current_frame, count);

	codeblock               = block_owner_frame->f.block.codeblock;
	static_chain_prev_frame = block_owner_frame->f.block.env;

	if (!codeblock) {
	  rb_raise(rb_eLocalJumpError, "no block given");
	}

	new_frame = frame_alloc(current_frame, codeblock, &pc, &locals);

	/* blocks don't have blocks themselves */
	new_frame->f.block.codeblock       = 0; 
	new_frame->f.block.env             = 0;
	new_frame->static_chain_prev_frame = static_chain_prev_frame;
	new_frame->self                    = static_chain_prev_frame->self;

	tos  -= argc;
	argv = tos;
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
        TRACE("BREAK\n");
	
	/* Jump back to the lexical enclosing code */
	/* FIXME run protect blocks */
	/* FIXME check static_chain_prev_frame is on the call stack - needed for procs */
	current_frame = current_frame->static_chain_prev_frame;	

	/* FIXME correct message? */
	if (!current_frame)
	  rb_raise(rb_eLocalJumpError, "break called out of block");

	frame_reenter(current_frame, &pc, &locals);

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
    case ST_SELF:
      {
	/* FIXME - only needed to allow our current crappy class decl to work */
	TRACE("ST_SELF\n");
	current_frame->self = POP_STACK();
	pc += 1;
	break;
      }
    case RAISE:
      {
	VALUE excep_class;
	Frame *frame;

	TRACE("RAISE\n");

	excep_class = POP_STACK();
        Check_Type(excep_class, T_CLASS);	

	current_frame->pc = pc;
	frame = frame_find_handler(current_frame,
				   &pc, &locals);

	if (frame) {

	  PUSH_STACK(rb_exc_new2(excep_class, inspect(excep_class)));
	  // FIXME tos    = 
	  // FIXME set current exception?
	  current_frame = frame;
	  goto bytecode_dispatch;

	} else {

	  // FIXME set current exception?
	  TRACE("raise: raising C exception %s\n", inspect(excep_class));
	  rb_raise(excep_class, inspect(excep_class));  /* no messages allowed */
	}

      }
    case REHANDLE:
      {
	VALUE exception;
	Frame *frame;

	TRACE("REHANDLE\n");

	exception = POP_STACK();

	current_frame->pc = pc;
	frame = frame_find_handler(current_frame,
				   &pc, &locals);

	if (frame) {

	  PUSH_STACK(exception);
	  // FIXME tos    = 
	  // FIXME set current exception?
	  current_frame = frame;
	  goto bytecode_dispatch;

	} else {

	  // FIXME set current exception?
	  TRACE("rehandle: reraising C exception %s\n", inspect(exception));
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


static Handler *
bc_compile_handlers(VALUE handlers) {
  VALUE handler_ary, handler;
  int i, size;
  Handler *result;

  handler_ary = rb_iv_get(handlers, "@handlers");
  size        = RARRAY(handler_ary)->len;

  if (!size) return 0;

  result = ALLOC_N(Handler, size + 1);  /* +1 for sentinel handler at end */

  for ( i=0; i<size; i++ ) {
    handler = rb_ary_entry(handler_ary, (long) i);
    // printf("\nCCC handler %s", inspect(handler));
    result[i].start_pc   = FIX2INT(rb_iv_get(handler, "@start_pc"));
    result[i].end_pc     = FIX2INT(rb_iv_get(handler, "@end_pc"));
    result[i].handler_pc = FIX2INT(rb_iv_get(handler, "@handler_pc"));
  }

  result[i].handler_pc = 0;  /* set sentinel value */

  return result;
}


static CodeBlock *
codeblock_from_writer(VALUE writer) {

  VALUE bc_ary, val;
  int i, size ;
  int *bc;

  /* Ensure writer has compiled itself */
  rb_funcall(writer, rb_intern("compile"), 0);

  bc_ary = rb_iv_get(writer, "@result");
  size   = RARRAY(bc_ary)->len;

  /*  TRACE("Size is %d\n", size); */
  bc  = ALLOC_N(int, size);
  
  /*  TRACE("Bytecode is at: %x\n",bc); */
  
  for (i=0; i < size; i++) {
    /* TRACE("Entry %d: ", i); */
    val = rb_ary_entry(bc_ary, (long) i);

    if (FIXNUM_P(val)) {
      
      /* TRACE("fixnum %d\n", FIX2INT(val)); */
      bc[i] = FIX2INT(val);
      
    } else if (SYMBOL_P(val)) {
      
      /* TRACE("symbol %d\n", val); */
      bc[i] = SYM2ID(val);

    } else if (rb_obj_is_kind_of(val, cWriter)) {

      /* TRACE("Writer\n"); */ 
      bc[i] = (int) codeblock_from_writer(val);

    } else {

      /* Otherwise val is an immediate value wrapped in an array
      Unwrap it */
      /* TRACE("Unwrapping immediate\n"); */
      bc[i] = (int) rb_ary_entry(val, (long) 0);

    }
  }  

  /* FIXME plenty more validity checks could go here! */
  if (bc[size-2] != RETURN) { /* -2 to skip over parameter to RETURN */
    rb_raise(rb_eStandardError, "bytecode doesn't end in RETURN");
  }

  {
    CodeBlock *codeblock;
    VALUE opt_args_ary;
    int num_offsets;

    codeblock = ALLOC(CodeBlock);
    codeblock->bytecode     = bc;
    codeblock->num_locals   = FIX2INT(rb_iv_get(writer, "@num_locals"));
    codeblock->num_args     = FIX2INT(rb_iv_get(writer, "@num_args"));

    /* FIXME Copy @opt_args to C array, if @opt_args isnt nil.
       Zeroth elem is the number of opt args.
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

    codeblock->handlers = bc_compile_handlers(rb_iv_get(writer, "@handlers"));

    /* TRACE("DONE\n"); */
    return codeblock;
  }
}


static VALUE
bc_define_bytecode_method0(VALUE klass, VALUE name_as_sym, VALUE writer)
{
  CodeBlock *cb = codeblock_from_writer(writer);
  NODE *node    = rb_node_newnode(NODE_BFUNC, cb, cb->num_args, 0); 

  rb_add_method( klass, 
                 SYM2ID(name_as_sym),
		 node,
		 NOEX_PUBLIC);

  return Qnil;
}


extern VALUE ruby_top_self; /* from object.c */

static int
bc_block_given_p() {

  CFrame *cframe;

  if (!thread.on_bc_stack) {
    return 0; 
  }

  TRACE("CCC block_given_p\n");

  cframe = thread.current_frame;

  if (cframe->type != FRAME_C) {
      rb_raise(rb_eNameError, "bc_block_given_p but frame not C");
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

  /*
  if (!bc_block_given_p()) 
    return Qundef;
  */

  if (!thread.on_bc_stack) {
    return Qundef; 
  }

  TRACE("CCC yield\n");

  cframe = thread.current_frame;

  if (cframe->type != FRAME_C) {
      rb_raise(rb_eNameError, "bc_yield but frame not C");
  }
  
  if (!cframe->block.codeblock) {
    rb_raise(rb_eLocalJumpError, "no block given");
  }

  codeblock               = cframe->block.codeblock;
  static_chain_prev_frame = cframe->block.env;

  frame = (Frame *) (((char *) cframe) + sizeof(CFrame));
  frame->f.type                  = FRAME_BYTECODE;
  frame->f.prev_frame            = (Frame *) cframe;
  frame->f.block.codeblock       = 0; 
  frame->f.block.env             = 0;
  frame->static_chain_prev_frame = static_chain_prev_frame;
  frame->self                    = static_chain_prev_frame->self;
  frame->pc                      = codeblock->bytecode;
  frame->codeblock               = codeblock;
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
      rb_raise(rb_eNameError, "bc_call but frame not C");
  }

  frame = (Frame *) (((char *) cframe) + sizeof(CFrame));
  frame->f.prev_frame = (Frame *) cframe;
  frame->f.type       = FRAME_BYTECODE;
  frame->self         = recv;
  frame->pc           = codeblock->bytecode;
  frame->codeblock    = codeblock;

  MEMCPY(&(frame->locals), argv, VALUE, argc_top_level);

  return bc_run(frame);
}

VALUE
bc_stop(VALUE val) {
  TRACE("CCC leaving bc\n");
  thread.on_bc_stack = 0;
  return Qnil;
}

static VALUE
bc_run_from_writer(VALUE runner, VALUE writer) {
  CodeBlock *cb;
  CFrame *cframe;
  Frame *frame;
  VALUE recv    = ruby_top_self; 

  thread.tos = stack;

  cframe = (CFrame *) frame_stack;
  cframe->type = FRAME_C;

  thread.current_frame = cframe;

  cb = codeblock_from_writer(writer);

  frame = (Frame *) (((char *) frame_stack) + sizeof(CFrame));
  frame->f.prev_frame = (Frame *) cframe;
  frame->f.type       = FRAME_BYTECODE;
  frame->self         = recv;
  frame->pc           = cb->bytecode;
  frame->codeblock    = cb;

  TRACE("CCC entering bc\n");
  thread.on_bc_stack = 1;
  return rb_ensure(bc_run, (VALUE) frame, bc_stop, Qnil);

}


void Init_Runner() { 

  stack                = ALLOC_N(VALUE, 100);
  frame_stack          = ALLOC_N(char, 10000);

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

  to_ary = rb_intern("to_ary");
}

