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


/* value stack */
VALUE *stack;

#define PUSH_STACK(val) *(tos++) = (val)

#define POP_STACK() (*(--tos))

/* key structures */ 
typedef struct codeblock {
  int *bytecode;
  int num_locals;
  int num_args;
} CodeBlock;

typedef struct closure {
  CodeBlock *codeblock;
  struct frame *env;
} Closure;

/* FIXME move stack into frame? */
typedef struct frame { 
  /* the code being run in this frame */
  CodeBlock *codeblock;

  /* the Ruby block associated with this method call */
  Closure block;

  /* the previous frame in the call stack */
  struct frame *prev_frame;

  /* the previous frame in the static chain */
  struct frame *static_chain_prev_frame;

  /* the pc to restart when execution returns to this frame */
  int   *pc;

  /* local variables (these run on beyond the end of the 
     frame struct) */
  VALUE locals; 

} Frame;

/* frame stack */
char *frame_stack;
Frame *next_frame;

/* The size of a frame (in bytes) that can hold num_locals locals. 
   -1 for the initial local declared in the Frame */
#define FRAME_SIZE(num_locals) (sizeof(Frame) + sizeof(VALUE) * ((num_locals) -1)) 


/* FIXME - inline this for handy performance boost */
static NODE* bc_find_method( ID method_id, VALUE recv, int num_args, 
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

static Frame *get_frame_from_static_chain(Frame *frame, int count) {
  /* Returns the frame COUNT frames back up the static chain from
     FRAME */
  for (;;) {
    if (frame == (Frame *) 0) {
      /* FIXME - proper exception (or rb_bug?) */
      rb_raise(rb_eStandardError, "static chain exhausted");
    }
    if (count == 0) {
      return frame;
    }
    count -= 1;
    frame = frame->static_chain_prev_frame;
  }
}

/*
  Frame maintenance
  Will need revisiting when frames are potentially heap-allocated
  (because captured in a closure).
 */
void
frame_reenter(Frame *frame, int **pc, VALUE **locals) {
  int sz;

  sz         = FRAME_SIZE(frame->codeblock->num_locals);
  next_frame = (Frame *) (((char *) frame) + sz);
  *locals    = &(frame->locals);
  *pc        = frame->pc;
}

static Frame *
frame_alloc( Frame *current_frame, CodeBlock *codeblock, 
	     int **pc, VALUE **locals) {

  Frame *new_frame;
  int sz;

  current_frame->pc = *pc;

  new_frame  = next_frame;  
  new_frame->codeblock  = codeblock;
  new_frame->prev_frame = current_frame;

  sz         = FRAME_SIZE(codeblock->num_locals);
  next_frame = (Frame *) (((char *) next_frame) + sz);
  *locals    = &(new_frame->locals);
  *pc        = codeblock->bytecode;

  return new_frame;
}

static char* inspect(VALUE s) {
  return STR2CSTR(rb_funcall(s, rb_intern("inspect"), 0));
}

static VALUE bc_run(VALUE runner, CodeBlock *codeblock, VALUE recv, 
		    int argc_top_level, VALUE *argv) {

  VALUE *locals, *tos = stack;
  int *pc;
  int i;


  Frame *current_frame;

  next_frame = (Frame *) frame_stack; /* reset frame stack */
  current_frame = frame_alloc(next_frame, codeblock, &pc, &locals);
  current_frame->prev_frame = 0;

  locals[0] = recv;
  for (i=0; i < argc_top_level; i++)
    locals[i+1] = argv[i];

  TRACE("Received codeblock %x\n", codeblock);

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
	Frame *frame = get_frame_from_static_chain(current_frame, count);
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
	Frame *frame = get_frame_from_static_chain(current_frame, count);
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
    case CALL:
      {
        int method_id = pc[1];
        int argc      = pc[2]; 
	CodeBlock *block = (CodeBlock *) pc[3];

	VALUE recv, *argv, result;
	NODE *nd;

        TRACE("CALL %d\n", argc);
        pc   += 4;
	tos  -= argc + 1;

	
	recv = *tos;
	argv = tos + 1;

	nd = bc_find_method(method_id, recv, argc, argv);

	switch nd_type(nd) {
	case NODE_CFUNC: { 
	  /* fast-track C functions */
	  PUSH_STACK(call_cfunc(nd->nd_cfnc, recv, nd->nd_argc, argc, argv));
	  break;  
	}
	case NODE_BFUNC: {

	  /* bytecode method - need to interpret it */

	  CodeBlock *codeblock = (CodeBlock *) nd->nd_cfnc;
	  Frame *new_frame;

	  /* FIXME optional and star args */
          if (argc != codeblock->num_args)
	    rb_raise(rb_eArgumentError, 
		     "wrong # of arguments (%d for %d)",
		     argc, codeblock->num_args);

	  new_frame = frame_alloc(current_frame, codeblock, &pc, &locals);
	  new_frame->block.codeblock         = block;      
	  new_frame->block.env               = current_frame;
	  new_frame->static_chain_prev_frame = 0;

	  DEBUG {
            for (i=0; i < codeblock->num_locals; i++)
	      locals[i] = Qnil;
          }

	  locals[0] = recv;
	  for (i=0; i < argc; i++)
	    locals[i+1] = argv[i];

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
	  // rb_call( klass, recv, method_id, argc, argv, 1);
	  return Qnil;
	  break;
	}

        break;
      }
    case RETURN:
      {
	int count = pc[1];

	TRACE("RETURN %d\n", count);

	current_frame = get_frame_from_static_chain(current_frame, count)->prev_frame;

	/* were we called from C? */
	if (current_frame == (Frame *) 0) {
	  return POP_STACK();
	}

	/* return to previous frame - result should be on top 
	   of the stack - just where we need it */
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

	block_owner_frame = get_frame_from_static_chain(current_frame, count);

	codeblock               = block_owner_frame->block.codeblock;
	static_chain_prev_frame = block_owner_frame->block.env;

	if (!codeblock) {
	  rb_raise(rb_eLocalJumpError, "yield called out of block");
	}

	new_frame = frame_alloc(current_frame, codeblock, &pc, &locals);

	/* blocks don't have blocks themselves */
	new_frame->block.codeblock         = 0; 
	new_frame->block.env               = 0;
	new_frame->static_chain_prev_frame = static_chain_prev_frame;

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
        NODE *node;

        TRACE("DEFN\n");

        klass = POP_STACK();
        Check_Type(klass, T_CLASS);

	node = rb_node_newnode(NODE_BFUNC, method, method->num_args, 0); 
	rb_add_method( klass, name, node, NOEX_PUBLIC);

	/* FIXME clear method look up cache */

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
    default:
      {
	rb_raise(rb_eStandardError, "unknown bytecode");
	/* not reached */
      }
    }
  }
}

static CodeBlock *codeblock_from_writer(VALUE writer) {

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
    codeblock = ALLOC(CodeBlock);
    codeblock->bytecode   = bc;
    codeblock->num_locals = FIX2INT(rb_iv_get(writer, "@num_locals"));
    codeblock->num_args   = FIX2INT(rb_iv_get(writer, "@num_args"));
    
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

  /* FIXME clear method look up cache */

  return Qnil;
}


extern VALUE ruby_top_self; /* from object.c */

static VALUE
bc_run_from_writer(VALUE runner, VALUE writer) {
  CodeBlock *cb;
  VALUE recv    = ruby_top_self; 
  int argc      = 0;
  VALUE *argv   = (VALUE *) 0;

  cb = codeblock_from_writer(writer);

  return bc_run(cRunner, cb, recv, argc, argv);

}


void Init_Runner() { 

  stack = ALLOC_N(VALUE, 100);
  frame_stack = ALLOC_N(char, 10000);

  mBytecode = rb_define_module("Bytecode");

  cRunner   = rb_define_class_under(mBytecode, "Runner", rb_cObject);

  rb_define_singleton_method( cRunner, "run", bc_run, 4);
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

