#include "ruby.h"
#include "node.h"

VALUE call_cfunc(VALUE (*func)(), VALUE recv, int len, int argc, VALUE *argv);

NODE *rb_get_method_body( VALUE *klassp, ID *idp, int *noexp);

VALUE rb_call( VALUE klass, VALUE recv, ID mid, int argc, 
	       VALUE *argv, int scope);

NODE * rb_get_method_body_in_cache(VALUE klass, VALUE recv, ID mid, int argc, 
				   VALUE *argv, int *noexp);

VALUE safe_call_cfunc(VALUE (*func)(), VALUE recv, int len, int argc, VALUE *argv, 
		      VALUE (*e_proc)(), VALUE data2);

void bc_push_tag(struct tag *_tag);
void bc_pop_tag(struct tag *_tag);
void bc_jump_tag(int jump_type);

VALUE bc_current_exception(void);
