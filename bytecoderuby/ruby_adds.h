#include "ruby.h"
#include "node.h"

VALUE call_cfunc(VALUE (*func)(), VALUE recv, int len, int argc, VALUE *argv);

NODE *rb_get_method_body( VALUE *klassp, ID *idp, int *noexp);

VALUE rb_call( VALUE klass, VALUE recv, ID mid, int argc, 
	       VALUE *argv, int scope);

NODE * rb_get_method_body_in_cache(VALUE klass, VALUE recv, ID mid, int argc, 
				   VALUE *argv, int scope);



