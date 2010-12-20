require 'ruby/interpreter/parse'
require 'bcr/writer'

module Bytecode

  # Helper modules for compilation. They are included into
  # Ruby::Interpreter classes.
  module CompileHelpers

    # *** General helpers ***************************************

    # Included in nodes that we're not interested in
    module Ignore
      def compile_for_value(w)
	c = child
	c.compile_for_value(w) if c
      end
      def compile_for_void(w)
	c = child
	c.compile_for_void(w) if c
      end
      def compile_for_return(w)
	c = child
	c.compile_for_return(w) if c
      end
    end

    # Helper for classes where compile_for_value is pretty much the
    # same as compile_for_void, so we implement compile_for_value by
    # passing an optional flag to ..for_void.
    module CompileForValueUsingVoid
      def compile_for_value(w)
        compile_for_void(w, true)
      end
    end

    # No point compiling in void context. Such situations are normally
    # trapped in the parser (as useless use of a literal in void context)
    # but some slip through
    module IgnoreCompileForVoid
      def compile_for_void(w); end
    end



    # *** Misc helpers ***************************************

    # Common code for NodeWhile and NodeUntil
    module NodeWhileUntil
      def compile_for_void(w)
	w.loops.while_loop do |loop|
	  loop.before_condition.place
          
          conditional.compile_for_value(w)

	  cond_jump(w,loop)
           
	  loop.after_condition.place

          body.compile_for_void(w) if body

          w.goto(loop.before_condition)

	  loop.out.place
        end
      end
    end


    # Common code for NodeDot2 (a..b) and NodeDot3 (a...b)
    module NodeDot
      def compile_for_value(w)
	w.ld_imm(Range)
	zbegin.compile_for_value(w)
	zend.compile_for_value(w)
        w.ld_imm(exclusive?).
	  call(:new, 3)
      end
    end

    
    # Common code for NodeOr and NodeAnds. Needs #if_type    
    module NodeOrAnd
      def common_code(w)
        out_lbl = w.new_label
        first.compile_for_value(w)
	yield out_lbl
        out_lbl.place
      end
      def compile_for_value(w)
	common_code(w) do |out_lbl|
	  w.dup.send(if_type, out_lbl).pop
	  second.compile_for_value(w)
	end
      end
      def compile_for_void(w)
	common_code(w) do |out_lbl|
	  w.send(if_type, out_lbl)
	  second.compile_for_void(w)
	end
      end
    end


    # *** Assignment helpers *************************************

    # Common code for assignments - looks after getting the
    # value on to the stack
    # Needs #assign_from_stack()
    module Assign
      include CompileForValueUsingVoid
      def compile_for_void(w, really_for_value = false)
        value.compile_for_value(w) if value
	if value or w.honour_nil_asgns_p
	  w.dup if really_for_value
	  assign_from_stack(w)
	end
      end 
    end

    module LocalAssign
      include Assign
      def assign_from_stack(w)
	w.locals.store_to_local_called(variable_id)
      end
    end

    module LocalAccess
      def compile_for_value(w)
	local_id, depth = w.locals.find_local(variable_id)
	if depth == 0
	  w.ld_loc(local_id)
	else
	  w.ld_loc_l(local_id, depth)
	end
      end 
    end

    module ConstAccess
      def const_get(w, const_name_as_sym)
	w.ld_self.
	  ld_imm(const_name_as_sym).
          call(:bcr_const_get, 1)	
      end
    end

    # *** Method definition helpers ********************************

    # Common code for NodeDefn and NodeDefs
    # Needs #define_method
    module MethodDefinition

      attr_reader :normal_args,
                  :optional_args,
                  :rest_arg,
		  :block_arg,
                  :body
 
      def compile_for_void(w)
	decode()

	# Set up a new writer to contain the method's body
        method_writer = Bytecode::Writer.new(nil, method_id)

	# Compile arguments
        method_writer.locals.register_locals(normal_args)
	method_writer.num_args = normal_args.size

	compile_opt_args(method_writer)

        if rest_arg
	  method_writer.locals.register_local(rest_arg)
	  method_writer.rest_arg = true
	end

	if block_arg
	  method_writer.make_proc
	  method_writer.locals.store_to_local_called(block_arg)
	end

	# Compile the body
	if body
	  body.compile_for_return(method_writer)
	else
	  method_writer.ld_imm(nil).return(0)
	end

	# Ask subclass to define the method in the owning writer
	define_method(w, method_writer)
      end

      def compile_opt_args(method_writer)
	return unless optional_args

	jump_points = [0]
	optional_args.each_child do |arg, last|
	  unless Ruby::Interpreter::NodeLasgn === arg
	    raise "unexpected assignment to non-local var for optional args"
	  end
	  arg.compile_for_void(method_writer)
	  jump_points.push(method_writer.current_pc)
	end

	method_writer.opt_args_jump_points = jump_points
      end

      def decode
	# Decode the MRI representation of arguments into something
        # a bit more understandable.
        return if @decodedp
	
	# Method     Object
	#            Defn
	# defn         Scope                        
	# table          arg names
	# next_node      Block
        # head             Args 
	# arg_names          number required args
	# rest               index of rest arg + 2, or -1 if no rest arg
	# optional           optional args, if any
	# next_node        body

	scope     = defn
        arg_names = scope.table
	nn        = scope.next_node
	if nn.kind_of?(Ruby::Interpreter::NodeArgs)
	  args = nn
	  body = nil
	else
	  args = nn.head
	  body = nn.next_node
	end

        @normal_args   = arg_names[0, args.count]
	@optional_args = args.optional
        @rest_arg      = if args.rest == -1
                           nil
                         else
                           arg_names[args.rest - 2]
                         end

	if !body
	  return
        elsif body.head.kind_of?(Ruby::Interpreter::NodeBlock_arg)
          @block_arg = body.head.variable_id
          @body      = body.next_node
        else
          @block_arg = nil
          @body      = body
        end

        @decodedp = true 
      end
    end

    # *** Call helpers *********************************************

    # Common code for NodeCall, NodeFcall, NodeVcall, 
    # NodeSuper and NodeZsuper
    module Call

      def compile_for_value(w, options = {})
        put_receiver_on_stack(w)
	argc           = compile_args(w)
        block          = options[:block] || nil
        ampersand_argp = options[:ampersand_arg] || false
	w.call(method_name(w), argc, 
	       superp, private_okp, ampersand_argp, 
	       block)
      end

      # Classes which use this module can override the default methods
      # which follow ..
      def compile_args(w)
	case args
	when Ruby::Interpreter::NodeArray     # normal args only, no scatter
	  argc = args.unroll_onto_stack(w)
	when NilClass                         # no args at all
	  argc = 0
	when Ruby::Interpreter::NodeArgscat   # normal (head) + scatter (body)
	  argc = -(1 + args.head.unroll_onto_stack(w))
	  args.body.compile_for_value(w)
	when Ruby::Interpreter::NodeRestary   # scatter only
	  argc = -1
	  args.head.compile_for_value(w)
	else
	  raise "Found unknown node when compiling args: #{args.type}"
	end
	argc
      end

      # .. more defaults
      def method_name(w); method_id; end 
      def superp;         false;     end
      def private_okp;    false;     end
      def put_receiver_on_stack(w)
	w.ld_self
      end
    end

    # Super call helper
    module Super
      def superp; true; end
      def method_name(w)
	w.method_name
      end
    end

    # Helper for use in contexts where private calls are OK
    module PrivateOk
      def private_okp; true; end
    end

    # Helper for iterator and for calls
    module IterForHelper
      def block_compiler_proc(locals_in_parent_p)
	proc do |block|
	  block.locals.create_in_parent = locals_in_parent_p
          block.num_args = 1
	  if variable
	    # yield puts args in first local, 
	    # but we want them on the stack
	    block.ld_loc(0) 
	    block.honouring_nil_asgns do
	      variable.compile_for_void(block) 
	    end
	  end
	  block.loops.redo_label() # set the target for any redo's
	  if body
	    body.compile_for_return(block)
	  else
	    block.ld_imm(nil).return(0)
	  end
        end
      end
    end

  end
end
