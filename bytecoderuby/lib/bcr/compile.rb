require 'bcr/compile_helpers'

module Ruby
  module Interpreter

    # shortcut
    Helpers = Bytecode::CompileHelpers
  
    ########## Base class for nodes ########################
    class Node
      # every node type must override at least one of 
      # compile_for_value and compile_for_void
      def compile_for_value(w, &block)
	# Avoid never-ending mutual recursion ..
	raise "Must override one of void/value for #{self.class}" if @alert 
	@alert = true
	compile_for_void(w, &block)
	w.ld_imm(nil)
      end
      def compile_for_void(w, &block)
	compile_for_value(w, &block)
	w.pop
      end
      def compile_for_return(w)
	compile_for_value(w)
	w.return(0)
      end
    end

    ########## Misc ########################################
    class NodeNewline
      include Helpers::Ignore
      def child; next_node; end
    end
    class NodeScope
      include Helpers::Ignore
      def child; next_node; end
    end
    class NodeBegin
      include Helpers::Ignore
      def child; body; end
    end
    class NodeSvalue
      include Helpers::Ignore
      def child; head; end
    end

    class NodeArgscat
      def compile_for_value(w)
	# FIXME real ruby doesn't call Array#concat
	head.compile_for_value(w)
	body.compile_for_value(w)
        w.call(:concat, 1)
      end
    end

    class NodeBlock
      def each_child
        node = self
        while node
          h  = node.head
          nn = node.next_node
          if nn
            yield h, false
          else
            yield h, true
          end
          node = nn
        end
      end
      def compile_for_value(w)
        each_child do |block, last|
          if last
            block.compile_for_value(w)
          else
            block.compile_for_void(w)
          end
        end
      end
      def compile_for_void(w)
        each_child do |block, last|
          block.compile_for_void(w)
        end
      end
      def compile_for_return(w)
        each_child do |block, last|
          if last
            block.compile_for_return(w)
          else
            block.compile_for_void(w)
          end
        end
      end
    end	

    class NodeMasgn
      include Helpers::CompileForValueUsingVoid
      def compile_for_void(w, really_for_value = false)

	# VALUE - array - the rhs of the assignment
	# HEAD - array - the lhs individual assignments (except gather)
	# MASGN_ARGS - assignment to the gather arg (if any)
	
        value.compile_for_value(w) if value 

        num_elems = head ? head.length : 0
	gather    = masgn_args
	gather    = nil if gather == :*  # not sure why * gets passed thru
        
        w.ary_scatter(num_elems, !!gather, really_for_value)

	w.honouring_nil_asgns do
	  gather.compile_for_void(w) if gather
	  head.unroll_for_masgn(w) if head
	end
      end
    end

    ########## Literals ####################################
    class NodeLit
      # Numbers (ints, floats etc)
      include Helpers::IgnoreCompileForVoid
      def compile_for_value(w); w.ld_imm(literal); end
    end

    class NodeFalse
      include Helpers::IgnoreCompileForVoid
      def compile_for_value(w); w.ld_imm(false); end
    end

    class NodeTrue
      include Helpers::IgnoreCompileForVoid
      def compile_for_value(w); w.ld_imm(true); end
    end

    class NodeNil
      include Helpers::IgnoreCompileForVoid
      def compile_for_value(w); w.ld_imm(nil); end
    end

    class NodeStr
      # A string.
      include Helpers::IgnoreCompileForVoid
      # FIXME Ruby dups literal strings each time they 
      # are executed.  	
      def compile_for_value(w); w.ld_imm(literal); end
    end

    class NodeSelf
      # Self .. not really a literal, but near enough.
      include Helpers::IgnoreCompileForVoid
      def compile_for_value(w); w.ld_self; end
    end

    class NodeDot2
      # An inclusive range ( .. )
      include Helpers::NodeDot
      def exclusive?; false; end
    end

    class NodeDot3
      # An exclusive range ( ... )
      include Helpers::NodeDot
      def exclusive?; true; end
    end

    class NodeHash
      # A hash
      def compile_for_value(w)
	# FIXME real ruby doesn't call Hash.[]
        w.ld_imm(Hash)
        argc = head.unroll_onto_stack(w)
        w.call(:[], argc)
      end
    end

    class NodeZarray
      # An empty array.
      # FIXME real ruby doesn't call Array.[]
      def compile_for_value(w); 
        w.ld_imm(Array).
          call(:[], 0)
      end
    end

    class NodeArray
      # An array. Arrays are used internally by MRI so there's
      # additional helper methods on here besides the obvious
      # compile_for_value
      def compile_for_value(w)
	# FIXME real ruby doesn't call Array.[]
        w.ld_imm(Array)
        argc = unroll_onto_stack(w)
        w.call(:[], argc)
      end
      def unroll_onto_stack(w)
        # Compiles each head element in self and self's subarrays.
	# Returns the #elements in self.
	ary = self
	while ary
	  ary.head.compile_for_value(w)
          ary = ary.next_node
        end
        length
      end
      def unroll_for_masgn(w)
        # Compiles (for void) the elements of self, *in reverse order*
        # Used for NodeMasgn (multiple assignment).
        next_node.unroll_for_masgn(w) if next_node
        head.compile_for_void(w)
      end
      def each_child
        node = self
        while node
	  yield node.head
          node = node.next_node
        end
      end
    end

    ############ Variables ###################################
    class NodeLasgn
      # Assignment to a local, a = 1
      include Helpers::LocalAssign
    end

    class NodeDasgn
      # Assignment to a local in a block, a = 1
      include Helpers::LocalAssign
    end

    class NodeDasgn_curr
      # Assignment to a local in the current block, a = 1
      include Helpers::LocalAssign
      # Dasgn_curr seems to be assignment to a var in innermost block,
      # so we could actually optimise the compilation code here. It
      # doesn't seem worth it however
    end

    class NodeLvar
      # Access to a local, a
      include Helpers::LocalAccess
    end

    class NodeDvar
      # Access to a local in a block, a
      include Helpers::LocalAccess
    end

    class NodeIasgn
      # Assignment to an instance variable, @a = 1
      include Helpers::Assign
      def assign_from_stack(w)
	w.st_ivar(variable_id)
      end 
    end

    class NodeIvar
      # Access to an instance variable, @a
      def compile_for_value(w)
	w.ld_ivar(variable_id)
      end
    end

    class NodeGasgn
      # Assignment to a global, $a = 1
      include Helpers::Assign
      def assign_from_stack(w)
	w.st_gvar(variable_id)
      end 
    end

    class NodeGvar
      # Access to a global, $a
      def compile_for_value(w)
	w.ld_gvar(variable_id)
      end
    end

    class NodeConst
      include Helpers::ConstAccess
      # Access to a constant, A
      def compile_for_value(w)
	const_get(w, variable_id)
      end
    end

    #############  Class variables #################
    class NodeCvdecl
    # Decl. of class variable
      include Helpers::Assign
      def assign_from_stack(w)
        w.ld_self
        w.def_cvar(variable_id)
      end
    end

    class NodeCvasgn
    # Assignment to class variable
     include Helpers::Assign
      def assign_from_stack(w)
        w.st_cvar(variable_id)
      end
    end

    class NodeCvar
    # Access to class variable
      def compile_for_value(w)
        w.ld_cvar(variable_id)
      end
    end

    #############  Class, method definitions #################
    class NodeClass
      # Class declaration class A .. end
      include Helpers::CompileForValueUsingVoid
      def compile_for_void(w, really_for_value=false)
	# Compile the class body into a separate method_writer
        method_writer = Bytecode::Writer.new()

	args = body.table
        method_writer.locals.register_locals(args)
	method_writer.num_args = args.size

	body.compile_for_return(method_writer) if body

	# Now the bytecode for the class; end statement itself
	w.ld_self.
	  ld_imm(class_name.method_id) # FIXME don't understand Colon2!
	if zsuper
	  # FIXME this should be protected by a rescue so that if the
	  # zsuper can't be found we raise the correct 'TypeError:
	  # undefined superclass' rather than 'NameError:
	  # uninitialized constant' as currently
	  zsuper.compile_for_value(w)
	else
	  w.ld_imm(nil)
	end
	w.call(:get_or_make_class, 2)

	# .. this is where we use the class body - we run it with 
	# the new (or retrieved) class as self
	w.run(method_writer) unless method_writer.current_pc == 0

	w.pop if !really_for_value
      end
    end

    class NodeDefn
      # Define a method, def a .. end
      include Helpers::MethodDefinition
      def define_method(w, method_writer)
        w.ld_self.
	  defn(method_id, method_writer)
      end
    end

    class NodeDefs
      # Define a singleton method, def o.a .. end
      include Helpers::MethodDefinition
      def define_method(w, method_writer)
        receiver.compile_for_value(w)
        w.defs(method_id, method_writer)
      end
    end

    ######## Calls#####################################
    class NodeCall
      # Call o.a()
      include Helpers::Call
      def put_receiver_on_stack(w)
        receiver.compile_for_value(w)
      end
    end

    class NodeAttrasgn
      # Call o.a() (not sure how this differs from Call)
      include Helpers::Call
      def put_receiver_on_stack(w)
        receiver.compile_for_value(w)
      end
    end

    class NodeFcall
      # Call to self, self.a() or a()
      include Helpers::Call
      include Helpers::PrivateOk
    end

    class NodeVcall
      # Call to self that can't be distinguished from local access, a
      include Helpers::Call
      include Helpers::PrivateOk
      def compile_args(w); 0; end
    end

    class NodeSuper
      # Call to super, super(a)
      include Helpers::Call
      include Helpers::Super
    end

    class NodeZsuper
      # Call to super using calling method's args, super
      include Helpers::Call
      include Helpers::Super
      def compile_args(w)
	# FIXME *args also
	num_args = w.num_args + w.num_opt_args
	(0...num_args).each { |local_id| w.ld_loc(local_id) }
	num_args
      end
    end

    ######## Iterator and for #####################################
    class NodeIter
      # Call with a block, o.a() { .. }
      include Helpers::IterForHelper
      def compile_for_value(w)
	p = block_compiler_proc(false)
	iter.compile_for_value(w, :block => p)
      end
    end

    class NodeFor
      # For, for .. in .. end
      include Helpers::IterForHelper
      def compile_for_value(w)
	iter.compile_for_value(w)
	p = block_compiler_proc(true)
	w.call(:each, 0, false, false, false, p)
      end
    end

    ######## Ampersand args  ############################    
    class NodeBlock_pass
      # Call with an ampersand arg, o.a(&p)
      def compile_for_value(w)
	body.compile_for_value(w)
	iter.compile_for_value(w, :ampersand_arg => true)
      end
    end

    ######## Yield and return #####################################
    class NodeYield
      # Yield to block, yield a
      def compile_for_value(w)
	if stts
	  stts.compile_for_value(w)
	else
	  w.ld_imm(nil)
	end
        w.yield(1, w.depth)
      end
    end

    class NodeReturn
      # Return, return a
      def compile_for_void(w)
        if stts
          stts.compile_for_value(w)
        else
          w.ld_imm(nil)
        end
        w.return(w.depth)
      end
      def compile_for_return(w)
        compile_for_void(w)
      end
    end

    ######## Until  #####################################
    class NodeUntil
      # until <cond> .. end
      include Helpers::NodeWhileUntil
      def cond_jump(w,loop)
        w.if(loop.out)
      end
    end

    ######## While  #####################################
    class NodeWhile
      # While loop, while <cond> .. end
      include Helpers::NodeWhileUntil
      def cond_jump(w,loop)
       w.if_not(loop.out)
      end
    end

    ######## Loop control #####################################
    class NodeBreak
      def compile_for_void(w)
	if stts
	  stts.compile_for_value(w)
	else
	  w.ld_imm(nil)
	end
	w.loops.break
      end
    end

    class NodeNext
      def compile_for_void(w)
	w.loops.next
      end
    end

    class NodeRedo
      def compile_for_void(w)
	w.loops.redo
      end
    end

    class NodeRetry
      def compile_for_void(w)
	# retry only valid inside blocks and rescue clauses,
	# so we don't use current_loop
#	w.retry
      end
    end


    ######## If, and, or, not #####################################
    class NodeIf
      def common_code(w)
        not_true_lbl = w.new_label
        end_lbl = w.new_label

        conditional.compile_for_value(w)
	w.if_not(not_true_lbl)

	yield not_true_lbl, end_lbl

        end_lbl.place
      end
      def compile_for_value(w)
	common_code(w) do |not_true_lbl, end_lbl|
	  body ? body.compile_for_value(w) : w.ld_imm(nil)
	  w.goto(end_lbl)
	  
	  not_true_lbl.place
	  zelse ? zelse.compile_for_value(w) : w.ld_imm(nil)
	end
      end
      def compile_for_void(w)
	common_code(w) do |not_true_lbl, end_lbl|
	  body.compile_for_void(w) if body
	  w.goto(end_lbl) if zelse

	  not_true_lbl.place
	  zelse.compile_for_void(w) if zelse
	end
      end
    end

    class NodeOr
      include Helpers::NodeOrAnd
      def if_type; :if; end
    end

    class NodeAnd
      include Helpers::NodeOrAnd
      def if_type; :if_not; end
    end

    class NodeNot
      def compile_for_value(w)
        true_lbl = w.new_label
        out_lbl = w.new_label
	body.compile_for_value(w)
	w.if(true_lbl).
          ld_imm(true).
          goto(out_lbl)
	true_lbl.place
	w.ld_imm(false)
	out_lbl.place
      end
    end

    class NodeCase
=begin
Tree structure is:

                      Msg to send
                      to parent to
                      get this
Item                  item         Description
--------------------------------------------------------------------------
case                               the case statement
  value               head         the value to test
  when1               body         the first when clause
    array1            head         array of poss. matches (2 in this case)
      array2
    code1             body         the body of the first when clause
    when2             next_node    second when clause
      array1                       only poss. match for this clause
      code2                        the body of the second when clause
      code_else                    the body of the else clause (or nil if none)

which generates code like:

push value
dup value
if when1.array1 === value goto code1
dup value
if when1.array2 === value goto code1
goto when2
pop value
code1: code1
goto out
dup value
if when2.array1 === value goto code2
goto when3
pop value
code2: code2
goto out
when3:
out:
rest of code
=end
      include Helpers::CompileForValueUsingVoid      
      def compile_for_void(w, really_for_value = false)
        out_lbl     = w.new_label #("out")
	compile_msg = if really_for_value
			:compile_for_value
		      else
			:compile_for_void
		      end

	# the value to be tested
	# FIXME what if we eg return from inside the case statement -
	# test value will get left on the stack
	head.compile_for_value(w)

        body.each_child do |when_or_else_node|
	  case when_or_else_node
	  when NodeWhen
	    when_or_else_node.send(compile_msg, w, out_lbl)
	  when nil
	    # this code gets executed when all matches have failed
	    # and there's no else clause
	    w.pop       # remove test value
	    w.ld_imm(nil) if really_for_value
	  else
	    # else clause - will be the last in the list.
	    # Run the else clause's body code and fall
	    # through to the code after the case statement
            w.pop         # remove test value
            when_or_else_node.send(compile_msg, w)
	  end
        end

        out_lbl.place
      end
    end

    class NodeWhen
      def each_child
        node = self
        while NodeWhen === node
	  yield node
          node = node.next_node
        end
	yield node
      end

      def compile_for_value(w, out_lbl)
	compile_for_void(w, out_lbl, true)
      end

      def compile_for_void(w, out_lbl, really_for_value = false)
	code_lbl    = w.new_label
	compile_msg = if really_for_value
			:compile_for_value
		      else
			:compile_for_void
		      end

	# when clause - check each test item in turn,
	# jumping to the clause's body if we have a match
	head.each_child do |match_value|
	  w.dup                             # duplicate the test value
	  match_value.compile_for_value(w)  # get the tester
	  w.swap
	  w.call(:===, 1)
	  w.if(code_lbl)
	end
	
	# come here if no matches
	next_when_lbl = w.new_label 
	w.goto(next_when_lbl)  
	
	# come here if we had a match
	code_lbl.place
	w.pop                  # remove test value
	if body	       
	  body.send(compile_msg, w) 
	else
	  # empty when clause body
	  w.ld_imm(nil) if really_for_value
	end
	w.goto(out_lbl)
	
	# label picking up the next when (or else) clause
	next_when_lbl.place
      end
    end

    ######## Exceptions #############################
    class NodeEnsure
      def compile(w, zbody, zensure, compile_msg)
	if !zbody
	  # empty body has value nil
	  w.ld_imm(nil) if compile_msg == :compile_for_value 
	  zensure.compile_for_void(w)
	  # no need to rehandle - ensure must have run normally
	  # without a jump (no body to cause the jump!)
	  return
	end

	handler = w.handlers.with_ensure_handler do
	  zbody.send(compile_msg, w)
	end
	
	handler.in_handler do
	  # ensure block can never have a value
	  zensure.compile_for_void(w)
	  w.jump_rehandle(handler.storage)
	end
      end
      def compile_for_void(w)
	compile(w, head, ensr, :compile_for_void)
      end
      def compile_for_value(w)
	compile(w, head, ensr, :compile_for_value)
      end
    end


    class NodeRescue
      # A block with rescue clauses, begin .. rescue .. end
      def compile_for_value(w)
	#     head                protected by handler
	#     else                protected by prev handler
	#     goto no_exception
	#     handler             protected by prev handler
	#     no_exception:
	if !head
	  # no exceptions possible if no head
	  w.ld_imm(nil)
	  return
	end

	no_exception_label = w.new_label
	handler = w.handlers.with_rescue_handler do
	   head.compile_for_value(w)
	end

	zelse.compile_for_value(w) if zelse
	w.goto(no_exception_label)

	handler.in_handler do
	  resque.compile_for_value(w, no_exception_label, handler) if resque
	end

	no_exception_label.place
      end
    end

    class NodeResbody
      include Helpers::ConstAccess
      # A rescue clause, rescue
      def compile_for_value(w, out_lbl, handler)
	compile_for_void(w, out_lbl, handler, true)
      end
      def compile_for_void(w, out_lbl, handler, really_for_value = false)
	code_lbl = w.new_label 
	compile_msg = if really_for_value
			:compile_for_value
		      else
			:compile_for_void
		      end

	if args  # catch specific exception classes
	  args.each_child do |exception_class|
	    w.dup
	    exception_class.compile_for_value(w)
	    w.swap.
	      call(:===, 1).
	      if(code_lbl)
	  end
	else
	  w.dup  # no exception class specified - catch StandardError
	  const_get(w, :StandardError)  
	  w.swap.
	    call(:===, 1).
	    if(code_lbl)
	end

	# come here if no matches
	next_rescue_lbl = w.new_label
	w.goto(next_rescue_lbl)  
	
	# come here if we had a match
	code_lbl.place

	if body
	  rbody = maybe_store_exception(w, body)
	  rbody.send(compile_msg, w) if rbody
	else
	  # empty rescue clause body
	  w.pop   # remove exception value
	  w.ld_imm(nil) if really_for_value
	end
	w.goto(out_lbl)
	
	# label picking up the next rescue (or else) clause
	next_rescue_lbl.place
	if head
	  # FIXME what about value/void distinction here?
	  head.compile_for_value(w, out_lbl, handler)
	else
	  # don't pop - leave exception on stack for reraising
	  w.jump_rehandle(handler.storage)
	end
      end

      def maybe_store_exception(w, resbody)
	# Handle the '=> e' part of 'rescue Exception => e'
	# Unfortunately this Ruby construct gets mangled into 
	# Block(Assign(e, Gvar($!))) in the parse tree, so we have to 
	# hack about a bit to detect it

	if resbody.kind_of?(NodeBlock)
	  assign = resbody.head
	  retval = resbody.next_node
	else
	  assign = resbody
	  retval = nil
	end

	if assign.kind_of?(Helpers::Assign) and 
	   (gvar = assign.value).kind_of?(NodeGvar) and
	   gvar.variable_id == :$!

	  # This covers the very obscure case of 'rescue blah => e; end'
	  # ie no actual handler code. In this case, the value 
	  # of the rescue expression is the exception, so we dup the 
	  # exception so that it gets left on the stack after the store.
	  # FIXME what if rescue is in void context?
	  w.dup unless retval

	  assign.assign_from_stack(w)

	  resbody = retval
	else
	  w.pop   # remove exception value
	end

	return resbody
      end
    end

  end
end

