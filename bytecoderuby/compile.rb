require 'ruth/mri'
require 'writer'

module Ruby
  module Interpreter

    class MriNode
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
    module Ignore
      def compile_for_value(w);  child.compile_for_value(w);  end
      def compile_for_void(w);   child.compile_for_void(w);   end
      def compile_for_return(w); child.compile_for_return(w); end
    end

    class MriNodeNewline
      include Ignore
      def child; next_node; end
    end
    class MriNodeScope
      include Ignore
      def child; next_node; end
    end
    class MriNodeBegin
      include Ignore
      def child; body; end
    end

    module CompileForValueUsingVoid
      # convenience method for classes where compile_for_value is 
      # pretty much the same as compile_for_void, so we implement
      # compile_for_value by passing an optional flag to ..for_void.
      def compile_for_value(w)
        compile_for_void(w, true)
      end
    end

    module PushSelf
      def push_self(w)
	w.ld_self
      end
    end

    class MriNodeSelf
      include PushSelf
      def compile_for_value(w); push_self(w); end
    end

    class MriNodeArray
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
        # Used for MriNodeMasgn (multiple assignment).
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
      def compile_for_value(w)
	# FIXME real ruby doesn't call Array.[]
        w.ld_imm(Array)
        argc = unroll_onto_stack(w)
        w.call(:[], argc)
      end
    end

    class MriNodeArgscat
      def compile_for_value(w)
	# FIXME real ruby doesn't call Array#concat
	head.compile_for_value(w)
	body.compile_for_value(w)
        w.call(:concat, 1)
      end
    end

    class MriNodeMasgn
      include CompileForValueUsingVoid
      def compile_for_void(w, really_for_value = false)

	# VALUE - array - the rhs of the assignment
	# HEAD - array - the lhs individual assignments (except gather)
	# MASGN_ARGS - assignment to the gather arg (if any)
	
        value.compile_for_value(w) if value 

        num_elems = head ? head.length : 0
	gather    = masgn_args
	gather    = nil if gather == :*  # not sure why * gets passed thru
        
        w.ary_scatter(num_elems, !!gather, really_for_value)

        gather.compile_for_void(w) if gather
        head.unroll_for_masgn(w) if head
      end
    end

    ########## Literals ####################################
    class MriNodeLit
      def compile_for_value(w); w.ld_imm(literal); end
    end
    class MriNodeFalse
      def compile_for_value(w); w.ld_imm(false); end
    end
    class MriNodeTrue
      def compile_for_value(w); w.ld_imm(true); end
    end
    class MriNodeNil
      def compile_for_value(w); w.ld_imm(nil); end
    end

    class MriNodeStr
      # FIXME Ruby dups literal strings each time they 
      # are executed.  	
      def compile_for_value(w); w.ld_imm(literal); end
    end

    class MriNodeZarray
      # FIXME real ruby doesn't call Array.[]
      def compile_for_value(w); 
        w.ld_imm(Array).
          call(:[], 0)
      end
    end

    ########### Statements ###################################
    class MriNodeBlock
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

    ############ Variables ###################################
    module LocalAssign
      include CompileForValueUsingVoid
      def compile_for_void(w, really_for_value = false)
        # The value field will be nil for Masgn (multiple assignment) -
        # the value will be waiting for us on the stack in this case
	local_id, depth = w.locals.find_or_make_local(variable_id)

        value.compile_for_value(w) if value
	
	w.dup if really_for_value

	if depth == 0
	  w.st_loc(local_id)
	else
	  w.st_loc_l(local_id, depth)
	end
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

    class MriNodeLasgn
      include LocalAssign
    end

    class MriNodeDasgn
      include LocalAssign
    end

    class MriNodeDasgn_curr
      include LocalAssign
      # Dasgn_curr seems to be assignment to a var in innermost block,
      # so we could actually optimise the compilation code here. It
      # doesn't seem worth it however
    end

    class MriNodeLvar
      include LocalAccess
    end

    class MriNodeDvar
      include LocalAccess
    end

    class MriNodeIvar
      def compile_for_value(w)
	w.ld_ivar(variable_id)
      end
    end

    class MriNodeIasgn
      include CompileForValueUsingVoid
      def compile_for_void(w, really_for_value = false)
        # The value field will be nil for Masgn (multiple assignment) -
        # the value will be waiting for us on the stack in this case
	value.compile_for_value(w) if value
	w.dup if really_for_value
	w.st_ivar(variable_id)
      end 
    end

    class MriNodeConst
      def compile_for_value(w)
        # FIXME this is just a quick hack to allow method testing
        w.ld_imm(Object).
          ld_imm(variable_id).
          call(:const_get, 1)
      end
    end


    #############  Class, method definitions #################
    class MriNodeClass
      # FIXME super
      # FIXME nested classes
      # FIXME create new classes
      def compile_for_void(w)
	# replace current class (self) with contained class
	w.ld_imm(Object).        # FIXME this should work off ruby_top_self
          ld_imm(class_name).
          call(:const_get, 1).
          st_self
        body.compile_for_void(w)
      end
    end
    
    module MethodDefinition
      # Common code for MriNodeDefn and MriNodeDefs
      # Needs #define_method
      attr_reader :normal_args,
                  :optional_args,
                  :rest_arg,
		  :block_arg,
                  :body
 
      def compile_for_void(w)
	decode

        if block_arg
          print("Warning: block arguments to methods are currently ignored!\n")
        end

        method_writer = Bytecode::Writer.new()

        method_writer.locals.register_locals(normal_args)
	method_writer.num_args = normal_args.size

	compile_opt_args(method_writer)

        if rest_arg
	  method_writer.locals.register_locals([rest_arg])
	  method_writer.rest_arg = true
	end

        body.compile_for_return(method_writer)

	define_method(w, method_writer)
      end

      def compile_opt_args(method_writer)
	return unless optional_args

	jump_points = [0]
	optional_args.each_child do |arg, last|
	  unless MriNodeLasgn === arg
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
        args      = scope.next_node.head
        body      = scope.next_node.next_node

        @normal_args   = arg_names[0, args.count]
	@optional_args = args.optional
        @rest_arg      = if args.rest == -1
                           nil
                         else
                           arg_names[args.rest - 2]
                         end

        if body.head.kind_of?(MriNodeBlock_arg)
          @block_arg = body.head.variable_id
          @body      = body.next_node
        else
          @block_arg = nil
          @body      = body
        end

        @decodedp = true 
      end
    end

    class MriNodeDefn
      include MethodDefinition, PushSelf
      def define_method(w, method_writer)
        push_self(w)
        w.defn(method_id, method_writer)
      end
    end

    class MriNodeDefs
      include MethodDefinition
      def define_method(w, method_writer)
        receiver.compile_for_value(w)
        w.defs(method_id, method_writer)
      end
    end

    ######## Control flow #############################
    module NodeCall
      # Common code for MriNodeCall and MriNodeFcall
      # Needs #put_receiver_on_stack
      def compile_for_value(w, &block)
	# temp hack
	if method_id == :raise
	  args.unroll_onto_stack(w)
	  w.raise
	  return
	end

        put_receiver_on_stack(w)
	case args
	when MriNodeArray     # normal args only, no scatter
	  argc = args.unroll_onto_stack(w)
	when NilClass         # no args at all
	  argc = 0
	when MriNodeArgscat   # normal (head) + scatter (body)
	  argc = -(1 + args.head.unroll_onto_stack(w))
	  args.body.compile_for_value(w)
	when MriNodeRestargs  # scatter only
	  argc = -1
	  args.head.compile_for_value(w)
	else
	  raise "Found unknown node when compiling args: #{args.type}"
	end
	w.call(method_id, argc, &block)
      end
    end

    class MriNodeCall
      include NodeCall
      def put_receiver_on_stack(w)
        receiver.compile_for_value(w)
      end
    end

    class MriNodeFcall
      include NodeCall, PushSelf
      alias :put_receiver_on_stack :push_self
    end

    class MriNodeIter
      def compile_for_value(w)
	iter.compile_for_value(w) do |block|
          block.num_args = 1
	  if variable
	    # yield puts args in first local, 
	    # but we want them on the stack
	    block.ld_loc(0) 
	    variable.compile_for_void(block) 
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

    class MriNodeYield
      def compile_for_value(w)
        stts.compile_for_value(w)
        w.yield(1, w.depth)
      end
    end

    class MriNodeReturn
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

    class MriNodeWhile
      def compile_for_void(w)
        w.loops.while_loop do |loop|
          w.label(loop.before_condition)
          
          conditional.compile_for_value(w)
          w.if_not(loop.out)
           
          w.label(loop.after_condition)  # for use by redo

          body.compile_for_void(w) if body

          w.goto(loop.before_condition)

          w.label(loop.out)
        end
      end
    end

    class MriNodeBreak
      def compile_for_void(w)
	w.loops.break
      end
    end

    class MriNodeNext
      def compile_for_void(w)
	w.loops.next
      end
    end

    class MriNodeRedo
      def compile_for_void(w)
	w.loops.redo
      end
    end

    class MriNodeRetry
      def compile_for_void(w)
	# retry only valid inside blocks and rescue clauses,
	# so we don't use current_loop
#	w.retry
      end
    end

    class MriNodeIf
      def common_code(w)
        not_true_lbl = w.labels.new("not_true")
        end_lbl = w.labels.new("end")

        conditional.compile_for_value(w)
	w.if_not(not_true_lbl)

	yield not_true_lbl, end_lbl

        w.label(end_lbl)
      end
      def compile_for_value(w)
	common_code(w) do |not_true_lbl, end_lbl|
	  body.compile_for_value(w)
	  w.goto(end_lbl)
	  
	  w.label(not_true_lbl)
	  zelse ? zelse.compile_for_value(w) : w.ld_imm(nil)
	end
      end
      def compile_for_void(w)
	common_code(w) do |not_true_lbl, end_lbl|
	  body.compile_for_void(w)
	  w.goto(end_lbl) if zelse

	  w.label(not_true_lbl)	
	  zelse.compile_for_void(w) if zelse
	end
      end
    end

    module NodeOrAnd
      # Common code for MriNodeOr and MriNodeAnds
      # Needs #if_type
      def common_code(w)
        out_lbl = w.labels.new("out")
        first.compile_for_value(w)
	yield out_lbl
        w.label(out_lbl)	
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
   
    class MriNodeOr
      include NodeOrAnd
      def if_type; :if; end
    end

    class MriNodeAnd
      include NodeOrAnd
      def if_type; :if_not; end
    end

    class MriNodeNot
      def compile_for_value(w)
        true_lbl = w.labels.new("true")
        out_lbl = w.labels.new("out")
	body.compile_for_value(w)
	w.if(true_lbl).
          ld_imm(true).
          goto(out_lbl).
	  label(true_lbl).
	  ld_imm(false).
	  label(out_lbl)
      end
    end

    class MriNodeCase
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
      include CompileForValueUsingVoid      
      def compile_for_void(w, really_for_value = false)
        out_lbl     = w.labels.new("out")
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
	  when MriNodeWhen
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

        w.label(out_lbl)
      end
    end

    class MriNodeWhen
      def each_child
        node = self
        while MriNodeWhen === node
	  yield node
          node = node.next_node
        end
	yield node
      end

      def compile_for_value(w, out_lbl)
	compile_for_void(w, out_lbl, true)
      end

      def compile_for_void(w, out_lbl, really_for_value = false)
	code_lbl    = w.labels.new("code")
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
	next_when_lbl = w.labels.new("when")
	w.goto(next_when_lbl)  
	
	# come here if we had a match
	w.label(code_lbl)
	w.pop                  # remove test value
	if body	       
	  body.send(compile_msg, w) 
	else
	  # empty when clause body
	  w.ld_imm(nil) if really_for_value
	end
	w.goto(out_lbl)
	
	# label picking up the next when (or else) clause
	w.label(next_when_lbl)
      end
    end

    ######## Exceptions #############################
    class MriNodeRescue
      def compile_for_value(w)
	w.handlers.rescue(resque) do
	  head.compile_for_value(w)
	end
      end
    end

    class MriNodeResbody
      def compile_for_value(w, out_lbl)
	compile_for_void(w, out_lbl, true)
      end
      def compile_for_void(w, out_lbl, really_for_value = false)
	code_lbl = w.labels.new("code")
	compile_msg = if really_for_value
			:compile_for_value
		      else
			:compile_for_void
		      end

	args.each_child do |exception_class|
	  w.dup
	  exception_class.compile_for_value(w)
	  w.swap
	  w.call(:===, 1)
	  w.if(code_lbl)
	end

	# come here if no matches
	next_rescue_lbl = w.labels.new("rescue")
	w.goto(next_rescue_lbl)  
	
	# come here if we had a match
	w.label(code_lbl)
	w.pop                  # remove test value
	if body	       
	  body.send(compile_msg, w) 
	else
	  # empty rescue clause body
	  w.ld_imm(nil) if really_for_value
	end
	w.goto(out_lbl)
	
	# label picking up the next rescue (or else) clause
	w.label(next_rescue_lbl)
	if head
	  head.compile_for_value(w, out_lbl)
	else
	  # don't pop - leave exception on stack for reraising
	  w.rehandle
	end
      end
    end

  end
end


module Bytecode
  def Bytecode.compile_and_run_for_void(ruby_src)

    p = Ruby::Interpreter.parse(ruby_src)
    #print "\n#{p.inspect}\n\n"

    p.compile_for_void(w = Writer.new())
    w.ld_imm(nil).return(0)

    #puts w.to_s

    Runner.run_from_writer(w)
    
  end
  def Bytecode.compile_and_run_for_return(ruby_src)
    p = Ruby::Interpreter.parse(ruby_src)
    #print "\n#{p.inspect}\n\n"

    p.compile_for_return(w = Writer.new())
    w.compile()
    #puts w.to_s

    Runner.run_from_writer(w)
  end
end
