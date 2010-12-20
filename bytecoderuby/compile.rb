require 'ruth/mri'
require 'writer'

module Ruby
  module Interpreter

    class CompilationError < Exception; end;

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
    class MriNodeNewline
      def compile_for_value(w);  next_node.compile_for_value(w);  end
      def compile_for_void(w);   next_node.compile_for_void(w);   end
      def compile_for_return(w); next_node.compile_for_return(w); end
    end

    module PushSelf
      def push_self(w)
	depth = w.depth
	if depth == 0
	  w.ld_loc(0)
	else
	  w.ld_loc_l(0, depth)
	end
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
      def compile_for_void(w, really_for_value = false)
        value.compile_for_value(w) if value 

        num_elems = head ? head.length : 0
        gather    = !!masgn_args
        
        w.ary_scatter(num_elems, gather, really_for_value)

        masgn_args.compile_for_void(w) if masgn_args
        head.unroll_for_masgn(w) if head
      end
      def compile_for_value(w)
        compile_for_void(w, true)
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
      def compile_for_value(w)
        compile_for_void(w, true)
      end 
      def compile_for_void(w, really_for_value = false)
        # The value field will be nil for Masgn (multiple assignment) -
        # the value will be waiting for us on the stack in this case
	local_id, depth = w.find_or_make_local(variable_id)

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
	local_id, depth = w.find_local(variable_id)
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


    class MriNodeConst
      def compile_for_value(w)
        # FIXME this is just a quick hack to allow method testing
        w.ld_imm(Object).
          ld_imm(variable_id).
          call(:const_get, 1)
      end
    end


    #############  Class, method definitions #################
     class MriNodeScope
       # FIXME this gives local vars used in class defs	
       def compile_for_void(w)
         next_node.compile_for_void(w)
       end
     end

    class MriNodeClass
      # FIXME super
      # FIXME nested classes
      # FIXME create new classes
      def compile_for_void(w)
	# replace current class (self) with contained class
	w.ld_imm(Object).        # FIXME this should work off ruby_top_self
          ld_imm(class_name).
          call(:const_get, 1).
          st_loc(0)
        body.compile_for_void(w)
      end
    end
    
    class MriNodeDefn
      attr_reader :normal_args,
                  :optional_args,
                  :rest_arg,
		  :block_arg,
                  :body
 
      def compile_for_void(w)
	decode

        if optional_args or rest_arg or block_arg
          print("Warning: 'odd' arguments to methods are currently ignored!\n")
        end

        method_writer = Bytecode::Writer.new()

        method_writer.register_locals(normal_args)
        method_writer.num_args = normal_args.size # FIXME and the rest

        body.compile_for_return(method_writer)

        w.ld_loc(0).                        # Get hold of self
          defn(method_id, method_writer)
      end

      def decode
	# Decode the MRI representation of arguments into something
        # a bit more understandable.
        return if @decodedp

	scope     = defn
        arg_names = scope.table
        args      = scope.next_node.head
        body      = scope.next_node.next_node

        @normal_args   = arg_names[0, args.count]
        @optional_args = args.optional  # FIXME names?
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

    ######## Control flow #############################
    module NodeCall
      # Common code for MriNodeCall and MriNodeFcall
      # Needs #put_receiver_on_stack
      def compile_for_value(w, &block)
        put_receiver_on_stack(w)
	argc = args ? args.unroll_onto_stack(w) : 0
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
	  block.current_loop.redo_label() # set the target for any redo's
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
        w.while_loop do |loop|
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
	w.current_loop.break
      end
    end

    class MriNodeNext
      def compile_for_void(w)
	w.current_loop.next
      end
    end

    class MriNodeRedo
      def compile_for_void(w)
	w.current_loop.redo
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
        not_true_lbl = w.new_label("not_true")
        end_lbl = w.new_label("end")

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
        out_lbl = w.new_label("out")
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
    
    #puts w.to_s

    Runner.run_from_writer(w)
  end
end
