#!/usr/bin/ruby

require 'Runner'
require 'bytecode_mri'

class Class
  def define_bytecode_method(name, writer, num_args)
    writer.compile
    writer.num_args = num_args
    define_bytecode_method0(name, writer)	
  end
end

module Bytecode


  class Writer
    # clashes with DUP bytecode	
    undef_method :dup

    # The number of arguments expected by the block code held in
    # this Writer
    attr_accessor :num_locals, :num_args, :opt_args_jump_points, :rest_arg
    attr_reader   :parent, :depth, :current_pc

    attr_reader   :locals, :loops, :labels, :handlers

    def initialize(parent = nil)
      @parent      = parent
      @depth       = parent ? parent.depth + 1 : 0

      @code        = []   # holds the bytecode instruction objects
      @current_pc  = 0

      @locals = LocalManager.new(self)
      @loops  = LoopManager.new(self)
      @labels = LabelManager.new(self)
      @handlers = HandlerManager.new(self)
    end

    def to_s
      sep = "\n" + "\t" * depth
#      sep + @code.join(sep)
      result = ""
      pc = 0
      @code.each do |instruction|
	result << sep << pc.to_s << "\t" << instruction.to_s
	pc += instruction.bc_size
      end
      result + "\n" + @handlers.to_s
    end

    def << (code)
      @result << code
    end

    # Labels
    def label(name)
      labels.label(name, @current_pc)
      self
    end

    def add_instruction(instruction)
	@code << instruction
	@current_pc +=instruction.bc_size
	self
    end

    # Instructions requiring special handling
    def call(method_id, num_args)
	if block_given?
	    # allow block or method defn. code to be added
	    code = Writer.new(self)
	    yield code	
	else
	    code = nil
	end
	instruction = Call.new(method_id, num_args, code)
	add_instruction(instruction)
    end

    def local_id_from_name(local_name)
      if local_name.kind_of?(Fixnum)
      then
	local_name
      else
        @locals[local_name] || register_local(local_name)
      end
    end

    private :local_id_from_name

    def ld_loc(local_name)
      add_instruction(LdLoc.new(local_id_from_name(local_name)))
    end

    def st_loc(local_name)
      add_instruction(StLoc.new(local_id_from_name(local_name)))
    end

    # Most instructions come through here
    def method_missing(name, *args)
      klass = Bytecode.const_get(Bytecode.camel_case(name))
      #raise "Unknown bytecode: #{name}" unless klass
      instruction = klass.new(*args)	
      #print "#{instruction.bc_size} #{name}\n"

      add_instruction(instruction)	
    end

    def compile
      return if @result # ie if previously compiled

      @handlers.compile_all()

      @result = []

      @code.each { |bc| bc.compile_into(@result) }

      @result.each_with_index do |val, @current_pc|
	if val.kind_of?(Instruction)
	  patch = val.fixup(self, @current_pc)
	  @result[ @current_pc .. @current_pc + patch.size - 1] = patch
	end
      end

      num_locals = @code.map {|bc| bc.max_local_id}.max
      @num_locals = num_locals ? num_locals + 1 : 0

      @result
    end

  end
end




module Bytecode
  class Writer
    class Manager
      attr_reader :owner, :parent

      def initialize(owner)
	@owner       = owner
	@parent      = owner.parent
      end
    end
  end
end





module Bytecode
  class Writer
    class LocalManager < Manager
      private

      def initialize(owner)
	super

	@locals      = {}   # map from local name to id
	@local_id    = -1   # id of next local added to @locals will be this + 1
      end

      # Locals
      def register_local(local_name)
	@locals[local_name] = (@local_id += 1)
      end

      public

      def register_locals(arg_names)
	# Register a block of locals; used to ensure arguments to methods
	# are in positions 1 .. num_args
	for arg in arg_names
	  register_local(arg)
	end            
      end

      class CantFindArgError < Exception; end

      def find_local(local_name)
	# Returns LOCAL_ID, NUM_FRAMES back
	local_id = @locals[local_name]
	if local_id
	  return local_id, 0
	elsif parent
	  local_id, num_frames = 
	    parent.locals.find_local(local_name)
	  return local_id, num_frames + 1
	else
	  raise CantFindArgError, "Can't find local '#{local_name}'"
	end
      end

      def find_or_make_local(local_name)
	find_local(local_name)
      rescue CantFindArgError
	return register_local(local_name), 0
      end
    end
  end
end




module Bytecode
  class Writer
    class LoopManager < Manager

      def initialize(owner)
	super

	@loop_stack  = []   # tracks loop nesting in the source

	# Add place holder to loop stack if we represent a block.
	# Used by break, next etc to decide how to compile themselves.
	@loop_stack.push(BlockLoop.new(owner)) if parent
      end

      # Loops
      def while_loop
	loop = WhileLoop.new(owner)
	@loop_stack.push(loop)
	begin
	  yield loop
	ensure
	  @loop_stack.pop
	end 
      end

      def current_loop
	@loop_stack[-1]
      end

      def break;      current_loop.break;      end
      def next;       current_loop.next;       end
      def redo;       current_loop.redo;       end
      def redo_label; current_loop.redo_label; end

      class Loop
	def initialize(w); @writer = w; end
      end

      class WhileLoop < Loop
	attr_reader :before_condition, 
	  :after_condition,
	  :out
	
	def initialize(w)
	  super
	  @before_condition = w.labels.new("before_cond")
	  @after_condition  = w.labels.new("after_cond")
	  @out              = w.labels.new("out")
	end
	def break
	  @writer.goto(@out)
	end
	def next
	  @writer.goto(@before_condition)
	end
	def redo
	  @writer.goto(@after_condition)
	end
      end

      class BlockLoop < Loop
	def break
	  @writer.break
	end
	def next
	  @writer.ld_imm(nil).return(0)
	end
	def redo
	  @writer.goto(@redo_label)
	end
	def redo_label
	  @redo_label = @writer.labels.new("redo")
	  @writer.label(@redo_label)
	end
      end

    end
  end
end


module Bytecode
  class Writer
    class LabelManager < Manager

      def initialize(owner)
	super

	@labels      = {}   # map from label to instruction offset
	@used_labels = []   
      end

      def label(name, current_pc)
	@labels[name] = current_pc
      end

      def offset_for_label(name)
	offset = @labels[name] or raise "No such label #{name}"
	return offset - owner.current_pc
      end

      def new(name = "1")
	# Return a new, unique label
	while @used_labels.find {|l| name == l}
	  name = name.succ
        end
	@used_labels.push(name)
	name
      end

    end
  end
end


module Bytecode
  class Writer

    class HandlerManager < Manager
      def initialize(owner)
	super
	@stack    = [] # tracks the nesting structure of rescue clauses
	@handlers = [] # final list of all handlers
      end

      def with_handler_context(handler, &p)
	@stack.push(handler)
	p.call()
	@stack.pop
      end

      def add_handler(handler)
	handler.end() 
	@handlers.push(handler)
      end

      def rescue(handler_code, &p)
	handler = RescueHandler.new(owner, @stack[-1], handler_code)
	with_handler_context(handler, &p)
	add_handler(handler)
      end

      def compile_all()
	# NB we can't use .each because nested handlers
	# might cause @handlers to be added to while we're
	# iterating over it
	i = 0
	while (i < @handlers.size)
	  @handlers[i].compile()
	  i += 1
	end
	# Now we've compiled them all we need to fix up the 
	# handler pcs where necessary.
	@handlers.each do |handler|
	  handler.fixup()
	end
	owner.return(0)    # FIXME dummy to keep the C code check happy
      end
      
      def to_s
	result = ""
	@handlers.each {|handler| result << handler.to_s << "\n"}
	result
      end


      class Handler
        attr_reader :start_pc, :end_pc, :handler_pc
	def initialize(owner, parent_handler)
	  @owner = owner
	  @start_pc = owner.current_pc
	  @parent_handler = parent_handler
	end
	def end()
	  @end_pc = @owner.current_pc
	end
	def compile(); end
	def fixup();   end
	def to_s
	  "#{type.name}: #{start_pc} - #{end_pc} jumping to #{handler_pc}"
	end
      end

      class FixupHandler < Handler
	def fixup()
	  @handler_pc = @parent_handler.handler_pc  
	end
      end

      class RescueHandler < Handler
	def initialize(owner, parent_handler, handler_code)
	  super(owner, parent_handler)
	  @handler_code = handler_code
	  @out_lbl      = owner.labels.new("out")
	end

	def end()
	  super
	  @owner.label(@out_lbl)
	end

	def compile()
	  if @parent_handler
	    int_compile_with_fixup()
	  else
	    int_compile()
	  end
	end
	def int_compile()
	  @handler_pc = @owner.current_pc
	  @handler_code.compile_for_value(@owner, @out_lbl)
	  @owner.goto(@out_lbl)
	end
	def int_compile_with_fixup()
	  # In code like begin; begin; rescue A; end rescue B; end,
	  # the A handler must be protected by the B handler.
	  # However, the A code is compiled after the outer
	  # handler has been removed from the handler stack. The B
	  # handler is therefore recorded in the A handler as its 
	  # @parent_handler. When we compile the A handler, we add
	  # an additional 'fixup' handler to the handler list, which protects
	  # A and points at the B handler. We don't initially
	  # know the handler_pc for the B handler, so we just store
	  # a ref at first (in parent_handler). Later, in the fixup
	  # phase of handler compilation, we can resolve the parent_handler
	  # into a handler_pc reference (see FixupHandler).
	  handler_manager = @owner.handlers
	  h = FixupHandler.new(@owner, @parent_handler)
	  handler_manager.with_handler_context(@parent_handler) do 
	    int_compile()
	  end
	  handler_manager.add_handler(h)
	end
      end
    end


  end
end
