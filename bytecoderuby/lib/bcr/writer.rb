require 'bcr/bcr_runtime.so'
require 'bcr/bytecode_mri'

module Bytecode


  class Writer
    # clashes with DUP bytecode	
    undef_method :dup

    # The number of arguments expected by the block code held in
    # this Writer
    attr_accessor :num_locals, :num_args, :opt_args_jump_points, :rest_arg
    attr_reader   :parent, :method_name, :depth, :current_pc

    attr_reader   :locals, :loops, :handlers

    def initialize(parent = nil, method_name = nil)
      @parent      = parent
      @depth       = parent ? parent.depth + 1 : 0
      @method_name = method_name

      @code        = []   # holds the bytecode instruction objects
      @current_pc  = 0

      @honour_nil_asgns_depth = 0

      @locals = LocalManager.new(self)
      @loops  = LoopManager.new(self)
      @handlers = HandlerManager.new(self)
    end

    def new_label
      JumpTarget.new(self)
    end

    def num_opt_args
      return 0 unless opt_args_jump_points
      opt_args_jump_points.size - 1
    end

    def to_s
      compile
      
      sep = "\n" + "\t" * depth

      result = sep + "Codeblock #{@method_name} num_locals=#{@num_locals}"
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

    def add_instruction(instruction)
	@code << instruction
	@current_pc +=instruction.bc_size
	self
    end

    # Instructions requiring special handling
    def call( method_id, num_args, superp=false, 
	      private_okp=false, ampersand_argp=false, block=nil)
      if block
	# allow block or method defn. code to be added
	code = Writer.new(self)
	block.call(code) #yield code	
      else
	code = nil
      end
      instruction = Call.new( method_id, num_args, superp, 
			      private_okp, ampersand_argp, code)
      add_instruction(instruction)
    end

    # Most instructions come through here
    def method_missing(name, *args)
      klass = Bytecode.const_get(Bytecode.camel_case(name))
      instruction = klass.new(*args)	

      add_instruction(instruction)	
    end

    def honouring_nil_asgns
      @honour_nil_asgns_depth += 1
      begin
	yield
      ensure
	@honour_nil_asgns_depth -= 1
      end
    end

    def honour_nil_asgns_p
      @honour_nil_asgns_depth > 0
    end

    def compile
      return if @result # ie if previously compiled

      @result = []

      @code.each { |bc| bc.compile_into(@result) }

      # FIXME this actually uses 1 more local than necessary in many
      # cases. The +1 is necessary for blocks, where all arguments are
      # passed as a vector local 0, but this local is unnamed and so
      # isn't known about by @locals
      @num_locals = @locals.num_locals + 1
      
      @num_jump_data = @handlers.max_storage + 1

      @result
    end

    class JumpTarget
      attr_reader :target
      def initialize(writer)
	@writer = writer
      end
      def place
	@target = @writer.current_pc
      end
    end

  end
end




module Bytecode
  class Writer
    class Manager
      attr_reader :writer, :parent

      def initialize(writer)
	@writer       = writer
	@parent      = writer.parent
      end
    end
  end
end





module Bytecode
  class Writer
    class LocalManager < Manager
      private

      def initialize(writer)
	super

	@locals      = {}   # map from local name to id
	@local_id    = -1   # id of next local added to @locals will be this + 1
      end

      public

      # Locals
      def register_local(local_name)
	if @create_in_parent
	  # create in parent
	  id, depth = parent.locals.register_local(local_name)
	  return id, depth + 1
	else
	  # create in this frame
	  id = (@local_id += 1)
	  @locals[local_name] = id
	  return id, 0
	end
      end

      def register_locals(arg_names)
	# Register a block of locals; used to ensure arguments to methods
	# are in positions 1 .. num_args
	for arg in arg_names
	  register_local(arg)
	end            
      end

      def create_in_parent=(bool)
	@create_in_parent = bool
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
	return register_local(local_name)
      end

      def store_to_local_called(local_name)
	local_id, depth = find_or_make_local(local_name)
	if depth == 0
	  @writer.st_loc(local_id)
	else
	  @writer.st_loc_l(local_id, depth)
	end
      end

      def num_locals
	@locals.size
      end
    end
  end
end




module Bytecode
  class Writer
    class LoopManager < Manager

      def initialize(writer)
	super

	@loop_stack  = []   # tracks loop nesting in the source

	# Add place holder to loop stack if our writer represents a block.
	# Used by break, next etc to decide how to compile themselves.
	@loop_stack.push(BlockLoop.new(writer)) if parent
      end

      # Loops
      def while_loop
	loop = WhileLoop.new(writer)
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
	  @before_condition = w.new_label 
	  @after_condition  = w.new_label 
	  @out              = w.new_label 
	end
	def break
	  # pop to get rid of break value, which we can't handle yet
	  @writer.pop.goto_e(@out)
	end
	def next
	  @writer.goto_e(@before_condition)
	end
	def redo
	  @writer.goto_e(@after_condition)
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
	  @writer.goto_e(@redo_label)
	end
	def redo_label
	  @redo_label = @writer.new_label 
	  @redo_label.place
	end
      end

    end
  end
end


module Bytecode
  class Writer

    class HandlerManager < Manager
      attr_accessor :storage
      attr_reader   :max_storage
      def initialize(writer)
	super
	@handlers = [] # final list of all handlers
	@storage = -1
	@max_storage = 0
      end

      def setup_storage
	@storage += 1
	@max_storage = @storage if @storage > @max_storage
	yield @storage
	@storage -= 1
      end

      def with_handler(handler_class)
	handler = nil 
	setup_storage do |storage|
	  handler = handler_class.new(writer, storage)
	  yield handler
	  handler.end()
	end

	@handlers.push(handler)

	return handler
      end

      def with_rescue_handler(&p)
	with_handler(RescueHandler, &p)
      end

      def with_ensure_handler(&p)
	with_handler(EnsureHandler, &p) 
      end

      def to_s
	result = ""
	@handlers.each {|handler| result << handler.to_s << "\n"}
	result
      end

      class Handler
	# must match declarations in C
	HANDLER_TYPE_RESCUE = 1
	HANDLER_TYPE_ENSURE = 2
        attr_reader :start_pc, :end_pc, :handler_pc, :storage
	def initialize(writer, storage, handler_type)
	  @writer   = writer
	  @handlers = writer.handlers
	  @storage  = storage
	  @handler_type = handler_type

	  @writer.hbody_enter(@storage)
	  @start_pc = @writer.current_pc
	end
	def end()
	  @end_pc = @writer.current_pc
	end

	def in_handler(&p)
	  raise 'Handler already provided.' if @handler_pc

	  @handler_pc = @writer.current_pc

	  @handlers.setup_storage(&p) 
	end

	def to_s
	  "#{self.class.name}: #{start_pc} - #{end_pc} jumping to #{handler_pc}"
	end
      end

      class RescueHandler < Handler
	def initialize(writer, storage)
	  super(writer, storage, HANDLER_TYPE_RESCUE)
	end
      end

      class EnsureHandler < Handler
	def initialize(writer, storage)
	  super(writer, storage, HANDLER_TYPE_ENSURE)
	end
      end
    end


  end
end

