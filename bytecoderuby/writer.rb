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

  class Loop
    def initialize(w); @writer = w; end
  end

  class WhileLoop < Loop
    attr_reader :before_condition, 
                :after_condition,
                :out
    
    def initialize(w)
      super
      @before_condition = w.new_label("before_cond")
      @after_condition  = w.new_label("after_cond")
      @out              = w.new_label("out")
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
      @redo_label = @writer.new_label("redo")
      @writer.label(@redo_label)
    end
  end

  class Writer
    # clashes with DUP bytecode	
    undef_method :dup

    # The number of arguments expected by the block code held in
    # this Writer
    attr_accessor :num_args, :num_locals
    attr_reader   :parent, :depth

    def initialize(parent = nil)
      @parent      = parent
      @depth       = parent ? parent.depth + 1 : 0
#      print("Parent #{@parent} depth #{@depth}\n")

      @code        = []   # holds the bytecode instruction objects
      @current_pc  = 0

      @labels      = {}   # map from label to instruction offset
      @used_labels = []   

      @locals      = {}   # map from local name to id
      @local_id    = 0    # id of next local added to @locals will be this + 1

      @loop_stack  = []   # tracks loop nesting in the source

      # Add place holder to loop stack if we represent a block.
      # Used by break, next etc to decide how to compile themselves.
      @loop_stack.push(BlockLoop.new(self)) if parent
    end

    def to_s
      #print "'ere we go\n"
      sep = "\n" + "\t" * depth
      sep + @code.join(sep)
    end

    def << (code)
      @result << code
    end

    # Labels
    def label(name)
      @labels[name] = current_pc
      self
    end
    
    def offset_for_label(name)
      offset = @labels[name] or raise "No such label #{name}"
      return offset - current_pc
    end

    def new_label(name = "1")
	# Return a new, unique label
        begin
	  raise "blah"
	rescue Exception => dummy
	  bt = dummy.backtrace.join("\n")
	  # FIXME print "NEW LABEL called from\n#{bt}"
	end
	while @used_labels.find {|l| name == l}
	  name = name.succ
        end
	@used_labels.push(name)
	name
    end

    def add_instruction(instruction)
	@code << instruction
	@current_pc +=instruction.bc_size
	self
    end

    # Locals
    def register_local(local_name)
      @locals[local_name] = (@local_id += 1)
    end

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
	  parent.find_local(local_name)
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

    # Loops
    def while_loop
      loop = WhileLoop.new(self)
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

    def current_pc
      @current_pc
    end

    def compile
      @result = []
#      print(self)
#      print("Labels: #{@labels.inspect}\n") 
      @code.each { |bc| bc.compile_into(@result) }
      @result.each_with_index do |val, @current_pc|
	if val.kind_of?(Instruction)
	  patch = val.fixup(self)
	  @result[ @current_pc .. @current_pc + patch.size - 1] = patch
	end
      end

      num_locals = @code.map {|bc| bc.max_local_id}.max
      @num_locals = num_locals ? num_locals + 1 : 0

      @result
    end

  end

end

