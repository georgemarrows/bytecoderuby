require 'bytecode.rb'

=begin
Various additions to the simple classes defined in bytecode.rb 
so that they can be compiled for the C bytecode interpreter in
bc_runner.c. 'MRI' = 'Matz Ruby interpreter' - the idea is that
another Ruby VM could use the same basic bytecode defined in 
bytecode.rb but add methods specific to its requirements.
=end

module Bytecode

  # Additions to the basic instruction class
  class Instruction
    def compile_into(code_vector)
      code_vector << bc_code
      @args.each { |arg| code_vector << (arg || 0) }
    end
    def bc_size
      bc_num_args + 1
    end
    def fixup
      # most instructions don't need to perform jump fixups
    end
    def max_local_id
      # The local with highest id this instruction refers to.
      # Most instructions don't refer to locals, so this makes a good default.
      -1
    end
  end
  
  def Instruction.output_c_defines(file)
    file.puts("#define #{bc_name} #{bc_code}")
  end


  # Additions/changes to other instructions
  module BranchInstruction
    def compile_into(code_vector)
      unless offset.kind_of?(Fixnum)
        code_vector << self << :dummy
      else
        code_vector << bc_code << offset
      end
    end
    def fixup(code_vector, current_pc)
      relative_tgt = code_vector.labels.offset_for_label(offset)
      @absolute_tgt = current_pc + relative_tgt
      return [bc_code, relative_tgt]
    end
    def to_s
      "#{type.name}\t#{@absolute_tgt}"
    end	    
  end

  module LocalInstruction
    def max_local_id
      var
    end
  end

  class LdImm 
    def compile_into(code_vector)
      code_vector << bc_code << [value]
    end
  end

  class AryScatter
    def compile_into(code_vector)
      # collapse gather & push_value into one C int
      g = gather     ? 1 : 0
      p = push_value ? 1 : 0
      flags = (g << 1) | p
      code_vector << bc_code << num_elems << flags
    end
    def bc_size
      3  # because of the compile_into jiggery-pokery
    end
  end

  # C header
  def Bytecode.make_c_header(fname)
    File.open(fname, "w") do |file|
      each_instruction do |instruction, name| 
	instruction.output_c_defines(file)
      end
    end
  end

end

if $0 == __FILE__
  Bytecode.make_c_header("bc_runner.h")
end
