require 'bcr/bytecode.rb'

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
      @args.each { |arg| code_vector << arg }
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
    def to_s
      "#{self.class.name}\t#{offset.target}"
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

