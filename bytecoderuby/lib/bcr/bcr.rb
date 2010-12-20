require 'bcr/compile'

module Bytecode
  PRINT = ARGV.include?("-print")

  def Bytecode.compile_and_run_for_void(ruby_src)
    print ruby_src if PRINT
    p = Ruby::Interpreter.parse(ruby_src)
    print "\n#{p.inspect}\n\n" if PRINT

    p.compile_for_void(w = Writer.new())
    w.ld_imm(nil).return(0)
    w.compile()

    puts w.to_s if PRINT

    Runner.run_from_writer(w)
    
  end
  def Bytecode.compile_and_run_for_return(ruby_src)

    p = Ruby::Interpreter.parse(ruby_src)
    print "\n#{p.inspect}\n\n" if PRINT

    p.compile_for_return(w = Writer.new())
    w.compile()
    puts w.to_s if PRINT

    Runner.run_from_writer(w)
  end
end

