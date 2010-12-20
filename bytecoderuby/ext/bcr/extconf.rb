require 'mkmf'

$CFLAGS += ' -Wall'

create_makefile("bcr_runtime")

# add dependencies for building bc_runner.h
File.open("Makefile", "a") do |makefile|
  makefile << %{
bc_runner.h: ../../lib/bcr/bytecode.rb
\t$(RUBY) header_helper.rb 

bc_runner.o: bc_runner.c bc_runner.h ruby_adds.h
  }
end
