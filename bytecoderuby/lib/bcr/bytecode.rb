#!/usr/bin/ruby

=begin

Instructions and their arguments:

LD_IMM,  value
Loads the Ruby value VALUE on to the stack.

LD_LOC, var
Loads the contents of the local variable given by VAR (a number) on 
to the stack.

LD_LOC_L, var, count
Gets the contents of the local variable given by VAR (a number) from the 
frame COUNT frames back on static chain and pushes it onto the stack. A COUNT
of 0 means the current frame, so LD_LOC_L(1,0) is equivalent to LD_LOC(1).

ST_LOC, var
Pops the top of the stack into the local variable VAR

ST_LOC_L, var, count
Pops the top of the stack and stores it into the local variable VAR in the 
frame COUNT frames back on the static chain.

DUP,     <no args>
Pushes a second copy of the top of the stack on to the stack.

POP,     <no args>
Discards the value on the top of the stack.

POP,     <no args>
Swaps the top two items on the stack.

CALL,    [method_id, num_args, block]
Calls the method given by METHOD_ID. NUM_ARGS+1 values are popped off 
the stack - the first is the receiver of the message, the rest are 
arguments to the method. The value returned by the method is pushed on to
the stack.

RETURN,  count	
Returns to the frame COUNT frames back on the static chain. Raises a 
LocalJumpError if the frame is no longer on the call stack.  

GOTO,    [offset]
Jumps to the bytecode at OFFSET relative to the start of this opcode.

IF,      [offset]
Pops a value off the stack. If it's true (ie neither false nor nil), 
jumps to the bytecode at OFFSET relative to the start of this opcode; 
otherwise continues at the next instruction.

IF_NOT,  [offset]
Pops a value off the stack. If it's false (ie either false or nil), 
jumps to the bytecode at OFFSET relative to the start of this opcode; 
otherwise continues at the next instruction.

YIELD,   [num_args, count]
Pops NUM_ARGS values off the top of the stack and passes them to the block 
belonging to the frame COUNT frames up the static chain. LocalJumpError 
if there frame does not have a block.

DEFN,    [name, code]
Pops the stack, checks that the object is a Class, and then creates a 
method called NAME with bytecode CODE in that class. How CODE is represented
is left to the implementor.

ARY_SCATTER,    [num_elems, gather, push_value]
Used for implementing the lhs of multiple assignments. Pops the stack, 
converting the value into an array if it isn't one already. Yadda, yadda.

BREAK
Rolls the call stack back to the first frame that is also present on the
static chain. Raises a LocalJumpError if there is no such frame on the
call stack. 

LD_IVAR, [var_id]
Pops an object off the stack. Pushes the value of the instance variable 
VAR_ID of the object that was tos.

ST_IVAR, [var_id]
Pops two items off the stack. Stores the item that was one below the top 
of stack into the instance variable VAR_ID of the object that was tos.

DEFS FIXME

LD_SELF
Pushes self on to the stack

RUN,   [code]
Pops the stack and runs CODE with that object as self.
=end

module Bytecode

  class Instruction
    def initialize( *args )
        #print("#{self.class}: #{args.join(' ')}\n")
	if args.size != bc_num_args
          raise "Invalid number of arguments"
	end
        @args = args
    end	
    def to_s
        args = @args.join("\t")
        "#{type.name}\t#{args}"
    end	
  end

  module BranchInstruction
  end

  module LocalInstruction
  end

  # FIXME move these to a tools module?
  def Bytecode.camel_case(name)
    if name.kind_of?(Symbol) then name = name.id2name end
    name.downcase.gsub(/(^|_)(\w)/) do
      $2.upcase
    end
  end

  def Bytecode.uncamel_case(name)
    if name.kind_of?(Symbol) then name = name.id2name end
    name.gsub(/(^[A-Z])|([A-Z])/) do
      $1 || "_" + $2
    end
  end

  @index = 0
  def Bytecode.new_instruction_index
	@index += 1	
  end
	
  def Bytecode.instruction(name, args, *other)    
    instruction_id = new_instruction_index
    include_module = 
	if other[0] 
	    "include " + other[0]
	else
	    ""
        end
 
    arg_access_methods = ""
    args.each_with_index do |arg, i|
	arg_access_methods << <<-EOMETHODDEF
	def #{arg}
		@args[#{i}]
	end
        EOMETHODDEF
    end    	
    class_name = camel_case(name)	
    module_eval <<-EOCLASSDEF
        class #{class_name} < Instruction
	    #{include_module}
            def bc_code
		#{instruction_id}
	    end
            def #{class_name}.bc_code
		#{instruction_id}
	    end
            def #{class_name}.bc_name
                "#{name}"
            end 
	    def bc_num_args
            	#{args.size}
	    end
	    #{arg_access_methods}
        end
    EOCLASSDEF
  end


  instruction 'LD_IMM',      ['value']
  instruction 'LD_SELF',     []
  instruction 'LD_LOC',      ['var'],                   'LocalInstruction' 
  instruction 'LD_LOC_L',    ['var', 'count'],          'LocalInstruction' 
  instruction 'ST_LOC',      ['var'],                   'LocalInstruction'  
  instruction 'ST_LOC_L',    ['var', 'count'],          'LocalInstruction' 
  instruction 'LD_IVAR',     ['var_id']
  instruction 'ST_IVAR',     ['var_id']
  instruction 'LD_GVAR',     ['var_id']


  instruction 'DUP',         []
  instruction 'POP',         []
  instruction 'SWAP',        []
  instruction 'ARY_SCATTER', ['num_elems', 'gather', 'push_value']

  instruction 'GOTO',        ['offset'],                'BranchInstruction'
  instruction 'IF',          ['offset'],                'BranchInstruction'
  instruction 'IF_NOT',      ['offset'],                'BranchInstruction'

  instruction 'CALL',        ['method_id', 'num_args', 'superp', 'block']
  instruction 'RETURN',      ['count']	
  instruction 'YIELD',       ['num_args', 'count']
  instruction 'BREAK',       []
  instruction 'RUN',         ['code']

  instruction 'DEFN',        ['name', 'code']
  instruction 'DEFS',        ['name', 'code']

  instruction 'REHANDLE',    []

  def Bytecode.each_instruction
    constants.each do |name|
      val = const_get(name)
      if val.kind_of?(::Class) and val < Instruction
	yield(val, name)
      end
    end
  end

end
