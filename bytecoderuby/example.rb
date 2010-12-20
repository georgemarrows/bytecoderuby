#!/usr/bin/ruby

require 'writer'

##############################################################
# Sums of integers (iterative)
##############################################################

# 1. Create our bytecode writer

code = Bytecode::Writer.new()

# 2. Add the code to it. See bytecode.rb for the meaning of the bytecodes
# Note you can use names for labels (targets of gotos).
# The code is equivalent to:
#   sum = 0
#   i   = 1
#   while (i <= self)
#     sum += i
#     i   += 1
#   end

code.
		ld_imm(0).
		st_loc(2).        # 0 -> sum
                ld_imm(1).
		st_loc(1).        # 1 -> i

label(:loop).	ld_loc(1).
	        ld_loc(0).	  # local 0 is self
                call(:<=, 1).	  # i <= self
                if_not(:end).
	
		ld_loc(2).
		ld_loc(1).
		call(:+, 1).
		st_loc(2).        # sum + i -> sum
		
		ld_loc(1).
                ld_imm(1).
                call(:+, 1).      
		st_loc(1).        # i+1 -> i

                goto(:loop).

label(:end).    ld_loc(2).
		return(0) 	          # return prod

# 3. Define a new method on for instances of the Integer class.
# The arguments are 
# - method name	-- must be a symbol
# - the bytecode writer containing our code
# - the number of arguments to the method (excluding self). 

Integer.define_bytecode_method(:sum_to_iter, code, 0)

# 4. Test it!

puts "\nSums of numbers 1 to 10 (iterative method)"
(1..10).each { |i| print "#{i}\t#{i.sum_to_iter}\n" }




##############################################################
# Sums of integers (recursive)
##############################################################

code = Bytecode::Writer.new()

=begin
Equivalent Ruby is:

class Integer
  def sum_to_recursive
    if self == 0
      return 0
    else
      return (self-1).sum_to_recursive + self
    end
  end
end
=end

code.
		ld_loc(0).
		ld_imm(0).
		call(:==, 1).      # self == 0
		if_not(:recurse).

		ld_imm(0).
		return(0).         # return 0

label(:recurse).ld_loc(0).
		ld_imm(1).
		call(:-, 1).       # self - 1

		call(:sum_to_recursive, 0).

		ld_loc(0).
		call(:+, 1).
		return(0) 

# 3. Define a new method on for instances of the Integer class.

Integer.define_bytecode_method(:sum_to_recursive, code, 0)

# 4. Test it!

puts "\nSums of numbers 1 to 10 (recursive method)"
(1..10).each { |i| print "#{i}\t#{i.sum_to_recursive}\n" }



##############################################################
# Blocks
##############################################################

# 1. Here's an example showing how to define and call blocks
#code = Bytecode::Writer.new()

#code.
#	ld_imm("huzzah!").
#	yield(1).
#	return
	
#Object.define_bytecode_method(:yielder, code, 0, 1)

#"abc".yielder

#code = Bytecode::Writer.new()

#code.
#	ld_loc(0).
#	call(:yielder, 0) do |block|
#		block.num_args = 1
#		block.	ld_loc(0).
#			ld_loc(0).
#			call(:print, 1).
#			return(0)
#	end.
#	return

#Object.define_bytecode_method(:iter_caller, code, 0, 1)

#"abc".iter_caller()


##############################################################
# Range.each (assumes Range includes end value)
##############################################################

# 1. Iterator example
# class Range
#   def each2
#     v = begin
#     e = end
#     while v <= e
#       yield(v)
#       break if v == e  # needed for string ranges eg ('a'..'z')
#       v = v.succ
#     end
#   end
# end

code = Bytecode::Writer.new()

# Locals
# 0	self
# 1	v
# 2	e
code.	ld_loc(0).
	call(:begin, 0).
	st_loc(1).		# v = begin

	ld_loc(0).
	call(:end, 0).
	st_loc(2).		# e = end

label(:loop).
	ld_loc(1).
	ld_loc(2).
	call(:<=, 1).		# v <= e
	if_not(:end).

	ld_loc(1).
	yield(1).		# yield v
	pop.			# ignore result of yield

	ld_loc(1).		
	ld_loc(2).
	call(:==, 1).           # v == e
	if(:end).      		         

	ld_loc(1).
	call(:succ, 0).
	st_loc(1).		# v = v.succ

	goto(:loop).

label(:end).
	return(0)

Range.define_bytecode_method(:each2, code, 0)



##############################################################
# Printing all elements of a range
##############################################################

# class Range
#   def print_all
#     self.each2 { |v| v.p v }
#   end
# end

code = Bytecode::Writer.new()

code.
	ld_loc(0).
	call(:each2, 0) do |block|
		block.num_args = 1
		block.	ld_loc(0).
			ld_loc(0).
			call(:print, 1).
			return(0)
	end.
	ld_imm(nil).
	return(0)

Range.define_bytecode_method(:print_all, code, 0)

puts "\nBlock tests: print each element of the ranges (1..10) and ('a' .. 'z')"
(1..10).print_all
('a'..'z').print_all



##############################################################
# Sum of integers (using blocks)
##############################################################

# sum = 0
# (1..self).each2 {|i| sum = sum + i}
# sum
#
# 0 self
# 1 sum
# 0 i

code = Bytecode::Writer.new()

code.
	ld_imm(0).
	st_loc(1).          # 0 -> sum

	ld_imm(Range).
	ld_imm(1).
	ld_loc(0).
	call(:new, 2).	    # Range.new(1,self)

	call(:each2, 0) do |block|
		block.num_args = 1

		block.	ld_loc_l(1,1).  # sum
			ld_loc(0).      # i
			call(:+, 1).    # sum + i
			st_loc_l(1,1).  # -> sum

			ld_imm(nil).    # return nothing
			return(0)
	end.

	ld_loc(1).	    # sum
	return(0)

Integer.define_bytecode_method(:sum_to_block, code, 0)

puts "\n\nSums of numbers 1 to 10 (block method)"
(1..10).each { |i| print "#{i}\t#{i.sum_to_block}\n" }



##############################################################
# Passing arguments to methods
##############################################################

# 1. Another example. This one's simply a synonym for String#[], 
# but it shows how to pass arguments.

code = Bytecode::Writer.new()

# 2. 

code.
		ld_loc(0).
		ld_loc(1).
		call(:[], 1).
		return(0)

# 3. Define a new method on for instances of the String class. 
#    NB - 1 argument this time.

String.define_bytecode_method(:at, code, 1)

# 4. Test it!


puts "\nString tests"
puts "abcdef".at(3)       # => 100, ASCII for d
puts "abcdef".at(/b.*e/)  # => bcde, the match for the regex


##############################################################
# Tests and benchmarks
##############################################################

class Integer
  def sum_to_iterr
    i, sum = 1, 0
    while i <= self
      sum += i
      i += 1	
    end
    sum 	
  end
  def sum_to_recursiver
    if self == 0
      return 0
    else
      return (self-1).sum_to_recursive + self
    end
  end
  def sum_to_blockr
    sum = 0
    (1..self).each3 {|i| sum = sum + i}
    sum
  end
  def sum_to_asap
    sum = 0
    (1..self).each { |i| sum += i }		
    sum
  end
end



class Range
  def each3
    v = self.begin
    e = self.end
    while v <= e
      yield(v)
      break if v == e  # needed for string ranges eg ('a'..'z')
      v = v.succ
    end
  end
end

puts "\nSum tests"

def test_sum(text, method)
   	print "#{50000.send(method)}\tfrom #{text}\n"
end


test_sum("Bytecode iter",      :sum_to_iter)
test_sum("Bytecode recursive", :sum_to_recursive)
test_sum("Bytecode block",     :sum_to_block)

test_sum("Standard iter",      :sum_to_iterr)
test_sum("Standard recursive", :sum_to_recursiver)
test_sum("Standard block",     :sum_to_blockr)

test_sum("Standard asap",      :sum_to_asap)

if ARGV[0]

	# Some quick benchmarks
	def benchmark( label, times, &block )
	  GC.start
	  begtime = Time.times.utime
	  times.times( &block )
	  endtime = Time.times.utime
	  puts "#{label} #{endtime - begtime} sec"
	end

	puts "\nSimple benchmarks"

	benchmark( "Bytecode iter     ", 20) { 50000.sum_to_iter }
	benchmark( "Bytecode recursive", 20) { 50000.sum_to_recursive }
	benchmark( "Bytecode block    ", 20) { 50000.sum_to_block }

	benchmark( "Standard iter     ", 20) { 50000.sum_to_iterr }
	benchmark( "Standard recursive", 20) { 50000.sum_to_recursiver }
	benchmark( "Standard block    ", 20) { 50000.sum_to_blockr }

	benchmark( "Standard asap     ", 20) { 50000.sum_to_asap }
end

