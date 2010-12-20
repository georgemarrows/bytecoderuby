print "\n\n**** Sums ****\n"

Bytecode.compile_and_run_for_void <<-EORUBY
class Integer
  def sum_to(last)
    sum = 0
    i   = self
    while (i <= last)
      sum += i
      i   += 1
    end
    sum
  end
end
p 10.sum_to(15)
EORUBY


print "\n\n**** Nested loops ****\n"

Bytecode.compile_and_run_for_void <<-EORUBY
class Integer
  def nested_loops
    i = 1
    while (i <= self)
      j = 1
      while (j <= i)
        print(i, "\t", j, "\n")
	if i + j >= 5 
          return
        elsif i + j >= 4
          print("blah")
        end
	j += 1
      end
      i += 1
    end
  end
  4.nested_loops
end
EORUBY

