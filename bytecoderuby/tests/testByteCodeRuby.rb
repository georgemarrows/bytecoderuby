$: << File.dirname($0) << File.join(File.dirname($0), "..")
require 'runit/testcase'
require 'runit/testsuite'
require 'runit/cui/testrunner'

require 'compile'

# FIXME necessary until we get GC integrated properly
GC.disable

# Methods
src = <<-EORUBY
  def single_result
    1
  end
  def multiple_results
    return 1, 2, 3
  end
  def multiple_results_star
    z = [2,3]; return 1, *z
  end
  def args(a, b, c)         
    return [a, b, c]
  end
  def yield_void
    i = 1; while i <= 4; yield i; i += 1; end
  end
  def yield_value(a)                 
    i = 1
    while i <= 4
      a << (yield i)
      i += 1
    end
    a
  end
  def yield_yield
    yield_void do |x| yield x; end
  end
  def return_test
    yield_void do |x| 
      yield_void do |y| 
	if x + y == 6 then return :a; end
      end
    end
    return :b
  end
  def retry_helper(a)
    a << :a
    self
  end
EORUBY

# Native Ruby class
mri_src = "class MRITestClass; #{src}; end"
eval(mri_src)

# BCR class
# create the class 'cos BCR can't do it for itself currently
class BCRTestClass; end; 
bcr_src = "class BCRTestClass; #{src}; end"
Bytecode.compile_and_run_for_void(bcr_src)


class TestByteCodeRuby < RUNIT::TestCase

  def value_of(src)
    Bytecode.compile_and_run_for_return(src)
  end

  benchmark = ARGV[0] == "-bench"

  if benchmark
    module Benchmarking
      def time( times, &block )
	GC.start
	begtime = Time.times.utime
	times.times( &block )
	endtime = Time.times.utime
	endtime - begtime
      end
      def benchmark(mri_src, bcr_src)
	mri = time(1) { eval(mri_src) }
	bcr = time(1) { Bytecode.compile_and_run_for_return(bcr_src) }
	return mri, bcr
      end
      def loopify(src)
	%{ cnt = 0; while cnt < 50000; #{src}; cnt += 1; end }
      end
    end
    include Benchmarking; extend Benchmarking
    # Used to factor out the cost of the loop itself  
    MRI_LOOP, BCR_LOOP = benchmark(src = loopify(""), src)
    def results(mri_src, bcr_src, src)
      mri, bcr = benchmark(mri_src, bcr_src)
      mri, bcr = mri - MRI_LOOP, bcr - BCR_LOOP
      print "\n#{src}\nMRI\t#{mri}\nBCR\t#{bcr}\n%age\t#{100*bcr/mri}\n"
    end
    def compare(src)
      loop_src = loopify(src)
      results(loop_src, loop_src, src)
    end
    def compare_method(src)
      # ensure MRI and BCR return the same result when 
      # the Ruby method call SRC is run
      loop_src = loopify(src)
      mri_loop_src = "obj = MRITestClass.new; #{loop_src}"
      bcr_loop_src = "obj = BCRTestClass.new; #{loop_src}"
      results(mri_loop_src, bcr_loop_src, src)
    end
  else
    def compare(src)
      # ensure MRI and BCR return the same result when 
      # Ruby code SRC is run
      assert_equal( eval(src), v = value_of(src) )
      #puts "'#{src}' has value #{v.inspect}"
    end
    def compare_method(src)
      # ensure MRI and BCR return the same result when 
      # the Ruby method call SRC is run
      mri_val = eval     "obj = MRITestClass.new; #{src}"

#      print "Value is #{mri_val.inspect}\n"
      bcr_val = value_of "obj = BCRTestClass.new; #{src}"


      assert_equal( mri_val, bcr_val )
    end
  end


  def compare_method_exception(src)
    # ensure MRI and BCR raise the same exception when
    # the Ruby method call SRC is run
    begin
      mri_val = eval "obj = MRITestClass.new; #{src}"
    rescue Exception => mri_exception
    end
    begin
      bcr_val = value_of "obj = BCRTestClass.new; #{src}"
    rescue Exception => bcr_exception
    end
    #print "Value is #{mri_exception.inspect}\n"
    assert(mri_exception != nil)
    assert_equal( mri_exception.class,   bcr_exception.class)
    # Won't worry too much about message at the moment
#    assert_equal( mri_exception.message, bcr_exception.message )
  end

  def testLiterals
    compare '"string"'
    compare ':symbol'
    compare '0'
    compare '12.3'
    compare '1_000_000'
    compare '1e6'
    compare 'true'
    compare 'false'
    compare 'nil'
    compare '/regex/'
    compare '1+2'
    compare '[]'
    compare '[1,2,3]'
  end

  def testLiteralCopies
    code = <<-EORUBY
      arr = []
      i = 0
      while i < 2; arr.push(OBJ); i+=1; end
      arr[0][-1] = "d"
      arr
    EORUBY

    # FIXME add a string (fails currently)
    ['[1,2,3]'].each do |obj|
      compare code.sub(/OBJ/, obj)
    end
  end

  def testCalls
    compare '1+2'
    compare '"abc".size'
    # FIXME need more here!
  end

  def testIf
    # tests compile for value
    compare 'if true  then 1 end'
    compare 'if false then 1 end'
    compare 'if true  then 1 else 2 end'
    compare 'if false then 1 else 2 end'
    compare 'if false then 1 elsif true  then 2 end'
    compare 'if false then 1 elsif false then 2 end'
    compare 'if false then 1 elsif true  then 2 else 3 end'
    compare 'if false then 1 elsif false then 2 else 3 end'

    # tests compile for void
    compare 'i=0; if true  then i=1 end; i'
    compare 'i=0; if false then i=1 end; i'
    compare 'i=0; if true  then i=1 else i=2 end; i'
    compare 'i=0; if false then i=1 else i=2 end; i'
    compare 'i=0; if false then i=1 elsif true  then i=2 end; i'
    compare 'i=0; if false then i=1 elsif false then i=2 end; i'
    compare 'i=0; if false then i=1 elsif true  then i=2 else i=3 end; i'
    compare 'i=0; if false then i=1 elsif false then i=2 else i=3 end; i'

  end

  def testAssignment
    compare 'i=123'
    compare 'i=123; i'
    compare 'i=123; i += 2'
    compare 'i=j=123'
    compare 'i=j=123; i'
    compare 'i=j=123; j'
  end

  def testWhile
    compare <<-EORUBY              # Basic loop
      i=0 
      while i <= 3 
        i += 1
      end
      i
    EORUBY
    compare <<-EORUBY              # Nested loops
      i = j = sum = 0.0
      while i <= 2; i += 1
        while j <= 3; j += 1
          sum += i/j
        end
      end
      sum 
    EORUBY
    compare <<-EORUBY               # Empty body
      arr = [1,2,3]
      while arr.shift == 1; end
      arr
    EORUBY
    compare <<-EORUBY               # As modifier
      arr = [1,2,3]
      arr.shift while arr.size > 1
      arr
    EORUBY
  end

  def testWhileJumps
    # Tests break, next, redo. We use the same test
    # harness for each of them.
    actions = ['break', 'next', 'redo']

    simple_loop = <<-EORUBY
      arr = [1,2,3,4,5]; res = []
      while (el = arr.shift)
        if el == 3 then el = 103; ACTION; end
	res.push(el)
      end
      [arr, res]
    EORUBY
    actions.each do |action|
       compare simple_loop.sub(/ACTION/, action)
    end

    nested_loops = <<-EORUBY
      arr1 = [1,2,3,4,5] 
      res  = []
      while (el1 = arr1.shift)
        if el1 == 2 then el1 = 102; ACTION1; end
        arr2 = arr1.dup
	while (el2 = arr2.shift)
          if el2 == 4 then el2 = 104; ACTION2; end
          res.push(el2)
        end
	res.push(el1)
      end
      [arr1, res]
    EORUBY
    actions.each do |action1|
      actions.each do |action2|
       compare nested_loops.sub(/ACTION1/, action1).
                            sub(/ACTION2/, action2)
      end
    end

  end

  def testOrAnd
    compare '1     || 2'
    compare 'true  || 2'
    compare 'nil   || 2'
    compare 'false || 2'

    # test short-circuiting - second assignment might not be executed
    # 2* is a hack to avoid '= not == in conditional' warning
    compare 'i = 1; true  || (2*(i=2)); i'   # => 1
    compare 'i = 1; false || (2*(i=2)); i'   # => 2

    compare '1     && 2'
    compare 'true  && 2'
    compare 'nil   && 2'
    compare 'false && 2'
    
    compare 'i = 1; true  && (2*(i=2)); i'   # => 2
    compare 'i = 1; false && (2*(i=2)); i'   # => 1
  end

  def testMultipleAssignment
    compare 'b = 1; c = 2; d = 3; a = b, c, d; a'
    compare 'b = 1; c = 2; d = 3; a = b, c, d'
    
    compare 'b = 1; c = 2; d = [3, 4]; a = b, c, *d; a'
    compare 'b = 1; c = 2; d = [3, 4]; a = b, c, *d'

    # the [a], [b] .. ] trick lets us make sure that each of
    # a, b etc have the same value as in MRI
    compare 'a, b, c = [1, 2, 3]; [[a], [b], [c]]'
    compare 'a, b, c = [1, 2, 3]'

    compare 'a, b, c = [1]; [[a], [b], [c]]'
    compare 'a, b, c = [1]'

    compare 'a, b, *c  = [1, 2, 3, 4]; [[a], [b], [c]]'
    compare 'a, b, *c  = [1, 2, 3, 4]'

    compare 'a, b, *c  = [1]; [[a], [b], [c]]'
    compare 'a, b, *c  = [1]'

    compare 'a, b, c = "12345"; [[a], [b], [c]]'
    compare 'a, b, c = "12345"'

    compare 'a, = 1, 2; a'
    compare 'a, = 1, 2'

    compare 'a, (b, c), d = 1, [2, 3], 4; [[a], [b], [c], [d]]'
    compare 'a, (b, c), d = 1, [2, 3], 4'

    compare '*a = 4; a'
    compare '*a = 4'

    compare '*a = nil; a'
    compare '*a = nil'

    # FIXME bloody restargs
    #compare 'a = *[*[1]]; a'
    #compare 'a = *[*[1]]'

  end


  def testMethods
    compare_method 'obj.single_result'    

    compare_method 'obj.multiple_results'    
    compare_method 'x,  y = obj.multiple_results; [[x], [y]]'    
    compare_method 'x, *y = obj.multiple_results; [[x], [y]]' 

    compare_method 'obj.multiple_results_star'
    
    compare_method 'obj.args(1, 2, 3)'
    # Argscat  compare_method 'obj.args(1, *[2, 3])'
    # Argscat  compare_method 'z = [2, 3]; obj.args(1, *z)'
    # Restargs compare_method 'obj.args(*[1, 2])'

    # calling C methods with too many / too few args
    compare_method_exception '"abc".tr("a", "b", "c")'
    compare_method_exception '"abc".tr("a")'

    # calling Ruby methods with too many / too few args
    compare_method_exception 'obj.args(1, 2, 3, 4)'
    compare_method_exception 'obj.args(1, 2)'

    # call yielding method without passing in a block
    compare_method_exception 'obj.yield'
  end

  def testBlocks
    # - yield for value, yield for void
    # * yield args: none, one, many
    # * arguments: none, multiple, gather, scatter
    # - access & assign to vars from enclosing scope
    # - nested blocks

    compare_method 'a=[]; obj.yield_void {     a << 9 }; a'    
    compare_method 'a=[]; obj.yield_void { |x| a << x }; a'
    compare_method 'a=[]; obj.yield_void { |x| a =  x }; a'
    compare_method 'a=[]; obj.yield_void { |a|        }; a'
    compare_method 'a=[]; obj.yield_void {            }; a'

    # FIXME should raise NameError compare_method 'a=[]; obj.yield_void { |x| a =  x }; x'

    compare_method 'obj.yield_value([]) {     9     }'
    compare_method 'obj.yield_value([]) { |x| x + 1 }'
    compare_method 'obj.yield_value([]) { |x|       }'
    compare_method 'obj.yield_value([]) {           }'

    compare_method 'a=[]; obj.yield_yield { |x| a << x }; a'

    # nested blocks and access/assignment to vars created in a block
    compare_method <<-EORUBY
      a = []
      obj.yield_value([]) do |x|
        b = a.size + x          # void assign to b, access to a
        obj.yield_value([]) do |y|
	  c = b + y             # access to b, void assign to c
          a << [x, y, b = c, c] # access to a, val assign to b, access to c
	end
      end
      a
    EORUBY

    compare_method <<-EORUBY
      a = []
      obj.yield_value([]) do |x|
        b = x
        obj.yield_value([]) do |y|
	  b = b + y            # access to b, void assign to b
	  a = [x, y, c = b, c] # assign to a, val assign to c, access to c
	end
      end
      a
    EORUBY
  end

  def testBlockJumps
    # * break, next, redo, retry, return
    actions = ['', 'next', 'redo', 'break']

    simple_loop = <<-EORUBY
      a = []; b = []
      obj.yield_void do |x| 
        a << x
        if x==2 then x=9; ACTION end
        b << x
      end
      [a, b]
    EORUBY
    actions.each do |action|
       compare_method simple_loop.sub(/ACTION/, action)
    end

    # tests value returned by next to yield call
    compare_method <<-EORUBY
      obj.yield_value([]) {|x| if x==2 then next else x end }
    EORUBY

    compare_method <<-EORUBY
      obj.return_test
    EORUBY
  end

=begin
  def testBlockRetry
    #retry is hard - it reevaluates its sender :-(
    compare_method <<-EORUBY
      a = []
      obj.retry_helper(a).yield_void do |x|
        a << x
        if a.size < 4 then retry end
      end
      a
    EORUBY
  end
=end
  #def testBlockWeird
    # * test nexts etc in a value context - does stack break?
    
  #end
end

# Run if we were directly called
if $0 == __FILE__
  testrunner = RUNIT::CUI::TestRunner.new
  if ARGV.size == 0
    suite = TestByteCodeRuby.suite
  else
    suite = RUNIT::TestSuite.new
    ARGV.each do |testmethod|
      if testmethod !~ /^-/   # ignore cli switches
	suite.add_test(TestByteCodeRuby.new(testmethod))
      end
    end
  end
  testrunner.run(suite)
end
