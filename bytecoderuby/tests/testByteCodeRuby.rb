$: << File.dirname($0) << File.join(File.dirname($0), "..")
require 'runit/testcase'
require 'runit/testsuite'
require 'runit/cui/testrunner'

require 'compile'

# FIXME necessary until we get GC integrated properly
GC.disable

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
    def compare( mri_src, 
		 bcr_src = mri_src, print_src = mri_src,
		 post_process = nil )
      # Ensure MRI and BCR return the same result when 
      # they run MRI_SRC and BCR_SRC. PRINT_SRC is the src code
      # to print, if result printing is turned on. If specified, 
      # POST_PROCESS is a proc to that gets run on the return values 
      # of MRI_SRC and BCR_SRC before they are compared.

      print = ARGV.include?("-print")

      puts "\n*** Compiling: #{print_src}" if print

      begin
	# ensure MRI evals run in the same context as BCR evals.
	mri_val = eval(mri_src, TOPLEVEL_BINDING)
      rescue Exception => mri_val
      end

      begin
	bcr_val = value_of bcr_src
      rescue Exception => bcr_val
      end      

      if post_process
	mri_val, bcr_val = post_process.call(mri_val, bcr_val) 
      end

      if print
	puts " MRI ==> #{mri_val.inspect}"
	puts " BCR ==> #{bcr_val.inspect}"
	# print backtrace if compilation error
	if Exception === bcr_val and bcr_val.backtrace 
	   if bcr_val.backtrace.find {|l| /compile_for/.match(l)}
	    puts bcr_val.backtrace 
	  end
	end
      end

      if Exception === mri_val
	assert_equal( mri_val.class, bcr_val.class )

	# Remove a) line numbers and b) details of where exception
	# was raise MRI exceptions cos BCR doesn't do these yet
	mri_msg = mri_val.message.gsub(/\(eval\):\d*:\s*/, "").
	                          gsub(/in `.*':\s*/, "")
	assert_equal( mri_msg, bcr_val.message )

      else
	# no exception
	assert_equal( mri_val, bcr_val )
      end
    end
    def compare_method(src)
      # ensure MRI and BCR return the same result when 
      # the Ruby method call SRC is run
      compare("obj = MRITestClass.new; #{src}",
	      "obj = BCRTestClass.new; #{src}",
	      src)
    end
    def remove_string(val, remove)
      if String === val
	val.gsub(remove, "")
      else
	val
      end
    end
    def compare_class(src)
      compare( "class MRITestClass; #{src}; end",
	       "class BCRTestClass; #{src}; end",
	       src,
	       # postprocessor to remove MRI & BCR strings if present
	       proc do |mri_val, bcr_val|
		 a = remove_string(mri_val, "MRI")
		 b = remove_string(bcr_val, "BCR")
		 [a, b]
	       end )

    end
  end

  def add_test_methods(src)
    # Native Ruby class
    mri_src = "class MRITestClass; #{src}; end"
    eval(mri_src, TOPLEVEL_BINDING)
    
    # BCR class
    bcr_src = "class BCRTestClass; #{src}; end"
    Bytecode.compile_and_run_for_void(bcr_src)
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
    compare %q{ [1, 2, 'a', 'b', :a, :b]  }
    compare %q{ {1=>2, 'a'=>'b', :a=>:b} }
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

  def testCCalls
    # tests calls to C methods

    add_test_methods <<-EORUBY
      def bloo
	# loop tests BCR's reuse of the stack
	i = 10000
	while i > 0
	  a = "%s" % self
	  i -= 1
	end
	a
      end
      def to_s
	"blah" + "abc"
      end
    EORUBY

    # bytecode -> C
    compare '1+2'
    compare '"abc".size'

    # bytecode -> C -> bytecode method calls
    compare_method '"%s" % obj'
    compare_method 'z = "abc"; obj.bloo + z'
    compare_method 'z = "abc"; z + obj.bloo'

    # bytecode -> C -> bytecode yields
    compare '[1,2,3].each'        # local jump error
    compare '[1,2,3].each {}'     # test return value with block
    compare 'a = []; [1,2,3].each                { |b| a.unshift(b) }; a'
    compare 'a = []; "abc".scan(Regexp.new(".")) { |b| a.unshift(b) }; a'  # uses rb_block_given_p

    # ditto, returning result to C
    compare '"abc".gsub(Regexp.new(".")) { |a| a + "-"}'

    compare <<-EORUBY
      a = []
      [1,2,3].each { |x|
	[4,5,6].each { |y|
	  a << x << y
	}
      }
      a
    EORUBY
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

  def testOrAndNot
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

    compare 'not true'
    compare 'not false'
    compare '!true'
    compare '!false'
    compare '!nil'
    compare '!123'
  end

  def testMultipleAssignment
    compare 'b = 1; c = 2; d = 3; a = b, c, d; a'
    compare 'b = 1; c = 2; d = 3; a = b, c, d'
    
    compare 'b = 1; c = 2; d = [3, 4]; a = b, c, *d; a'
    compare 'b = 1; c = 2; d = [3, 4]; a = b, c, *d'

    # the [[a], [b] .. ] trick lets us make sure that each of
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

    # FIXME obj.ivar= on lhs of multiple assignment
  end


  def testMethods

    add_test_methods <<-EORUBY
      def single_result
	1
      end
      def multiple_results
	return 1, 2, 3
      end
      def multiple_results_star
	z = [2,3]; return 1, *z
      end
      def arg(a)
	a
      end
      def args(a, b, c)         
	return [a, b, c]
      end
    EORUBY

    compare_method 'obj.single_result'    

    compare_method 'obj.multiple_results'    
    compare_method 'x,  y = obj.multiple_results; [[x], [y]]'    
    compare_method 'x, *y = obj.multiple_results; [[x], [y]]' 
      
    compare_method 'obj.multiple_results_star'

    compare_method 'obj.arg(1)'
    compare_method 'obj.arg(*[1])'

    compare_method 'obj.args(1, 2, 3)'
    compare_method 'obj.args(1, *[2, 3])'
    compare_method 'z = [2, 3]; obj.args(1, *z)'
    compare_method 'obj.args(*[1, 2, 3])'
    compare_method 'obj.args(1, 2, 3=>4, 5=>6)'

    # calling C methods with too many / too few args
    compare_method '"abc".tr("a", "b", "c")'
    compare_method '"abc".tr("a")'

    # calling Ruby methods with too many / too few args
    compare_method 'obj.args(1, 2, 3, 4)'
    compare_method 'obj.args(1, 2)'

    # non-existent method
    compare %{ "test".does_not_exist }
  end

  def testArgs
    add_test_methods <<-EORUBY
      def args_rest(*r)
	r
      end
      def args_optional(o=5, p=6, q=o+p)
        [[o], [p], [q]]
      end
      def args_optional_rest(o=5, p=6, q=o+p, *r)
        [[o], [p], [q], [r]]        
      end
      #def args_normal
      #end
      def args_normal_rest(n, *r)
	[[n], [r]]
      end
      def args_normal_optional(n, o=5, p=6, q=n+o+p)
        [[n], [o], [p], [q]]        
      end
      def args_normal_optional_rest(n, o=5, p=6, q=n+o+p, *r)
	[[n], [o], [p], [q], [r]]       
      end
    EORUBY
      
    args    = [1,2,3,4,5]
    methods = ['rest', 'optional', 'optional_rest', 'normal_rest',
               'normal_optional', 'normal_optional_rest']
    split = 2

    methods.each do |method|
      call = "obj.args_#{method}"
      (0 .. args.size).each do |i|
	a = args[0, i].join(',')
	compare_method "#{call}(   #{a}  )"
	compare_method "#{call}(*[ #{a} ])"
	if i >= split
	  a = args[0,     split  ].join(',')
	  b = args[split, i-split].join(',')
	  compare_method "z=[#{b}]; #{call}( #{a}, *z )"
	end
      end 
    end
  end

  def testBlocks
    # - yield for value, yield for void
    # - yield args: none, one, many
    # - arguments: none, multiple, gather, scatter
    # - access & assign to vars from enclosing scope
    # - nested blocks

    add_test_methods <<-EORUBY
      def yield_void
	i = 1; while i <= 4; yield i; i += 1; end
      end
      def yield_value(a)                 
	# also tests yield from inside a block
	yield_void { |i| a << (yield i) }
	a
      end
      def yield_args
	yield 1, 2
      end
      def return_test
	yield_void do |x| 
	  yield_void do |y| 
	    if x + y == 6 then return :a; end
	  end
	end
	return :b
      end
    EORUBY

    compare_method 'obj.yield_void'

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


    # FIXME bug in Ruth causes this to segv  
    #compare_method 'obj.yield_args {||                 }'
    compare_method 'obj.yield_args {                         }'
    compare_method 'obj.yield_args {|*|                      }'
    compare_method 'obj.yield_args {|a|      a               }'
    compare_method 'obj.yield_args {|a,|     a               }'
    compare_method 'obj.yield_args {|a,*|    a               }'
    compare_method 'obj.yield_args {|*a|     a               }'
    compare_method 'obj.yield_args {|a,b|    [[a], [b]]      }'
    compare_method 'obj.yield_args {|a,b,|   [[a], [b]]      }'
    compare_method 'obj.yield_args {|a,*b|   [[a], [b]]      }'
    compare_method 'obj.yield_args {|a,b,c|  [[a], [b], [c]] }'
    compare_method 'obj.yield_args {|a,b,*c| [[a], [b], [c]] }'

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
#  end

#  def testBlockJumps
    # - break, next, redo, return
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
    add_test_methods <<-EORUBY
      def retry_helper(a)
	a << :a
	self
      end
    EORUBY

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

  def testIvars
    add_test_methods <<-EORUBY
      def ivar= (val); @ivar = val;                  end
      def ivar;        @ivar;                        end
      def ivar_mult;   @a, @b = [1,2]; [[@a], [@b]]; end
    EORUBY

    compare_method 'obj.ivar = :xyz'
    compare_method 'obj.ivar = :xyz; obj.ivar'
    compare_method 'obj.ivar_mult'
  end

  def testCase
    stmts = [ # match first clause, first item
              %q{ case "abc"
                  when String, Float; a = 1
                  when String, Class; a = 2
                  end },
              # match later clause, later item
              %q{ case "abc"
                  when Integer, Class; a = 1
                  when Float, String;  a = 2
                  end },
              # hit else
              %q{ case "abc"
                  when Integer; a = 1
                  else        ; a = 2
                  end },
              # no match - fall through
              %q{ case "abc"
                  when Integer; a = 1
                  end },
              # empty when clause body
              %q{ case "abc"
                  when String
                  end },
              # nested statements
              %q{ case "abc"
                  when String
                    case 123
                    when String;  a = 1
                    when Integer; a = 2    
                    end
                  end },
            ]

    stmts.each do |stmt|
      compare stmt.gsub("a = ", "")    # test value context - remove assignments
      compare "a = 0;#{stmt};a"        # test void context - initialise var
    end

    # Test order of evaluation of when clauses 
    compare <<-EORUBY
      a = []
      case "abc"
      when begin a << 1; Integer; end
      when begin a << 2; String;  end
      when begin a << 3; Float;   end
      end
      a   # should be [1,2] - only first two 'when' test values evaluated
    EORUBY

  end

  # declare at top level
  Object.class_eval do
    class ParentError     < StandardError; end
    class ChildError      < ParentError;   end
    class GrandChildError < ChildError;    end
  end

  def testRescue

    # test raising
    compare %{ raise "abc" }
    compare %{ raise ParentError }

    stmts = [
      # no exception
      %q{ begin; 1
          rescue ParentError; 2
          end },
      # exception caught
      %q{ begin; 1; raise ParentError; 2
          rescue ParentError; 3
          end },
      # exception caught by superclass
      %q{ begin; 1; raise ChildError; 2
          rescue ParentError; 3
          end },
      # exception not caught by subclass
      %q{ begin; 1; raise ParentError; 2
          rescue ChildError; 3
          end },
      # exception caught by 2nd clause
      %q{ begin; 1; raise ParentError; 2
          rescue ChildError; 3
          rescue ParentError; 4
          end },
      # nested, no exception
      %q{ begin
	    begin; 1
            rescue ParentError; 2
	    end
          rescue ParentError; 3 
          end },
      # nested, matches inner
      %q{ begin
	    begin; 1; raise ChildError; 2
	    rescue ChildError; 3
	    end
          rescue ParentError; 4
          end },
      # nested, matches outer
      %q{ begin
	    begin; 1; raise ParentError; 2
	    rescue ChildError; 3
	    end
          rescue ParentError; 4
          end },
      # doubly nested, matches outer
      %q{ begin
	    begin
	      begin; 1; raise ParentError; 2
	      rescue GrandChildError; 3
	      end
	    rescue ChildError; 4
	    end
          rescue ParentError; 5
          end },
      # raise in rescue clause
      %q{ begin
	    1; raise ParentError; 2
          rescue ParentError
	    3; raise ChildError;  4
          end },
      # caught raise in rescue clause
      %q{ begin
	    1; raise ParentError; 2
          rescue ParentError
	    begin
	      3; raise ChildError; 4
	    rescue ChildError; 5
	    end
          end },
      # uncaught raise in rescue clause
      %q{ begin
	    1; raise ParentError; 2
          rescue ParentError
	    begin
	      3; raise ChildError; 4
	    rescue GrandChildError; 5
	    end
          end },
      # deep rescue nesting
      %q{ begin
	    1; raise ParentError; 2
          rescue ParentError
	    begin
	      3; raise ParentError; 4
	    rescue ParentError; 5
              begin
                6; raise ChildError; 8
              rescue GrandChildError; 9
              end
	    end
          end },
      # multiple classes in a rescue
      %q{ begin
	    1; raise ChildError; 2
          rescue GrandChildError, ParentError; 3
          end },
      # multiple classes in a rescue, no match
      %q{ begin
	    1; raise ParentError; 2
          rescue GrandChildError, ChildError; 3
          end },
      # else - no exception (else executed)
      %q{ begin;             0
          rescue ChildError; 1
          else;              2
          end },
      # else - exception (else not executed)
      %q{ begin;             0; raise ChildError
          rescue ChildError; 1
          else;              2
          end },
      # else - uncaught exception
      %q{ begin;             0; raise ParentError
          rescue ChildError; 1
          else;              2
          end },
      # else - exception raised in else
      %q{ begin;             0  # FIXME leaves unwanted item on stack
          rescue ChildError; 1
          else;              2; raise ChildError
          end },
    ]

    stmts.each do |stmt|
      # compilation for value + lets uncaught exceptions through
      compare stmt

      # compilation for void + tracks flow through the code
      stmt = stmt.gsub(/\d/) {|match| "a << #{match}"}
      compare "a = []; begin; #{stmt}; rescue Exception; a << 99; end; a"
    end

    compare <<-EORUBY # arbitrary expressions in a rescue
      a = [ChildError, ParentError]
      begin
	raise ParentError; 1
      rescue a[0], a[1];   2
      end
    EORUBY
    compare <<-EORUBY # raised in lower frame, not caught
      o = "test"
      def o.simple_raise
	1; raise ParentError; 2
      end
      begin 
	3; o.simple_raise;    4
      rescue ChildError;      5
      end
    EORUBY
    compare <<-EORUBY # raised in lower frame, caught higher
      o = "test"
      def o.simple_raise
	1; raise ParentError; 2
      end
      begin
	3; o.simple_raise;    4
      rescue ParentError;     5
      end
    EORUBY
    compare <<-EORUBY # raised in lower frame, caught there
      o = "test"
      def o.raise_and_rescue
	begin
	  1; raise ParentError; 2
	rescue ParentError;     3
	end
      end
      begin
	4; o.raise_and_rescue;  5
      rescue ParentError;       6
      end
    EORUBY
    compare <<-EORUBY # raised in lower frame, not caught there
      o = "test"
      def o.raise_and_rescue
	begin
	  1; raise ParentError; 2
	rescue ChildError;      3
	end
      end
      begin
	4; o.raise_and_rescue;  5
      rescue ParentError;       6
      end
    EORUBY
  end

  def testRescue2
    compare <<-EORUBY  # capture the raised exception
      begin
	raise ParentError
      rescue ParentError => c
	c.class
      end
    EORUBY

    compare <<-EORUBY  # capture the raised exception ..
      begin
	raise ParentError
      rescue ParentError => c
	# .. but don't do anything with it (= strange code!)
	# Value of this expression is the ParentError ?!
      end
    EORUBY

    compare <<-EORUBY 
      2.times {
        begin
	  raise ParentError
        rescue ParentError => c  # this store is Dasgn_curr cos of the block
        end
      }
    EORUBY

    compare <<-EORUBY 
      c = nil
      2.times {
        begin
	  raise ParentError
        rescue ParentError => c # this one is Lasgn to outer block
        end
      }
      c
    EORUBY

    compare %q{
      i = 0;
      while i < 5
	begin
	  raise ChildError
        rescue ChildError
          break
        end
      end; i }

    compare <<-EORUBY
      begin
	raise ParentError
      rescue ParentError
	q = 1
      end
      q
    EORUBY

return 
    compare %{
      begin   # can't cope with empty body
      rescue ParentError
      end
    }

    compare <<-EORUBY
      begin
	1 
      rescue ParentError
	qq = 1
      end
      qq   # should default to nil
    EORUBY


    compare <<-EORUBY
      begin;  raise "cain"
      rescue "not an exception class"
      end
    EORUBY

  end

  def testRescue3
    compare %{ 1 / 0 }

    # Error raised in C code
    compare %{
      begin
	1/0
      rescue ZeroDivisionError
	"zero div error"
      end
    }

    # Stack is bytecode-C-bytecode-C when error raised
    compare %{
      begin
	[1,2].each {|dummy| 1/0}
      rescue ZeroDivisionError
	"zero div error"
      end
    }

    # Re-raise an exception
    compare %{
      begin
	[1,2].each {|dummy| 1/0}
      rescue ZeroDivisionError
	raise()
      end
    }

    # Does re-raised exception = previous exception?
    compare %{
      e = e2 = nil    # FIXME only BCR requires this decl
      begin 
	begin
	  raise ParentError
	rescue ParentError => e
	  raise()
	end
      rescue ParentError => e2
      end
      e.equal?(e2)
    }

return
    # this fails when raise() in position 3 - we don't reset $!
    # after the end of the GrandChildError
    stmt = %{
      begin
	raise ChildError
      rescue ChildError
	1
	begin
	  raise GrandChildError
	rescue GrandChildError
	  2
	end
        3
      end 
    }

    (1..3).each do |x| 
      compare stmt.gsub(x.to_s, "raise()")
    end


    # FIXME - this one is easy to fix
    compare %{
      begin
	raise StandardError
      rescue  # empty rescue catches StandardError
	1
      end
    }

  end

  def testSuper
    add_test_methods %{
      class X;      
	def simple(a); a + 1; end; 
      end
      class Y < X;
	def no_super(); super(); end;
      end
    }

    # override only
    compare_class %{ 
      class Y < X; def simple(a); a + 10; end; end
      Y.new.simple(100) 
    }    

    # standard call to super
    compare_class %{ 
      class Y < X; def simple(a); super(a + 10); end; end
      Y.new.simple(100) 
    }    

    # call to super with wrong # args
    compare_class %{ 
      class Y < X; def simple(a); super(); end; end
      Y.new.simple(100) 
    }    

    # override method has diff #args to super
    compare_class %{ 
      class Y < X; def simple(a,b); super(a + b); end; end
      Y.new.simple(100,10) 
    }    

    # call to super with no args
    compare_class %{ 
      class Y < X; def simple(a); super; end; end
      Y.new.simple(100) 
    }    

    # call to super with no args; args changed before super call
    compare_class %{ 
      class Y < X; def simple(a); a += 10; super; end; end
      Y.new.simple(100) 
    }    

    # call to super with no args; override method has diff #args
    compare_class %{ 
      class Y < X; def simple(a,b); super; end; end
      Y.new.simple(100,10) 
    }    

    # call to super with no args, override method has optional arg
    compare_class %{ 
      class Y < X; def simple(a, b=10); super; end; end
      Y.new.simple(100) 
    }    

    # call to super with no args, override method has unused rest arg
    compare_class %{ 
      class Y < X; def simple(a, *c); super; end; end
      Y.new.simple(100) 
    }    

    # super call to C method from singleton method
    compare %{
      o = "test"
      def o.size; super * 2; end
      o.size
    }

    # FIXME super call to methods in modules?

return 

    # Failing tests
    
    # call to super in a method which doesn't have a super
    # fix this when correcting calls to private etc
    compare_class %{ Y.new.no_super() }

    # FIXME check these in 1.7.3/1.8.0 before implementing
    # call to super with no args, override method has used rest arg
    compare_class %{ 
      class Y < X; def simple(a, *c); super; end; end
      Y.new.simple(100, 1000, 10000) 
    }    

    # call to super with no args, override method has optional and rest arg
    compare_class %{ 
      class Y < X; def simple(a, b=10, *c); super; end; end
      Y.new.simple(100) 
    }    

  end


  def testStack
    # FIXME - not sure what this is testing!
    compare <<-EORUBY
      "a" + (["b", "c"].each {|a| a})
    EORUBY
  end

  def testClasses

    # Ensure both MRI and BCR are running in the same context
    compare %{ self }

    compare %{ self.class.constants.sort }

    compare %{ Object }

    # Class creation
    compare_class %{ 
      class A
      end
      A.name
    }

    compare_class %{
      class B
	self.name
      end
    }

    compare_class %{
      class C
	class D; self.name; end
      end
    }

    # Class body runs in new scope
    compare %{
      a = 1  
      class Object
	a = 2
      end
      a
    }

    # Constant lookup
    compare_class %{
      class A; Object; end
    }

    # Super classes
    compare_class %{
      class A; end; A.superclass
    }

    compare_class %{
      class E < String 
	def size; 10; end
      end
      [E.superclass, E.new("abc").size]
    }

    compare_class %{
      a = String
      class F < a; end; F.superclass
    }

return

    # Failing tests

    # can't define singleton on top-level object
    compare %{
      def test(); 1; end
      test()
    }

    # Module.constants uses data from eval.c to decide what to return, but
    # BCR can't set up this data to match MRI
    compare %{
      Module.constants.sort
    }

    # Module.nesting also uses eval.c data
    compare %{
      class Hi; Module.nesting; end
    }

  end


  def testConsts
    compare %{ DOES_NOT_EXIST }

return
    # failing tests

    compare_class %{ A = 1; A }

    compare_class %{
      class A; B; end
    }

  end


  def testCatch
    compare %{ 
      a = []
      catch (:hi) do
	a << 1
	throw :hi
	a << 2
      end 
      a << 3
    }
return # failing tests
    # ensure not implemented yet
    compare %{ 
      a = []
      catch (:hi) do
	a << 1
	begin
	  throw :hi
	ensure
	  a << 2
	end
	a << 3
      end 
      a << 4
    }
  end

  def testStackAndJumps
    # Ensure stack not hosed - the "a" should not be in play
    # by the time we get to add "b" and "c"
    compare %q{
      a = "test"
      def a.test
	"a" + raise(Exception)
      end
      "b" + begin
	    a.test
	  rescue Exception
	    "c"
	  end
    }


return
    # FIXME the stack is broken here - result should be "bd" but is "cd"
    compare <<-EORUBY
      "b" + begin
	    "c" + raise(Exception)
	  rescue Exception
	    "d"
	  end
    EORUBY

    # The following (unnatural) code breaks the stack - 
    # result should be "ae", but in BCR the stack at the time of the first +
    # is "a", "b", "e", so the result is "be". The break should clear
    # the stack back to where it was when the expression it's part of
    # first started being evaluated. Other non-local jumps (correct term?) 
    # will behave in the same way.
    compare <<-EORUBY
      "a" + begin
	      while true
		"b" + begin break; "c" end
	      end
	      "e"
	    end
    EORUBY
  end
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
