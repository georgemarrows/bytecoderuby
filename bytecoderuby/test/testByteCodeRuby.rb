$: << 'lib' << 'ext'

require 'test/unit/testcase'
require 'test/unit/ui/console/testrunner'

require 'bcr/bcr'

class TestByteCodeRuby < Test::Unit::TestCase

  def value_of(src)
    Bytecode.compile_and_run_for_return(src)
  end

  BENCHMARK = ARGV.include?("-bench")

  @@test_class_id = 0

  if BENCHMARK
    RESULTS = []
    LOOPS   = 50000
    module Benchmarking
      def time( times, &block )
	GC.start
	begtime = Process.times.utime
	times.times( &block )
	endtime = Process.times.utime
	endtime - begtime
      end
      def benchmark(mri_src, bcr_src)
	mri = time(1) { eval(mri_src) }
	bcr = time(1) { Bytecode.compile_and_run_for_return(bcr_src) }
	return mri, bcr
      end
      def loopify(src)
	%{ cnt = 0; while cnt < #{LOOPS}; #{src}; cnt += 1; end }
      end
    end
    include Benchmarking; extend Benchmarking
    # Used to factor out the cost of the loop itself  
    MRI_LOOP, BCR_LOOP = benchmark(src = loopify(""), src)

    def results(mri_src, bcr_src, src)

      mri, bcr = benchmark(mri_src, bcr_src)
      mri, bcr = mri - MRI_LOOP, bcr - BCR_LOOP
      printf "\n%s\nMRI\t%.2f\nBCR\t%.2f\n%%age\t%.2f\n", 
	      src, mri, bcr, 100*bcr/mri

      # Record and write out results. We write out after each test
      # in case the interpreter crashes.
      RESULTS << [src, mri, bcr]
      File.open("results", "w") do |f|
	Marshal.dump(RESULTS, f)
      end

    rescue Exception => e
      print "\n#{src}\nERROR => #{e}\n"
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
    def compare_class(src, name_space=:new)
      if name_space == :new
      then
	# Run each BCR test in a different namespace so that the
	# effects of one BCR test don't interact with the effects of
	# another. Similarly for MRI tests.
	id = @@test_class_id += 1
      else
	id = ""
      end
      loop_src = loopify(src)
      results("class MRITestClass#{id}; #{loop_src}; end",
	      "class BCRTestClass#{id}; #{loop_src}; end",
	      src)
    end
    def compare_flow(src, dummy=true)
      compare(src)
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
      end

      if Exception === mri_val
	print mri_src if  mri_val.class != bcr_val.class #, mri_src, "\n"
	assert_equal( mri_val.class, bcr_val.class )

	# Remove 
	# a) line numbers 
	# b) details of where exception was raised from MRI exceptions 
	# - BCR doesn't do these yet
	# c) object ids
	# - these don't stand a chance of matching
	mri_msg = mri_val.message.gsub(/\(eval\):\d*:\s*/, "").
	                          gsub(/in `.*':\s*/, "").
	                          gsub(/0x[0-9a-f]{1,8}/, "")
	bcr_msg = bcr_val.message.gsub(/0x[0-9a-f]{1,8}/, "")

	assert_equal( mri_msg, bcr_msg )

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
      case val
      when String
	val.gsub(remove, "")
      when Exception
	# create new exception of the same type but with REMOVE 
	# stripped out of the message
	val.class.exception(val.message.gsub(remove, ""))
      else
	val
      end
    end

    def compare_class(src, name_space=:new)
      if name_space == :new
      then
	# Run each BCR test in a different namespace so that the
	# effects of one BCR test don't interact with the effects of
	# another. Similarly for MRI tests.
	id = @@test_class_id += 1
      else
	id = ""
      end
      compare("class MRITestClass#{id}; #{src}; end",
	      "class BCRTestClass#{id}; #{src}; end",
	       src,
	       # postprocessor to remove MRI & BCR strings if present
	       proc do |mri_val, bcr_val|
		 a = remove_string(mri_val, "MRI")
		 b = remove_string(bcr_val, "BCR")
		 [a, b]
	       end )

    end

    def compare_flow(src, for_value=true)
      # Tracks flow through the code
      # A test like
      #   begin
      #   ensure
      #   end
      # gets transformed into
      #   a = []
      #   begin
      #     begin; a << 1
      #     ensure; a << 2
      #     end; a << 3
      #   rescue Exception
      #     a << 99
      #   end
      #   a
      # before being run. This tracks control flow through the code.

      # compilation for value + lets uncaught exceptions through
      compare(src) if for_value
      
      # compilation for void + tracks flow through the code
      cnt = 0
      src = src.gsub(/(\n|$)/) { "; a << #{cnt+=1}\n" }
      compare("a = []; begin; #{src}; rescue Exception; a << 99; end; a")
    end

  end

  def TestByteCodeRuby.add_test_methods(src)
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
    compare %q{ (0..2) }
    compare %q{ (0...2) }

    # handled by NodeLit
    compare %q{ /a/ }
    compare %q{ /a/m }
    compare %q{ /a\1/m }
  end

  def testLiteralCopies
    # check literals create new objects each time they are 'executed'
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



  add_test_methods <<-EORUBY
    def bloo
      # loop tests BCR's reuse of the stack
      #return "%s" % self
      
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


  def testCCalls
    # tests calls to C methods

    # bytecode -> C
    compare '1+2'
    compare '"abc".size'

    # bytecode -> C -> bytecode method calls
    compare_method '"%s" % obj'

    unless BENCHMARK
      compare_method 'z = "abc"; obj.bloo + z'
      compare_method 'z = "abc"; z + obj.bloo'
    end

    # bytecode -> C -> bytecode yields
    compare '[1,2,3].each'        # local jump error
    compare '[1,2,3].each {}'     # test return value with block
    compare 'a = []; [1,2,3].each { |b| a.unshift(b) }; a'

    # #scan calls rb_block_given_p
    compare 'a = []; "abc".scan(Regexp.new(".")) { |b| a.unshift(b) }; a' 

    # #gsub calls rb_block_given_p and result of block is returned to C
    compare '"abc".gsub(Regexp.new(".")) { |a| a + "-"}'

    # jumps in/out of bytecode -> C -> bytecode blocks
    actions = ['', 'next', 'redo', 'break', 'return [a,b]', ]

    loop = <<-EORUBY
      o = "test"
      def o.loop(a, b)
	[1,2,3,4].each do |x| 
	  a << x
	  if x==2 then x=9; ACTION end
	  b << x
	end
	[a << 99, b << 99]
      end
      o.loop([], [])
    EORUBY

    actions.each do |action|
       compare loop.sub(/ACTION/, action)
    end

    # value of break in a C call
    compare %{
      [1,2,3].each { break } # returns nil
    }

    # nested bytecode -> C -> bytecode yields + break
    compare <<-EORUBY
      a = []
      [1,2,3].each { |x|
	[4,5,6].each { |y|
	  a << x << y
	  break if x == 2 or y == 5
	}
      }
      a
    EORUBY
  end

  def testIfUnless
    # tests applicable to both if and unless
    if_unless_tests = [
      # tests compile for value
      'if true  then 1 end',
      'if false then 1 end',
      'if true  then 1 else 2 end',
      'if false then 1 else 2 end',
      '1 if true',
      '1 if false',
      # tests compile for void
      'i=0; if true  then i=1 end; i',
      'i=0; if false then i=1 end; i',
      'i=0; if true  then i=1 else i=2 end; i',
      'i=0; if false then i=1 else i=2 end; i',
      'i=0; i=1 if true; i',
      'i=0; i=1 if false; i',
    ]

    if_unless_tests.each { |test| compare test }
    if_unless_tests.each { |test| compare test.sub(/if/, "unless") }
    
    # test using :true and nil instead of true and false
    if_unless_tests.each do |test| 
      compare test.sub(/true/, ":true").sub(/false/, "nil") 
    end

    # elsif tests (don't apply to unless)
    # for value
    compare 'if false then 1 elsif true  then 2 end'
    compare 'if false then 1 elsif false then 2 end'
    compare 'if false then 1 elsif true  then 2 else 3 end'
    compare 'if false then 1 elsif false then 2 else 3 end'
    # for void
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

  def testUntil
    compare <<-EORUBY            # Basic loop
      i=0
      until i > 3
        i+= 1
      end
      i
    EORUBY

    compare <<-EORUBY              # Nested loops
      i = j = sum = 0.0
      until i > 2; i += 1
        until  j  > 3; j += 1
          sum += i/j
        end
      end
      sum 
    EORUBY

    compare <<-EORUBY               # Empty body
      arr = [1,2,3]
      until arr.shift == 2; end
      arr
    EORUBY

    compare <<-EORUBY               # As modifier
      arr = [1,2,3]
      arr.shift until arr.size <= 1
      arr
    EORUBY

    compare <<-EORUBY               # Mix while/until
      i = j = sum = 0.0
      until i > 2; i += 1
        while  j  <= 3; j += 1
          sum += i/j
        end
      end
      sum 
    EORUBY
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
#    compare 'a, b, c = [1]'

    compare 'a, b, *c  = [1, 2, 3, 4]; [[a], [b], [c]]'
    compare 'a, b, *c  = [1, 2, 3, 4]'

    compare 'a, b, *c  = [1]; [[a], [b], [c]]'
#    compare 'a, b, *c  = [1]'

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

  def testMethods
    # various result types
    compare_method 'obj.single_result'    

    compare_method 'obj.multiple_results'    
    compare_method 'x,  y = obj.multiple_results; [[x], [y]]'    
    compare_method 'x, *y = obj.multiple_results; [[x], [y]]' 
      
    compare_method 'obj.multiple_results_star'

    # various arg types: scatter, hashes
    compare_method 'obj.arg(1)'
    compare_method 'obj.arg(*[1])'

    compare_method 'obj.args(1, 2, 3)'
    compare_method 'obj.args(1, *[2, 3])'
    compare_method 'z = [2, 3]; obj.args(1, *z)'
    compare_method 'obj.args(*[1, 2, 3])'
    compare_method 'obj.args(1, 2, 3=>4, 5=>6)'

    # scatter an arg which isn't actually an array
    # FIXME this works in BCR but is *very* slow for some reason
    compare_method %q{ 
      m = Regexp.new('(\d)(\d)').match("12")
      obj.args( *m ) # to_ary method makes this behave like an array
    }

    compare_method %q{ obj.args(*123) }

    # FIXME this causes a 'bc_yield but frame not C' error
    compare_method %q{ obj.args(*"123") }

    # calling C methods with too many / too few args
    compare_method '"abc".tr("a", "b", "c")'
    compare_method '"abc".tr("a")'

    # calling Ruby methods with too many / too few args
    compare_method 'obj.args(1, 2, 3, 4)'
    compare_method 'obj.args(1, 2)'

    # non-existent method
    compare %{ "test".does_not_exist }

    # bodyless methods: compilation is slightly different to 
    # standard method
    compare %q{
      o = "test"
      def o.bodyless_method; end
      o.bodyless_method
    }

    compare %q{
      o = "test"
      def o.bodyless_method_with_args(x,*r); end
      o.bodyless_method_with_args(1, 2, 3)
    }

    # call without self, () or args (= NodeVcall)
    compare %q{
      o = "test"
      def o.test
	size
      end
      o.test
    }
  end


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

  def testArgs
      
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

  add_test_methods <<-EORUBY
    def yield_nothing
      yield
    end
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


  def testBlocks
    # - yield for value, yield for void
    # - yield args: none, one, many
    # - arguments: none, multiple, gather, scatter
    # - access & assign to vars from enclosing scope
    # - nested blocks

    compare_method 'obj.yield_nothing'

    compare_method 'a=[]; obj.yield_nothing {     a << 9 }; a'
    compare_method 'a=[]; obj.yield_nothing { |x| a << x }; a'

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
    EORUBY
  end

  def testBlockJumps
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

    # value returned by an iter method when break is called in block
    # - should be nil with no argument
    compare_method <<-EORUBY
      a = []  
      b = obj.yield_value(a) {|x| if x==2 then break else x end }
      [a, b]
    EORUBY

    # - or the value of the argument
    compare_method <<-EORUBY
      a = []  
      b = obj.yield_value(a) {|x| if x==2 then break :a else x end }
      [a, b]
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

  add_test_methods <<-EORUBY
    def ivar= (val); @ivar = val;                  end
    def ivar;        @ivar;                        end
    def ivar_mult;   @a, @b = [1,2]; [[@a], [@b]]; end
  EORUBY

  def testIvars
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
      compare stmt.gsub(/a = /, "")    # test value context - remove assignments
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
  eval %{
    class ParentError     < StandardError; end
    class ChildError      < ParentError;   end
    class GrandChildError < ChildError;    end
  }, TOPLEVEL_BINDING


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

    # test break from inside rescue clause
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

    # Empty rescue catches StandardError
    compare %{
      begin
	raise StandardError
      rescue
	1
      end
    }

  end


  def testRescue4
    compare %{
      begin   # can't cope with empty body
	raise ParentError
      rescue ParentError
      end
    }

    compare %{
      begin   # can't cope with empty body
      rescue ParentError
      end
    }

    compare %{
      begin
      rescue
      end
    }

    # ensure runs normally after rescue	1
    compare_flow %{ 
      begin
	raise StandardError
      rescue StandardError
      end
      begin
      ensure
      end 
    }

    # ensure runs normally after rescue	2
    compare_flow %{ 
      begin
	begin
	  raise StandardError
	rescue StandardError
	end
      ensure
      end 
    }

    # ensure stack gets cut back on rescue - answer here should be bd
    compare %{
      "b" + begin
	      "c" + raise(Exception)
	    rescue Exception
	      "d"
	    end
    }


    # stack should get cut back here too - same bd answer expected
    compare %{
      "b" + begin
	      "a" + begin
		      "c" + raise(Exception)
	            rescue 
                    end
	    rescue Exception
	      "d"
	    end
    }

    # should return :b
    compare %{
      o = "test"
      def o.test_a
	return :a
      end
      def o.test_b
	begin
	  return :b
	ensure
	  test_a()
	end
      end
      o.test_b
    }

    # ensure must return :a
    compare %{ 
      def (o='t').test
	begin
	  return :a
	ensure
	  begin
	    raise StandardError
	  rescue StandardError
	  end	 
	end
	return :b
      end
      o.test
    }

    # BCR previously used reraise rather than continuing the break
    compare_method %{
      [1,2].each do
	begin
	  break :a
	ensure
	  raise rescue nil
	end
      end
    }

    # BCR previously used to break rather than continuing the return
    compare %{
      def (o='t').test
	[1,2].each do
	  begin
	    return :a
	  ensure
	    [1,2].each do 
	      break
	    end
	  end
	end
	return :b
      end
      o.test      
    }
return
    # failing tests

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



  def testEnsure

    stmts = [

      # normal flow
      %{ begin
         ensure
         end },

      # exception runs ensure
      %{ begin
           raise StandardError
         ensure
         end },

      # break from block runs ensure
      %{ [1,2].each do
           begin
             break
           ensure
           end
         end },

      # next in block runs ensure
      %{ [1,2].each do
           begin
             next
           ensure
           end
        end },

      # return runs ensure
      %{ o = "test"
         def o.test(a)
	    return a
	 ensure
	 end
	 o.test(123) },

      # make sure that the 'break state' set up in the VM is cleared by
      # the time of the ensure
      %{ [1,2].each do
           break
         end
         begin         
         ensure
         end },

      # ditto, different arrangement	 
      %{ begin
           [1,2].each do
             break
           end
         ensure
         end },

      # break through 2 ensures
      %{ [1,2].each do
           begin
             begin
               break
             ensure
             end
           ensure
           end
         end },

      # break through an ensure; run 2nd normally
      %{ begin
           [1,2].each do
             begin
               break
             ensure
             end
           end
         ensure
         end },

      # break overrides exception
      %{ [1,2].each do
           begin
             raise StandardError
           ensure
             break
           end
         end },

      # exception overrides break
      %{ [1,2].each do
           begin
             break
           ensure
             raise StandardError
           end
         end },

     ]

     stmts.each do |stmt|
       compare_flow(stmt, true)
     end


     # check we don't use too much stack
     unless BENCHMARK
       compare %{
	 i = 0
	 while (i < 10000)
	   begin
	     1
	   ensure
	     2
	   end
	   i+=1
	 end
       }
     end

     # check value of ensure is ignored
     compare %{
       begin 
	 1
       ensure
	 2
       end
     }

   end

   def testEnsureLoops
     # break, next and redo in while should run ensure
     actions = ['break', 'next', 'redo',]
     
     loop = %{
       arr = [1,2,3,4,5]; res = []
       while (el = arr.shift)
         begin
           if el == 3 then el = 103; ACTION; end
         ensure
           res.push(el)
         end
       end
       [arr, res]
     }

     actions.each do |action|
       compare loop.sub(/ACTION/, action)
     end

     # break, next and redo in block should run ensure
     loop = %{
       res = []
       [1,2,3,4,5].each do |el|
         begin
           if el == 3 then el = 103; ACTION; end
         ensure
           res.push(el)
         end
       end
       res
     }

     actions.each do |action|
       compare loop.sub(/ACTION/, action)
     end
  end

  add_test_methods %{
    class X;      
      def simple(a); a + 1; end; 
    end
  }

  def testCVars

    # Make sure this a class variable and not an instance one
    compare_class %{
      class Y
        def ass=(a)
          @@a = a
        end
        def mule
          @@a
        end
      end
      obj_y = Y.new()
      obj_x = Y.new()
      obj_y.ass = 33
      obj_x.mule 
    }

     # Make sure that inheritance/cvar semantics is correct a la Ruby
     compare_class %{
      r = []
      class Y
        @@a=:a
        def mule
          @@a
        end
	 def ass=(a)
          @@a = a
        end
      end

      y = Y.new(); r << y.mule

      class X < Y
         @@a=:b
	 def ass2=(a)
          @@a = a
        end
      end
    
      x = X.new(); r << x.mule

      y.ass  = :c; r << x.mule << y.mule
      x.ass  = :d; r << x.mule << y.mule
      x.ass2 = :e; r << x.mule << y.mule
     
      r
    }

    # Example from matz
    compare_class %{
      r = []
      class Sup
        @@x = "A"          # declare @@x, set @@x as "A"
        def test 
           @@x 
        end
      end
    
      r << Sup.new.test
      class Sub1 < Sup
        @@x = "B"          # set @@x as "B"
      end
      r << Sub1.new.test
      class Sub2 < Sup
        @@x = "C"          # set @@x as "C"
      end
      r << Sub2.new.test
      r
    }

    # Undeclared cvar in instance
    compare_class %{
      class A
	def mule; @@x; end
      end
      A.new.mule
    }

return 
    # failing tests

    # Fetch from singleton method
    compare_class %{
      class A
	@@x = :a
	def mule; @@x; end
      end
      def (o = A.new).mule2
	@@x
      end
      [o.mule, o.mule2]
    }

    # Undeclared cvar in singleton context
    compare %{
      def (o = 'test').mule
	@@x
      end
      o.mule
    }

    # Undeclared cvar in class context
    compare_class %{
      class A
	@@x
      end
    }

    # Fetch in class context
    compare_class %{
      class A
	@@x = :a
	@@x
      end
    }
  end

  def testSuper
    # override only
    compare_class %{ 
      class Y < X; def simple(a); a + 10; end; end
      Y.new.simple(100) 
    }, :fixed_name_space

    # standard call to super
    compare_class %{ 
      class Y < X; def simple(a); super(a + 10); end; end
      Y.new.simple(100) 
    }, :fixed_name_space    

    # call to super with wrong # args
    compare_class %{ 
      class Y < X; def simple(a); super(); end; end
      Y.new.simple(100) 
    }, :fixed_name_space    

    # override method has diff #args to super
    compare_class %{ 
      class Y < X; def simple(a,b); super(a + b); end; end
      Y.new.simple(100,10) 
    }, :fixed_name_space

    # call to super with no args
    compare_class %{ 
      class Y < X; def simple(a); super; end; end
      Y.new.simple(100) 
    }, :fixed_name_space    

    # call to super with no args; args changed before super call
    compare_class %{ 
      class Y < X; def simple(a); a += 10; super; end; end
      Y.new.simple(100) 
    }, :fixed_name_space    

    # call to super with no args; override method has diff #args
    compare_class %{ 
      class Y < X; def simple(a,b); super; end; end
      Y.new.simple(100,10) 
    }, :fixed_name_space

    # call to super with no args, override method has optional arg
    compare_class %{ 
      class Y < X; def simple(a, b=10); super; end; end
      Y.new.simple(100) 
    }, :fixed_name_space

    # call to super with no args, override method has unused rest arg
    compare_class %{ 
      class Y < X; def simple(a, *c); super; end; end
      Y.new.simple(100) 
    }, :fixed_name_space

    # Y#no_super calls a super which doesn't exist
    compare_class %{ 
      class Y
	def no_super(); super(); end;
      end
      Y.new.no_super() 
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
      class A
	self.name
      end
    }

    compare_class %{
      class A
	class B; self.name; end
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
      class A < String 
	def size; 10; end
      end
      [A.superclass, A.new("abc").size]
    }

    compare_class %{
      a = String
      class A < a; end; A.superclass
    }


return

    # Failing tests

    # non-existent superclass
    compare_class %{
      class A < B; end
    }


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
    # basic catch
    compare_flow %{ 
      catch (:hi) do
	   throw :hi
       end 
    }
      
    # uncaught catch
    compare_flow %{ throw :hi }

    # return value from catch
    compare %{
      catch(:hi) do
	throw :hi, 23
      end 
    }

    # nested catches + values. Result is 10 + 2 + 1
    compare %{ 
      1 + catch(:hi) do
	    2 + catch(:bye) do
	          throw :bye, 10
	        end
          end
    }

    # throw runs ensure	 
    compare_flow %{ 
      catch (:hi) do
         begin
	   throw :hi
	 ensure
         end
       end 
    }, false

    # ensure runs normally after caught throw
    compare_flow %{ 
      begin
	catch (:hi) do
	  throw :hi
	end
      ensure
      end
    }, false

    # ensure doesn't interfere with correct tag selection
    compare_flow %{ 
      catch (:hi) do
	catch (:bye) do
	  begin
	    throw :hi
	  ensure
	  end
	end
      end
    }, false

  end

  def testProcClosures

    # simple call
    compare %{ 
      p = proc { "hi" }
      p.call() 
    }

    # error -> no block passed to proc
    compare %{ 
      p = proc()
      p.call() 
    }

    # block to call ignored
    compare %{ 
      p = proc() { "hi" }
      p.call() { "bye" }
    }

    # Test closures ..
    # .. access to self
    compare %{
      p = proc { self }
      p.call
    }

    # .. access to self in dead frame
    compare %{
      def (o = "test").test
	proc { self }
      end
      o == o.test.call()
    }

    # .. read local in current frame
    compare %{
      a = 123
      p = proc { a }
      p.call
    }

    # .. write local in current frame
    compare %{
      a = 123
      p = proc { a = 456 }
      p.call
      a
    }

    # .. write local in current frame
    # local first used after reference in proc
    # (use zzz to avoid clashes with other variable names which may
    # already have been used in the MRI top frame (TOPLEVEL_BINDING))
    compare %{
      p = proc { zzz = 456 }
      zzz = 123
      p.call
      zzz
    }

    # .. read local in earlier frame 
    compare %{
      def (o = "test").test(p) p.call(); end
      a = 123; o.test( proc { a } )
    }

    # .. write local in earlier frame 
    compare %{
      def (o = "test").test(p) p.call(); end
      a = 123; o.test( proc { a = 456 } ); a
    }

    # .. read local in a dead frame 
    compare %{
      def (o = "test").test
	a = 123
	proc { a }
      end
      o.test.call
    }

    # .. read and write local in a dead frame 
    compare %{
      def (o = "test").test
	a = 123
	[proc { a }, proc { a += 1 }]
      end
      get, set = o.test
      [get.call(), set.call(), get.call()]
    }

    # .. read and write local in a dead frame, 
    # 2 links up the static chain
    compare %{
      def (o = "test").test
	a = 123
	[1].each do
	  return [proc { a }, proc { a += 1 }]
	end
      end
      get, set = o.test
      [get.call(), set.call(), get.call()]
    }

    # .. procs with a shared environment 2 links back up the chain
    # but non-shared 1 link back up. Bit of a monster.
    compare %{
      def (o = "test").test
	a = 123
	res = []
	[1].each do; b = 456
	  res << proc { [a,b] } << proc { a += 1; b += 1 }
	end
	[1].each do; b = 789
	  res << proc { [a,b] } << proc { a += 1; b += 1 }
	end
	res
      end
      get1, set1, get2, set2 = o.test
      [get1.call(), set1.call(), get1.call(),
       get2.call(), set2.call(), get2.call()]
    }


  end


  def testProcArgs
    # call with args ..
    # .. single arg
    compare %{
      p = proc { |x| x + 1 }
      p.call(2)
    }

    # .. multiple args
    compare %{
      p = proc { |x,y| x + y }
      p.call(2, 3)
    }

    # .. gather args
    compare %{
      p = proc { |x, *y| [[x], [y]] }
      p.call(1,2,3,4)
    }

   # .. passed too many args to single arg (allowable: x => [1,2])
    compare %{
      p = proc { |x| x }
      p.call(1, 2)
    }

   # .. passed too few args
    compare %{
      p = proc { |x, y| [[x], [y]] }
      p.call(1)
    }

   # .. passed too many args
    compare %{
      p = proc { |x, y| [[x], [y]] }
      p.call(1, 2, 3)
    }

   # .. passed too many args to gather arg (allowable: x => [1,2])
    compare %{
      p = proc { |*x| x }
      p.call(1, 2)
    }

    # .. passed too few args to gather arg (allowable: x => [])
    compare %{
      p = proc { |*x| x }
      p.call()
    }

    # .. passed too few args to arg + gather arg
    compare %{
      p = proc { |x, *y| [[x], [y]] }
      p.call(1)
    }

    #  .. passed too few args to arg + gather arg
    compare %{
      p = proc { |x, *y| [[x], [y]] }
      p.call()
    }
 
   # .. passed too many args to arg + gather arg
    compare %{
      p = proc { |x, *y| [[x], [y]] }
      p.call(1, 2, 3)
    }
    
    # .. passed too many args to no args
    compare %{
      p = proc { }
      p.call(1)
    }

return

    # .. passed too few args to single arg (allowable: x => nil)
    compare %{
      p = proc { |x| x }
      p.call()
    }

    # .. passed too many args to no args (fails -> segv in Ruth)
    compare %{
      p = proc {|| }
      p.call(1)
    }

  end


  def testProcControl

    # Break just returns from proc
    # .. whether proc's owner frame is live
    compare %{
      def (o = "test").test(a)
	p = proc { a << 1; break; a << 2}
	a << 3
	p.call
	a << 4
      end
      a = []
      o.test(a)
      a
    }
    
    # .. dead
    compare %{
      def (o = "test").test(a)
	p = proc { a << 1; break; a << 2}
	a << 3
	p
      end
      a = []
      o.test(a).call
      a
    }

=begin
    # .. or live further back on the stack
    compare %{
      def (o = "test").test(a)
	a << 3
	test2(a) { a << 1; break; a << 2}
	a << 4
      end
      def o.test2(a)
	a << 5
	yield
	a << 6
      end
      a = []
      o.test(a)
      a
    }
=end
=begin
    # FIXME!!!!

    # .. or live further back on the stack
    compare %{
      def (o = "test").test(a)
	p = proc { a << 1; break; a << 2}
	a << 3
	test2(a,p)
	a << 4
      end
      def o.test2(a,p)
	a << 5
	p.call
	a << 6
      end
      a = []
      o.test(a)
      a
    }
=end

    # Procs binding blocks
    # proc should bind the block that was passed to test
    compare %{
      def (o = "test").test
	proc { yield }
      end
      a = 0
      (o.test { a = 1 }).call
      a
    }

    # no block passed - should give LocalJumpError
    compare %{
      def (o = "test").test
	proc { yield }
      end
      (o.test).call
    }

    # bound block's owning frame is dead at yield time
    compare %{
      def (o = "test").yielder_proc
	proc { yield }
      end
      def o.capture_block_proc
	a = 123
	return yielder_proc { a }
      end
      o.capture_block_proc.call
    }



    # FIXME more fun here with passing unusual blocks to o.test

return

    # Return is more complicated than break
    # Owner frame is live -> returns from method
    compare %{
      def (o = "test").test(a)
	p = proc { a << 1; return; a << 2}
	a << 3
	p.call
	a << 4
      end
      a = []
      o.test(a)
      a
    }
    
    # Owner frame is dead -> LocalJumpError
    compare %{
      def (o = "test").test(a)
	p = proc { a << 1; return; a << 2}
	a << 3
	p
      end
      a = []
      o.test(a).call
      a
    }

    # Owner frame is live further back on the stack -> 
    # returns from *current* frame. This looks wrong to me.
    compare %{
      def (o = "test").test(a)
	p = proc { a << 1; return; a << 2}
	a << 3
	test2(a,p)
	a << 4
      end
      def o.test2(a,p)
	a << 5
	p.call
	a << 6
      end
      a = []
      o.test(a)
      a
    }


    # Ruby weirdnesses

    # this code causes MRI Ruby 1.8.0.p2 to shutdown with an 'unexpected
    # break' message!
    compare %{
      def (o = "test").test
	 proc { yield }
      end
      (o.test { break }).call
    }

  end

  def testTmp

  end


  def testProcToBlock
    # FIXME in conjunction with super calls etc
    
    # pass proc as block
    compare %{ 
      a = []
      p = proc { |x| a << x }
      [1,2,3].each &p
      a
    }

    # pass proc from dead frame
    compare %{
      def (o = 'test').make_proc(a)
	proc { |x| a << x }
      end
      a = []
      [1,2,3].each &o.make_proc(a)
      a
    }

    # nil -> no block
    compare %{ 
      a = []
      p = nil
      [1,2,3].each &p
    }

    # other objects -> error
    compare %{ 
      a = []
      p = "rubbish"
      [1,2,3].each &p
    }

    # proc is evaluated before other args
    compare %{
      def (o = 'test').test(x)
	yield
      end
      a = []
      b = nil
      o.test(b=true, &(b ? proc {a << 1} : proc {a << 2}))
      a
    }

  end


  def testBlockToProc
    # capture block and call it
    compare %{
      def (o='test').test(&p)
	p.call
      end
      o.test {1}
    }
    # no block -> nil proc
    compare %{
      def (o='test').test(&p)
	p
      end
      o.test
    }
    # &arg and other args
    compare %{
      def (o='test').test(a,b=1,*c,&p)
	p.call([a,b,c])
      end
      o.test(1,2,3,4) {|ary| ary}
    }

return
    # failing tests

    # proc -> block -> proc -- p==q in MRI
    compare %{
      def (o='test').test(p, &q)
	p == q
      end
      p = proc {1}
      o.test(p, &p)
    }

    compare %{
      def (o='test').test(p, &q)
	p == q
      end
      # &p is compiled first and can't find the p local
      o.test(p = proc {1}, &p)
    }
  end


  def testPrivateProtected

    # FIXME message not always correct for following
    #    compare %q{ does_not_exist() }

    # FIXME - error message should include mention of 'local variable'
    # in this case
    # compare %q{ does_not_exist }


    # FIXME private by itself doesn't work
    #compare_class %q{
    #  class A
    #   private
    #   def private_method() "go away"; end
    #  end
    #  A.new.private_method()
    #}

    compare_class %q{
      class A
	def private_method() "go away"; end
	private :private_method
      end
      A.new.private_method()
    }

    compare_class %q{
      class A
	def private_method() "go away"; end
	private :private_method
	def calls_private() private_method(); end
      end
      A.new.calls_private()
    }

    compare_class %q{
      class A
	def private_method() "go away"; end
	private :private_method
	def calls_private_via_self() self.private_method(); end
      end
      A.new.calls_private_via_self()
    }

    compare_class %q{
      class B
	def protected_method() "go away"; end
	protected :protected_method
      end
      class C < B
	def calls_protected_in_parent()
	  self.protected_method()
	end
      end
      C.new.calls_protected_in_parent()
    }

    compare_class %q{
      class B
	def protected_method() "go away"; end
	protected :protected_method
      end
      B.new.protected_method()
    }

    # method calls without (), args or receiver (= NodeVcall)
    # .. private
    compare_class %q{
      class A
	def private_test; private_meth; end
	def private_meth; "hi";         end
	private :private_meth
      end
      A.new.private_test
    }
    # .. protected
    compare_class %q{
      class A
	def protected_test; protected_meth; end
	def protected_meth; "hi";         end
	protected :protected_meth
      end
      A.new.protected_test
    }

    # super calls to private and protected
    # FIXME why does this work?
=begin
    compare_class %{ 
      class A
	private 
	def m(a); a + 10; end
      end
      class B < A
	def m(a); super(a) + 10; end
      end
      B.new.m(100) 
    }
=end
  end


  def testFor

    # basic compilation + return value
    compare %{
      for i in [1,2,3]
      end
    }

    # local scope
    compare %{
      for i in [1,2,3]
	a = 2 * i
      end
      [a, i]
    }

    # local scope
    compare %{
      a = i = 1
      for i in [1,2,3]
	a = 2 * i
      end
      [a, i]
    }

    # local scope in nested loops
    compare %{
      for i in [1,2,3]
	a = i
	for j in [4,5,6]
	  b = j * i
	end
      end
      [a, b, i, j]
    }

    # local scope in nested loops
    compare %{
      for i in [1,2,3]
	a = i
	for j in [4,5,6]
	  b = j * i
	end
      end
      [a, b, i, j]
    }

    # each is private
    compare_class %{
      class A
	def each(); yield 1; yield 2; end
	private :each
      end
      for i in A.new
      end
    }

    # break
    compare %{
      a = []
      for i in [1,2,3]
	a << 2 * i 
	break if i == 2
      end
      [a, i]
    }

    # redo
    compare %{
      a = []
      for i in [1,2,3]
	a << 2 * i 
	redo if a.size < 3
      end
      [a, i]
    }
    
    

return 
    # FIXME error message not quite right.

    # compare_class needed because TOP_LEVEL_BINDING is polluted with
    # other locals and MRI returns the value of these
    compare_class %{
      [1,2,3].each do |i|
	a = i
	for j in [4,5,6]
	  b = j * i
	end
      end
      [a, b, i, j]      
    }

  end

  def testGlobals

    compare %{ $x }          # non-existent global
    compare %{ $x = 2; $x }  # store and load
    compare %{ $x }          # reload
    
    # Test built-in globals. These ones are taken from 'Programming
    # Ruby / The Ruby Language / Execution Environment Variables'
    compare %{ $0 }
    compare %{ $* }
    compare %{ $" }
    compare %{ $$ }
    compare %{ $? }

    # FIXME tests for other built-in vars $_ etc

return
    # These are bizarre ...
    # Regexp related
    compare %{
      "abcdefghij".match /c(.*)g/
      [$`, $&, $1, $']
    }
  end


  def testMethodMissing
return
    compare_class %q{
      class A
	def method_missing(*args)
	  "mm called"
	end
      end
      A.new.does_not_exist()
    }
  end

  def testRangeFlipFlops
return
    compare %q{
      i = 0; a = []
      while i < 10 
	a << i if i==2..i==8
	i += 1
      end
      a
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

  def testStringInterp
    return 
    # FIXME the following interpolated code has already been evaluated by 
    # the time it gets to BCR!
    compare %{ "a #{Object} b" }
    compare %{ "a #{Object.id} b" }

    # DREGX and DREGX_ONCE
    compare %q{ a=1; /#{a}/ }

  end


end

# Run if we were directly called
if $0 == __FILE__
  suite_to_run = Test::Unit::TestSuite.new('ByteCodeRuby')
  tests = TestByteCodeRuby.suite.tests
  ARGV.each do |arg|
    next if arg =~ /^-/ # ignore cli switches
    tests.each do |test|
      suite_to_run << test if test.method_name == arg
    end
  end
  suite_to_run = TestByteCodeRuby.suite if suite_to_run.size == 0

  tr = Test::Unit::UI::Console::TestRunner
  tr.run(suite_to_run, tr::VERBOSE)
end

