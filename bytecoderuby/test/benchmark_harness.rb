$: << 'test'

require 'pp'
require 'testByteCodeRuby'

dir   = "x"
fname = "#{dir}/00000"
tests = TestByteCodeRuby.instance_methods(true).sort.grep(/^test/)

Dir.mkdir(dir) rescue nil

# run until stopped
while true
  results = {}

  # for each test
  tests.each do |meth|

    # run the test
    %x{ruby -W0 test/testByteCodeRuby.rb -bench #{meth}}

    result = nil
    begin
      if $? == 0
	# fetch and store the results
	File.open("results") do |f|
	  result = Marshal.load(f)
	end
      else
	result = $?
      end
    rescue Exception => e
      result = e
    end

    results[meth] = result
  end

  # write the complete set of test results to disk
  File.open(fname, "w") do |f|
    Marshal.dump(results, f)
  end
  fname.succ!
end




