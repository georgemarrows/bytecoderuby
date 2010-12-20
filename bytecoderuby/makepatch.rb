#!/usr/local/bin/ruby

$base = "/dos2/George/ruby"
$dir = "/dos2/George/ruby/rubyvm/src/bytecoderuby"

def patch_from(file)
  base = $base
  dir  = $dir
  `diff -u #{base}/ruby-1.6.7/#{file} #{base}/ruby-1.6.7.hack/#{file} >>#{dir}/patch-1.6.7`
end

# clean out the patch file
File.open("#{$dir}/patch-1.6.7", "w").close()

['eval.c', 'node.h', 'ruby.h'].each do |file|
    patch_from(file)
end
