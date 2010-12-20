#!/usr/local/bin/ruby

$ruby_ver   = "1.8.0"

$base = "/dos2/George/ruby/build"
$dir = "/dos2/George/ruby/bytecoderuby"

$patch_file = "#{$dir}/patch-#{$ruby_ver}"

def patch_from(file)
  original = "#{$base}/ruby-#{$ruby_ver}/#{file}"
  modified = "#{$base}/ruby-#{$ruby_ver}.hack/#{file}"
  `diff -u #{original} #{modified} >>#{$patch_file}`
end

# clean out the patch file
File.open("#{$patch_file}", "w").close()

['eval.c', 'node.h', 'ruby.h', 'gc.c'].each do |file|
    patch_from(file)
end
