#!/usr/bin/env ruby

require 'polyproto'

srand
file = '/tmp/poly/heap_'
8.times { file << (65 + rand(25)) }
puts file

begin
  puts "writing #{file}.ML"
  f = File.open("#{file}.ML", 'w')
  f.puts "PolyML.SaveState.loadState \"#{file}.poly-heap\";"
  f.close

  puts "writing #{file}.poly-heap"
  p = Poly.new
  rid = p.eval('*shell_prepare*',
    "PolyML.SaveState.saveState \"#{file}.poly-heap\";\n")
  
  puts "waiting for ok from poly"
  p.resp(rid)
  p.close

  puts "launching poly"
  system("poly --use '#{file}.ML'")
ensure
  puts "\ncleaning up"
  File.delete "#{file}.ML"
  File.delete "#{file}.poly-heap"
end
