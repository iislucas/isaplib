#!/usr/bin/env ruby

require 'polyproto'
require 'readline'

$poly = Poly.new
puts "i'm client #{$poly.client}"

$verbose = (ARGV.include?('-v') or ARGV.include?('--verbose'))

def finish
  puts "\nclosing socket"
  puts "\nexiting"
  exit 0
end

trap("SIGINT") do
  finish()
end

def print_response(resp)
  case resp[1][2]
  when 'S'
    puts VTGREEN + 'ok'
  when 'X'
    puts VTRED + 'exception'
  when 'L'
    puts VTRED + 'prelude failed'
  when 'F'
    puts VTRED + "parse/typecheck failure"
    resp[2].each do |error|
      puts 'ERROR: ' + error[2][0]
    end
  when 'C'
    puts VTRED + 'cancelled'
  else
    puts VTBLUE + 'unknown'
  end
  print VTCLEAR
  
  puts Poly.pretty_response(resp, :vtcolor) if $verbose
end

Thread.new do
  loop do
    $stdout.flush
    cmd = Readline.readline(VTBLUE + 'poly:> ' + VTCLEAR)
    break if cmd == nil
    if (cmd.length == 0 or cmd[0] != '!'[0])
      request_id = $poly.eval('*console*', cmd)
      string, resp = $poly.resp(request_id)
      print string
      print_response(resp)
    else
      request_id = $poly.req('Q', 'test')
      string, resp = $poly.resp(request_id)
      puts Poly.pretty_response(resp, :vtcolor)
    end
  end
end.join

finish()
