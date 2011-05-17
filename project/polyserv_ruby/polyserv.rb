#!/usr/bin/env ruby

require 'polyproto'
require 'thread'

Dir.mkdir '/tmp/poly' unless File.exists? '/tmp/poly'
file = '/tmp/poly/poly.sock'
File.unlink(file) if File.exists?(file)
server = UNIXServer.open file

$poly = nil
$poly_lock = Mutex.new

$frompoly_streams = {}

poly_thread = Thread.new do
  loop do
    begin
      $poly_lock.synchronize do
        $poly.close unless $poly.nil?
  
        puts 'firing up poly'
        $poly = IO.popen('poly --ideprotocol', 'w+')
        resp = Poly.stream_response($poly)
        if resp[0] == 'H'
          puts "protocol version: #{resp[1]}"
        else
          puts 'ERROR: unexpected startup message from poly'
        end
        $stdout.flush
      end
  
      pid, status = Process::wait2
      puts "poly(#{pid}) exited with status #{status}"
    rescue Exception => e
      puts "poly thread crashed with: #{e.message}"
    end
  end
end

trap("SIGINT") do
  puts "\nexiting"
  poly_thread.kill
  File.unlink(file) if File.exists?(file)
end

$topoly_threads = {}

# main output thread
output_thread = Thread.new do
  begin
    ecode = false
    loop do
      begin
        # puts 'waiting for poly lock'
        $poly_lock.lock
        # puts 'waiting to get char from poly'
        c = $poly.getc
        # puts 'got char from poly'
      ensure
        # puts 'unlocking poly'
        $poly_lock.unlock
      end
    
      break if c.nil?
    
      print VTBLUE
      if c == ESC then print '[ESC]'
      else putc c
      end
      print VTCLEAR
    
      cull = []
      $frompoly_streams.each do |client,stream|
        if stream.closed?
          cull << client
        else
          begin
              stream.putc c
              stream.flush if ecode
          rescue Errno::EPIPE
            puts "caught a broken pipe from client #{client}, culling"
            cull << client
          end
        end
      end # each stream
    
      cull.each {|c| $frompoly_streams.delete c }
    
      $stdout.flush if ecode
      ecode = (c == ESC)
    end # loop
  rescue Exception => e
    puts 'error in output loop: ' + e.message
  ensure
    puts 'output thread done'
  end
end # thread


puts 'waiting for a connection'
client = 0

server_thread = Thread.new do
  begin
    while s = server.accept
      Thread.new(client,s) do |cl,sock|
        puts "connected client #{cl}"

        # write the client number out to the stream
        sock.write("\eQclient\e,#{cl}\eq")
    
        # attach this client's stream for multi-plexing
        $frompoly_streams[cl] = sock

        $topoly_threads[cl] = Thread.new do
          begin
            ecode = false
            while ((c = sock.getc) != nil)
              print VTRED
              if c == ESC then
                print '[ESC]'
              else
                putc c
                $poly.putc c unless ecode
              end
              print VTCLEAR
          
              if ecode
                if c == 'Q'[0]
                  puts 'captured a Q-code'
                  sock.ungetc c
                  resp = Poly.stream_response(sock, false)
                  Poly.stream_request(sock, 'Q', resp[1][0], 'ok')
                else
                  $poly.putc ESC
                  $poly.putc c
                end
                $stdout.flush
                $poly.flush
                sock.flush
                ecode = false
              end
    
              ecode = (c == ESC)
            end
          rescue Exception => e
            puts "client #{cl} topoly failed: #{e.message}"
          ensure
            puts "client #{cl} topoly finished"
            $frompoly_streams[cl].close
          end
        end
      end # client thread
      client += 1
    end # while server.accept
  rescue Exception => e
    puts "error in server thread near client #{client}: #{e.message}"
  ensure
    puts 'shutting down server'
    server.close
  end
end

# wait for poly thread to exit
poly_thread.join

puts "output: [#{output_thread.status}], server: [#{server_thread.status}]"

output_thread.kill
server_thread.kill
$topoly_threads.each { |client,thread| thread.kill }

puts 'closing pipe to poly'
$poly.close
puts 'bye'
