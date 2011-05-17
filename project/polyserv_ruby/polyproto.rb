require 'socket'
ESC = "\e"[0]
VTRED = "\e[31;1m"
VTGREEN = "\e[32;1m"
VTYELLOW = "\e[33;1m"
VTBLUE = "\e[34;1m"
VTPURPLE = "\e[35;1m"
VTTEAL = "\e[36;1m"
VTGRAY = "\e[37;1m"
VTCLEAR = "\e[0m"

class Poly
  attr_reader :mlout, :mlin, :client
  
  def self.stream_request(stream, chr, *list)
    stream.putc ESC
    stream.putc chr.upcase
    
    fst = true
    list.each do |item|
      if fst then
        fst = false
      else 
        stream.putc ESC
        stream.putc ','
      end
      item.to_s.each_char { |c| stream.putc c }
    end
    
    stream.putc ESC
    stream.putc chr.downcase
    stream.flush
  end
  
  def self.stream_response(stream, leading_esc=true)
    list = []
    if (!leading_esc or stream.getc == ESC)
      code = "\0"
      stack = []
      loop do
        c = stream.getc.chr
        case c
        when ';'
          # pop and push a new block
          stack.pop
          current = []
          if stack.length == 0
            puts 'ERROR: stack empty unexpectedly'
            return []
          else
            stack[-1] << current
            stack.push current
          end
        when ','
          # just a separator, do nothing
        else
          if c == code.downcase
            # pop, restore old code or break the loop
            stack.pop # block
            list = stack.pop # code + blocks
            break if stack.length == 0
            code = stack[-2][0]
          else
            # push code and new block on to stack
            code = c
            block = []
            call = [c, block]
            stack[-1] << call if stack.length >= 1
            stack.push call
            stack.push block
          end
        end
        
        str = ''
        while ((c = stream.getc) != ESC)
          str << c
        end
        
        if stack.length == 0
          puts 'ERROR: found empty stack when appending string'
          return []
        else
          stack[-1] << str unless str == ''
        end
      end
    else
      puts 'ERROR: expected ESC'
    end
    
    return list
  end
  
  def self.stream_read_until_esc(stream)
    str = '';
    while ((c = stream.getc) != ESC)
      str << c
    end
    return str
  end
  
  def self.pretty_response(r,format=:plain)
    return r.to_s unless r.respond_to?(:to_ary)
    case format
    when :plain
      open_col = ''
      close_col = ''
    when :html
      open_col = '<font style="color:purple;font-weight:bold">'
      close_col = '</font>'
    when :vtcolor
      open_col = VTPURPLE
      close_col = VTCLEAR
    end
    
    str = ''
    str << open_col << r[0] << '(' << close_col
    str << r[1..r.length].map do |block|
      block.map do |s|
        Poly.pretty_response(s, format)
      end.join(open_col + ', ' + close_col)
    end.join(open_col + '; ' + close_col)
    str << open_col << ')' << close_col
    return str
  end
  
  def self.output_errors(code, resp)
    resp[2].each do |error|
  			start_char = error[1][3].to_i
  			end_char = error[1][4].to_i
  			slice = code[0..start_char]
  			line = slice.count("\n")+1
  			line_start = slice.rindex("\n")
  			line_start = 0 if line_start.nil?
  			yield(line, start_char - line_start, end_char - line_start, error[2])
    end
  end
  
  def self.output_error_message(code, message)
    message.each do |m|
      if m.respond_to?(:to_ary)
        start_char = m[1][2].to_i
  			end_char = m[1][3].to_i
  			slice = code[0..start_char]
  		  if slice.nil?
  		    yield(nil,nil,nil,m[2][0])
  		  else
  			  line = slice.count("\n") + 1
  			  line_start = slice.rindex("\n")
  			  line_start = 0 if line_start.nil?
  			  #puts "[#{line}] [#{line_start}] [#{start_char}] [#{end_char}] [#{m[2][0]}]"
  			  yield(line, start_char - line_start, end_char - line_start, m[2][0])
			  end
      else
        yield(nil,nil,nil,m)
      end
    end
  end
  
  def initialize
    # ml = IO.popen('poly --ideprotocol', 'w')
    ml = UNIXSocket.open('/tmp/poly/poly.sock')
    
    # the 'ml' object is bidirectional
    @mlout = ml
    @mlin = ml
    
    ignore, r = self.resp(nil)
    @client = r[1][1].to_i;
    @request_num = -1;
  end
  
  def fresh_id
    @request_num += 1
    return "#{@client}:#{@request_num}"
  end
  
  def req(chr, *list)
    id = self.fresh_id
    Poly.stream_request(@mlout, chr, id, *list)
    return id
  end
  
  def eval(name, cmd)
    return self.req('R', name, 0, 0, cmd.length, '', cmd)
  end
  
  def resp(id)
    loop do
      str = Poly.stream_read_until_esc(@mlin)
      r = Poly.stream_response(@mlin, false)
      return str, r if (id.nil? or r[1][0] == id)
    end
  end
  
  def close
    @mlin.close
    @mlout.close unless @mlout.closed?
  end
end

TESTCODE = "val p = 5;\n"