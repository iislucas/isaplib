class Poly
  def self.textmate_errors(html, code, response)
    html.puts '<pre>'
    Poly.output_errors(code, response) do |line, col_start, col_end, message|
  			html.print "<a href=\"txmt://open?line=#{line}&column=#{col_start}\">"
    			html.print "Line #{line} [#{col_start}-#{col_end}]:</a> "
    			Poly.output_error_message(code, message) do |line,col_start,col_end,msg|
  				if line.nil?
  					html.print msg
  				else
  					html.print "<a href=\"txmt://open?line=#{line}&column=#{col_start}\">#{msg}</a>"
  				end
  			end
  			html.puts
  		end
  		
  		html.puts '</pre>'
		end
end