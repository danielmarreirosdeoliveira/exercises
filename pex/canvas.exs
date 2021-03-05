defmodule Canvas do

@defaults [fg: "black", bg: "white"]

def draw_text(options \\ []) do
	options = Keyword.merge(@defaults,options)
	IO.puts "Foreground #{inspect Keyword.get_values(options, :fg)}"
	IO.puts "Background #{options[:bg]}"
end

end

Canvas.draw_text(fg: "red",fg: "blue")
