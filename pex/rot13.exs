defmodule Rot13 do 
	def caesar([]) do
		[]
	end
	def caesar([h|t]) when h < 110 do
		[h+13|caesar(t)]	
	end
	def caesar([h|t]) do
		[h-13|caesar(t)]	
	end

end

IO.puts Rot13.caesar('ryvkve')

