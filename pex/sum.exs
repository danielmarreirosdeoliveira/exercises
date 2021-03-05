defmodule Sum do
	def sum([]), do: 0
	def sum([h|t]), do: h+sum(t)
end

IO.puts Sum.sum([1,2,4])

