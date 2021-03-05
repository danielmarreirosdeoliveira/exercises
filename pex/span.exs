defmodule Span do
	def go(l,l)  do
		[l]
	end
	def go(l,r) do
		[l|go(l+1,r)]
	end
end

IO.inspect Span.go(2,5)
