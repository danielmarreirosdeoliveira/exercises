defmodule Max do
	
	def go(l), do: maxi(l,0)

	defp maxi([],m) do
		m
	end
	
	defp maxi([h|t],m) do
		maxi(t,max(h,m))
	end
end

IO.puts Max.go([16,3,7,4])
