people = [
	%{ name: "Grumpy", height: 1.24,width: 1.2},
	%{ name: "Cat", height: 1.18},
]

defmodule HotelRoom do
	def book(%{name: name,height: height, width: width}) when height > 0 do
		IO.puts name
		IO.puts width 
	end
	def book(%{name: name,height: he}) when he > 0 do
		IO.puts name
		IO.puts name
	end
	def book(a) do
		IO.inspect a
		IO.puts "nobook"
	end
end

people |> Enum.each(&HotelRoom.book/1)
