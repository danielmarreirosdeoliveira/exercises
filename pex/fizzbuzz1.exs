fb = fn
	{ 0, 0, _ } -> "FizzBuzz"
	{ 0, _, _ } -> "Fizz"
	{ _, 0, _ } -> "Buzz"
	{ _, _, x } -> x 
end

IO.puts fb.({0,0,0})
IO.puts fb.({0,1,0})
IO.puts fb.({2,0,0})
IO.puts fb.({7,7,8})


fb1 = fn (n) -> fb.({rem(n,3),rem(n,5),n}) end


IO.puts fb1.(10)
IO.puts fb1.(11)
IO.puts fb1.(12)
IO.puts fb1.(13)
IO.puts fb1.(14)
IO.puts fb1.(15)
IO.puts fb1.(16)
IO.puts fb1.(17)

