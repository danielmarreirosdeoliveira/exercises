lp = with {4,x} <- {4,1+1-1},
          x = x*2
     do
       x*2
     end


IO.puts lp
