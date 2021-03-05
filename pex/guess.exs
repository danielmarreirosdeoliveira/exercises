defmodule Times do

	def talk(a..b,n) do
		talk(a..b,n,500)	
	end
	def talk(a..b,n,guess) when n == guess do
		IO.puts guess
	end
	def talk(a..b,n,guess) when guess > n do
		IO.puts guess
		talk(a..guess,n,a+div((guess-a),2))
	end
	def talk(a..b,n,guess) when guess < n do
		IO.puts guess
		talk(guess..b,n,b-div(b-guess,2))
	end
	def talk(a..b,n) do
		IO.inspect a
	end
end


Times.talk 1..1000,273







