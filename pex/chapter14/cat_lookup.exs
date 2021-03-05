defmodule Cat do
  def run do
    files = File.ls! "./testfiles"
    search files
  end

  def search([]) do
    IO.puts "end"
  end

  def search([h|t]) do
    words = String.split((File.read! "./testfiles/#{h}")," ",trim: true)

    i = for word when (word ==  "end\n" or word == "do\n" or word == "def") <- words, into: [], do: word
    IO.inspect i

    search(t)
  end
end

IO.puts inspect :timer.tc(Cat,:run,[])