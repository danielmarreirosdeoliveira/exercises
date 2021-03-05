defmodule Spawn1 do
  def greet(a) do
    receive do
      {sender, msg} -> send sender, {:ok, "Hello, #{a} #{msg}"}
                       greet(a)
    end
  end
end

pid = spawn(Spawn1, :greet, ["abc"])

send pid, {self(), "World!"}
receive do
  {:ok, message} ->
    IO.puts message
end
