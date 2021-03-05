defmodule Parallel do

  def pmap(collection,fun) do
    me = self()
    collection
    |> Enum.map(
        fn (elem) ->
          spawn_link fn -> (send me, {self(), fun.(elem)}) end
        end
       )
    |> Enum.map(
      fn (pid) ->
        receive do {^pid, result} -> result end
      end
       )
  end

  def run do
    pmap(1..100025, &(&1*&1))
  end
end


IO.puts inspect :timer.tc(Parallel, :run ,[])