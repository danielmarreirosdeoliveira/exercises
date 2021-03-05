defmodule SpawnChain do

  def counter(next_pid) do
    receive do
      n -> send next_pid, n+1
    end
  end

  def create_procs(n) do
    last = Enum.reduce 1..n, self(),
                       fn (_, send_to) ->
                         spawn(SpawnChain, :counter, [send_to])
                       end

    send last, 0

    receive do
      final_answer when is_integer(final_answer) ->
        IO.puts "Result is #{inspect(final_answer)}"
    end
  end

  def run(n) do
    IO.puts inspect :timer.tc(SpawnChain, :create_procs, [n])
  end
end