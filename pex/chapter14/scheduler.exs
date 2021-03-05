defmodule Server do
  def calc(scheduler) do
    send scheduler, {:ready, self()}

    receive do
      {:shutdown} ->
        exit(:normal)
      {msg} ->
        :timer.sleep(2000)
        send scheduler, {:answer, String.capitalize( msg)}
        calc(scheduler)
    end
  end
end

defmodule Client do

  def run(ar) do

    1..8
    |> Enum.map(fn(_) -> spawn(Server, :calc, [self()]) end)
    |> loop(ar, [])
  end

  def loop(pids, ar, results) do
    receive do
      {:ready, client} when length(ar) > 0 ->
        [h|t] = ar
        send client, {h}
        loop(pids, t,results)

      {:ready, client} ->
        send client, {:shutdown}
        if length(pids) > 1 do
          loop(List.delete(pids,client), ar, results)
        else
          Enum.reverse results
        end

      {:answer, msg} ->
        loop(pids, ar,[msg|results])
    end
  end

end

IO.puts inspect :timer.tc(Client,:run,[["hi","ho","ha","hu","le","la","lu","li"]])