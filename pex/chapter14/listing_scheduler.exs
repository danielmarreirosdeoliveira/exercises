defmodule Server do
  def calc(scheduler, fun) do
    send scheduler, {:ready, self()}

    receive do
      {:shutdown} ->
        exit(:normal)
      {file} ->
        send scheduler, {:answer, fun.(file)}
        calc(scheduler, fun)
    end
  end
end

defmodule Client do

  def run(ar, fun) do

    1..2
    |> Enum.map(fn(_) -> spawn(Server, :calc, [self(), fun]) end)
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



defmodule Cat do

  def search(file) do
    words = String.split((File.read! "./testfiles/#{file}")," ",trim: true)
    i = for word when (word ==  "end\n" or word == "do\n" or word == "def") <- words, into: [], do: word
    length(i)
  end
end


IO.puts inspect :timer.tc(Client,:run,[(File.ls! "./testfiles"),&Cat.search/1])