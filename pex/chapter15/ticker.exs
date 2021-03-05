defmodule Ticker do

  @interval 2000
  @name :ticker

  def start do
    pid = spawn(__MODULE__, :generator, [[]])
    IO.puts __MODULE__
    :global.register_name(@name,pid)
  end

  def register(client_pid) do
    IO.puts "register #{inspect client_pid} at #{inspect :global.whereis_name(@name)}"
    send :global.whereis_name(@name), { :register, client_pid }
  end

  def generator(clients) do

    receive do
      { :register, client_pid } ->
        IO.puts "incoming reguest to register #{inspect client_pid}"
        generator([client_pid|clients])
      x -> IO.puts "received #{x}"
        generator(clients)
    after @interval ->
      IO.puts "tick"
      Enum.each clients, fn client ->
        send client, {:tick}
      end
      generator(clients)
    end
  end
end

