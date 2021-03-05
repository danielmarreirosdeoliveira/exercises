defmodule Ticker do

  @interval 2000
  @name :ticker

  def start do
    pid = spawn(__MODULE__, :generator, [[],[]])
    IO.puts __MODULE__
    :global.register_name(@name,pid)
  end

  def register(client_pid) do
    IO.puts "register #{inspect client_pid} at #{inspect :global.whereis_name(@name)}"
    send :global.whereis_name(@name), { :register, client_pid }
  end

  def generator(clients,[current_client|remaining_clients]) do
    IO.puts "a"
    generator_impl(clients, remaining_clients, current_client)
  end

  def generator([current_client|rest],[]) do
    IO.puts "b"
    generator_impl([current_client|rest], rest, current_client)
  end

  def generator([],[]) do
    IO.puts "c"
    generator_impl([], [], :none)
  end


  defp generator_impl(clients,current_clients,client) do

    receive do
      { :register, client_pid } ->
        IO.puts "incoming reguest to register #{inspect client_pid}"
        generator([client_pid|clients],current_clients)
      x -> IO.puts "received #{x}"
           generator(clients,current_clients)
    after @interval ->
      IO.puts "#{inspect clients} - #{inspect current_clients} - #{inspect client}"

      if client != :none do
        send client, {:tick}
      end
      generator(clients,current_clients)
    end
  end
end

