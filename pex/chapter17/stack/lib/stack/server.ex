defmodule Stack.Server do
  use GenServer

  ####

  def start_link(current_number) do
    GenServer.start_link(__MODULE__, current_number, name: __MODULE__)
  end

  def push(val) do
    GenServer.cast __MODULE__, {:push, val}
  end

  def pop do
    GenServer.call __MODULE__, :pop
  end

  ####

  def handle_call(:pop, _from, [h|t]) do
    IO.puts "h: #{inspect [h|t]}"
    { :reply, h, t, 2000 }
  end

  def handle_cast({:push, val}, stack) do
    { :noreply, [val|stack]}
  end

  def terminate(reason, state) do
    IO.puts "terminate reason #{inspect reason}"
  end

  def handle_info(info, state) do
    IO.puts "i #{inspect info}"
    {:noreply,[3]}
  end
end