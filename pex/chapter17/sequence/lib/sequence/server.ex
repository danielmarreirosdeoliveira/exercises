defmodule Sequence.Server do
  use GenServer

  ## public

  def start_link(stash) do
    IO.puts "start link in server"
    GenServer.start_link __MODULE__, stash, name: __MODULE__
  end

  def next() do
    GenServer.call __MODULE__, :next
  end

  def next(delta) do
    GenServer.cast __MODULE__, {:next, delta}
  end

  ## private

  def init(stash) do
    current_state = Sequence.Stash.get stash
    IO.puts "current state fetched from statsh #{current_state}"
    { :ok, {stash, current_state} }
  end

  def terminate(_reason, {stash, state}) do
    IO.puts "saving state in stash #{state}"
    Sequence.Stash.put stash, state
  end

  def handle_call(:next,_from,{stash, state}) do
    {:reply, state, {stash,state + 1}}
  end

  def handle_cast({:next, delta},{stash, state}) do
    {:noreply, {stash,state+delta}}
  end

end