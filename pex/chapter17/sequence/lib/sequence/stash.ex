defmodule Sequence.Stash do

  use GenServer

  def start_link(current_number) do
    { :ok, pid } = GenServer.start_link(__MODULE__, current_number)
  end

  def put(stash, value) do
    GenServer.cast stash,{:put, value}
  end

  def get(stash) do
    GenServer.call stash, :get
  end

  ##

  def handle_call(:get, _from, state) do
    {:reply, state, state}
  end

  def handle_cast({:put, new_state}, _current_state) do
    {:noreply, new_state}
  end
end