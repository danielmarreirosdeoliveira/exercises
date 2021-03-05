defmodule Sequencer do
  use GenServer

  def handle_call(:pop, _from, [h|t]) do
    { :reply, h, t}
  end

  def handle_cast({:push, delta}, list) do
    { :noreply, [delta|list]}
  end
end