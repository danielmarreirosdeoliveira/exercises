defmodule Sequence.Server do
  use GenServer

  def format_status(_reason, [_pdict, state ]) do
    [data: [{'State', "My current '#{inspect state}'"}]]
  end

  def handle_call(:next_number, _from, current_number) do
    { :reply, current_number, current_number + 3 }
  end

  def handle_cast({:increment_number, delta}, current_number) do
    { :noreply, current_number + delta}
  end
end