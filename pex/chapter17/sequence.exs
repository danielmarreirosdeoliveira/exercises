defmodule SequenceSupervisor do
  use Supervisor

  def start_link(initial_number) do
    IO.puts "a #{initial_number}"
  end

  def init(_) do
    IO.puts "c"
  end
end

defmodule SequenceServer do
  use GenServer

  def start_link(initial_number) do
    GenServer.start_link(__MODULE__, initial_number, name: __MODULE__)
  end

  def next_number do
    GenServer.call __MODULE__, :next_number
  end

  def increment_number(delta) do
    GenServer.cast __MODULE__, {:increment_number, delta}
  end

  def handle_call(:next_number, _from, current_number) do
    { :reply, current_number, current_number + 1}
  end

  def handle_cast({:increment_number,delta},current_number) do
    { :noreply, current_number + delta}
  end
end

defmodule Sequence do
  use Application

  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    children = [
      worker(SequenceServer,[123])
    ]

    opts = [strategy: :one_for_one, name: SequenceSupervisor]
    {:ok, _pid} = Supervisor.start_link(children,opts)
  end
end

