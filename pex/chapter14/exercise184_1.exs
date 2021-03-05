defmodule Waiter do
  import :timer, only: [ sleep: 1 ]

  def child_proc(parent) do
    receive do
      _ ->
        send parent, "Hi"
        raise "a"
        send parent, "Ho"
    end
  end

  def run do
    pid = spawn_link(Waiter,:child_proc, [self()])
    send pid, "some"

    sleep 1000
    receive do
      msg -> IO.puts "received #{inspect msg}"
    end
    receive do
      msg -> IO.puts "received #{inspect msg}"
    end
    receive do
      msg -> IO.puts "received #{inspect msg}"
    end
  end
end

Waiter.run