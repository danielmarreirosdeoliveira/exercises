defmodule Link1 do
  import :timer, only: [ sleep: 1]

  def sad_function do
    sleep 500
    exit(:boom)
  end

  def run do
#    Process.flag(:trap_exit, true);
    pid = spawn_link(Link1, :sad_function, [])
    Process.monitor pid
    receive do
      msg ->
        IO.puts "msg received: #{inspect msg}"
    after 1000 ->
      IO.puts "nothing happened"
    end
  end
end

Link1.run