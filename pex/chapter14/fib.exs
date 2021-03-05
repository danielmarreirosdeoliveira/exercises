defmodule FibSolver do
  def fib(scheduler) do
    send scheduler, { :ready, self() }

    receive do
      n ->
        IO.puts n*n
        send scheduler, n * n
        fib(scheduler)
    end
  end
end

defmodule Scheduler do
  def run(ar) do
    spawn(FibSolver, :fib, [self()])
    loop(ar)
  end

  def loop(ar) do
    receive do
      {:ready, pid} when length(ar) > 0 ->
        [ next | tail ] = ar
        send pid, next
        loop(tail)
      {:ready, pid} ->
        IO.puts "ready"
    end
  end
end



IO.puts inspect Scheduler.run([1,2,4])