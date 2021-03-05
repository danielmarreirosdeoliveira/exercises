# iex --sname one -r ring2.exs -e "Ring.start :\"one\", :\"two\""
# iex --sname two -r ring2.exs -e "Node.connect(:\"one@Daniels-MacBook-Pro\")" -e "Ring.start(:two, :three)"
# iex --sname three -r ring2.exs -e "Node.connect(:\"one@Daniels-MacBook-Pro\")" -e "Ring.start(:three, :one)"
# send (:global.whereis_name :one), {:activate}

defmodule Ring do

  def start(client_name, server_name) do
    pid = spawn(__MODULE__, :loop, [client_name, server_name, false])
    :global.register_name(client_name, pid)
  end

  def loop(client_name, server_name, active) do

    receive do
      {:ping, client} ->
        IO.puts "received ping from client #{inspect client}"
        loop client_name, server_name, true
      {:activate} ->
        loop client_name, server_name, true

    after 2000 ->
      if active == true do
        if (:global.whereis_name server_name) != :undefined do
          send((:global.whereis_name server_name), {:ping, client_name})
          loop client_name, server_name, false
        end
      end
      loop client_name, server_name, active
    end
  end
end