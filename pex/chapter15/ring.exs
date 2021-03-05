defmodule Ring do

  def start_server(server_name,active) do
    pid = spawn(Ring,:server_loop,[server_name,:none,active])
    :global.register_name(server_name,pid)
  end

  def server_loop(server_name,args,active) do
    receive do
      {:register,client_name} ->
        server_loop server_name,client_name,active
      {:tock,source} ->
        IO.puts "received tock . from #{source}"
        server_loop server_name,args,:true
    after 2000 ->

      if args != :none do
        client = :global.whereis_name args
        if client != :undefined do
          case active do
            :true ->
              send client, {:tock,server_name}
              server_loop server_name,args,false
            false ->
          end
        end
      end
      server_loop server_name,args,active
    end
  end

  def register(server_name, client_name) do
    send((:global.whereis_name server_name), {:register,client_name})
  end
end