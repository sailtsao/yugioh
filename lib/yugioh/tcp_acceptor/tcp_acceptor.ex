defmodule TcpAcceptor do
  require Lager
  use ExActor.GenServer  

  definit listen_socket do
    accept_cast self
    initial_state listen_socket
  end
  
  defcast accept_cast,state: listen_socket do
    accept_loop(listen_socket)
  end

  definfo {:inet_async, listen_socket, _ref, {:error, :closed}},state: listen_socket do
    {:stop, :normal, listen_socket}
  end

  definfo {:inet_async, listen_socket, _ref, {:ok, socket}},state: listen_socket do
    case set_sockopt(listen_socket,socket) do
      :ok->
        :ok
      {:error,reason}->
        exit({:set_sockopt,reason})
    end
    start_client(socket)
    accept_loop(listen_socket)
  end

  def accept_loop(listen_socket) do
    case :prim_inet.async_accept(listen_socket, -1) do
      {:ok, _} ->
        noreply
      error -> 
        {:stop, {:cannot_accept, error}, listen_socket}
    end
  end
  
  def terminate(reason,listen_socket) do
    :gen_tcp.close(listen_socket)
  end
  
  def set_sockopt(listen_socket, socket) do
    true = :inet_db.register_socket(socket, :inet_tcp)
    case :prim_inet.getopts(listen_socket, [:active, :nodelay, :keepalive, :delay_send, :priority, :tos]) do
      {:ok, opts} ->
        case :prim_inet.setopts(socket, opts) do
          :ok -> 
            :ok
          error -> 
            :gen_tcp.close(socket)
            error
        end
      error ->
        :gen_tcp.close(socket)
        error
    end
  end

  def start_client(socket) do
    {:ok,client_pid} = :supervisor.start_child(ClientSup,[])
    :ok = :gen_tcp.controlling_process(socket,client_pid)
    send client_pid,{:go,socket}
  end
  
end