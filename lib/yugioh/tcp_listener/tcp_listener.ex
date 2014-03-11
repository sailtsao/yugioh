defmodule TcpListener do  
  require Lager
  use ExActor.GenServer  

  definit [acceptor_count,port] do
    :erlang.process_flag(:trap_exit,true)
    tcp_options = [:binary, {:packet, 0}, {:active, false}, {:reuseaddr, true},
    {:nodelay, false}, {:delay_send, true}, {:send_timeout, 5000}, {:keepalive, true}, {:exit_on_close, true}]
    case :gen_tcp.listen(port, tcp_options) do
        {:ok, listen_socket} ->
          Enum.each List.duplicate(1,acceptor_count),fn(_)->
            {:ok, _} = :supervisor.start_child(TcpAcceptorSup, [listen_socket])
          end
          {:ok, listen_socket}
        {:error, reason} ->
            {:stop, {:cannot_listen, reason}}
    end
    initial_state port
  end

  def terminate(reason,listen_socket) do
    :gen_tcp.close(listen_socket)
  end
  
end