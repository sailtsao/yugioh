defmodule Client do
  require Lager

  defrecord ClientRecord,user_id: 0,player_pid: nil

  def start_link do
    {:ok,spawn_link(__MODULE__,:init,[])}
  end
  
  def init do
    :erlang.process_flag(:trap_exit,true)
    client = ClientRecord.new
    receive do
      {:go,socket}->
        parse_packet_loop(socket, client)
    end
  end
  
  def async_recv(socket, length, timeout) when is_port(socket) do
    case :prim_inet.async_recv(socket, length, timeout) do
      {:error, reason} ->
        throw({reason})
      {:ok, res} ->
        res
      res ->
        res
    end
  end

  def parse_packet_loop(socket,client) do
    ref = async_recv(socket,4,60000)
    receive do
      {:inet_async, socket, ref, {:ok, <<message_length::16, message_id::16>>}} ->
        case message_length do
          x when x>4 ->
            ref = async_recv(socket, message_length-4, 1000)
            receive do
              {:inet_async, socket, ref, {:ok, binary_data}} ->
                route_message(message_id,binary_data,socket,client)
              {:inet_async, socket, ref, {:error,:timeout}} ->        
                disconnect_client(message_id, {:error,:timeout}, socket, client)
            end
          _ ->
            route_message(message_id,<<>>,socket,client)
        end
      {:inet_async, socket, ref, {:error,:timeout}} ->        
        disconnect_client(socket, client, 0, {:error,:timeout})
    end
  end

  def route_message(message_id,binary_data,socket,client = ClientRecord[user_id: 0])
  when message_id != 10000 and message_id != 10007 do
    disconnect_client(message_id,:invalid_user_id_message,socket,client)
  end
  

  def route_message(message_id,binary_data,socket,client = ClientRecord[player_pid: nil]) do
    {result,client} = Login.socket_event(message_id,binary_data,socket,client)
    case result do
      :ok ->
        parse_packet_loop(socket,client)
      {:fail,_reason} ->
        parse_packet_loop(socket,client)
      {:error,reason} ->
        disconnect_client(message_id,reason,socket,client)
    end    
  end

  def route_message(message_id,binary_data,socket,client) do
    case Player.socket_event(client.player_pid,message_id,binary_data) do
      :ok ->
        parse_packet_loop(socket,client)
      {:fail,_reason} ->
        parse_packet_loop(socket,client)
      {:error,reason} ->
        disconnect_client(message_id,reason,socket,client)
    end
  end

  def disconnect_client(message_id,reason,socket,client = ClientRecord[player_pid: nil]) do
    Lager.error "message_id [~p], reason [~p], client [~p] died",[message_id,reason,client]
    :gen_tcp.close(socket)
    exit(reason)
  end

  def disconnect_client(message_id,reason,socket,client) do
    Lager.error "message_id [~p], reason [~p], client [~p] died",[message_id,reason,client]
    # Login.logout(client.player_pid)
    exit(reason)
  end
end