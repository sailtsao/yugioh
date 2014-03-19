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
    async_recv(socket,4,70000)
    receive do
      {:inet_async, socket, _ref, {:ok, "<pol"}} ->
        async_recv(socket, 23-4, 1000)
        :gen_tcp.send socket,"<cross-domain-policy><allow-access-from domain='*' to-ports='1234' /></cross-domain-policy>"
        :gen_tcp.close(socket)
      {:inet_async, socket, _ref, {:ok, <<message_length::16, message_id::16>>}} ->
        case message_length do
          x when x>4 ->
            async_recv(socket, message_length-4, 1000)
            receive do
              {:inet_async, socket, _ref, {:ok, binary_data}} ->
                route_message(message_id,binary_data,socket,client)
              {:inet_async, socket, _ref, {:error,:timeout}} ->        
                disconnect_client(message_id, {:error,:timeout}, socket, client)
              {:inet_async, socket, _ref, {:error,:closed}} ->        
                disconnect_client(0, {:error,:closed},socket, client)
            end
          _ ->
            route_message(message_id,<<>>,socket,client)
        end
      {:inet_async, socket, _ref, {:error,:timeout}} ->        
        disconnect_client(0, {:error,:timeout},socket, client)
      {:inet_async, socket, _ref, {:error,:closed}} ->        
        disconnect_client(0, {:error,:closed},socket, client)
      other ->
        disconnect_client(0, other,socket, client)
    end
  end

  def route_message(9999,_,socket,client) do
    message_data = ProtoUtil.pack(9999,<<Time.now(:msec)::float>>)
    :gen_tcp.send(socket,message_data)
    parse_packet_loop(socket,client)
  end
  
  # the login & web login message should have a 0 user_id
  def route_message(message_id,_,socket,client = ClientRecord[user_id: 0])
  when message_id != 10000 and message_id != 10007 do
    disconnect_client(message_id,:invalid_user_id_message,socket,client)
  end
  
  # before enter game, the message should have a nil player_pid 
  def route_message(message_id,binary_data,socket,client = ClientRecord[player_pid: nil]) do
    {result,client} = System.Login.socket_event(message_id,binary_data,socket,client)
    case result do
      :ok ->
        parse_packet_loop(socket,client)
      {:fail,_reason} ->
        parse_packet_loop(socket,client)
      {:error,reason} ->
        disconnect_client(message_id,reason,socket,client)
    end    
  end

  # after enter game, the player_pid become not nil
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
    :gen_tcp.close(socket)
    case reason do
      x when x in [{:error,:timeout},{:error,:closed}]->
        Lager.debug "message_id [~p], reason [~p], client [~p] died",[message_id,reason,client]        
        exit(:normal)
      _->
        Lager.error "message_id [~p], reason [~p], client [~p] died",[message_id,reason,client]
        exit(reason)
    end    
  end

  def disconnect_client(message_id,reason,_socket,client) do
    System.Login.logout(client.player_pid)
    case reason do
      x when x in [{:error,:timeout},{:error,:closed}]->
        Lager.debug "message_id [~p], reason [~p], client [~p] died",[message_id,reason,client]
        exit(:normal)
      _->
        Lager.error "message_id [~p], reason [~p], client [~p] died",[message_id,reason,client]        
        exit(reason)
    end
  end
end