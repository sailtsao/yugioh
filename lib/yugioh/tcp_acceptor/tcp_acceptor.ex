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
# defmodule TcpAcceptor do  
#   require Lager
#   alias Yugioh.System.Login

#   defrecord Client,user_id: 0,role_id: 0,logined: false,player_pid: nil

#   def start_link(socket) do
#     Lager.debug "new client connected at socket [~p]",[socket]
#     {:ok,spawn_link(Yugioh.Acceptor.Acceptor,:parse_packet_loop, [socket,Client[]])}
#   end
  
#   def decode_message(cmd,bin) do
#     [h1,h2,_,_,_] = integer_to_list(cmd)
#     module = list_to_atom('Elixir.Yugioh.Proto.PT'++[h1,h2])
#     module.read(cmd,bin)
#   end
  
#   def do_error(socket,reason,client) do
#     Lager.error "client [~p] died by reason [~p]",[client,reason]
#     if is_pid(client.player_pid),do: Yugioh.Player.stop_cast(client.player_pid)
#   end

#   def parse_packet_loop(socket,client) do
#     case :gen_tcp.recv(socket,4,2000) do
#       {:ok,<<msgLength::16,msgID::16>>} ->
#         Lager.debug "get message [~p] length [~p] from client [~p]",[msgID,msgLength,client]
#         case msgLength do
#           x when x > 4->
#             case :gen_tcp.recv(socket,msgLength-4,2000) do
#               {:ok,binaryData} ->
#                 route_message(msgID,binaryData,socket,client)
#               other ->
#                 do_error(socket,other,client)
#             end        
#           _->
#             route_message(msgID,<<>>,socket,client)
#         end
#       {:error,:timeout} ->
#         parse_packet_loop(socket,client)
#       other ->
#         do_error(socket,other,client)
#     end
#   end
  
#   def route_message(msgID,binaryData,socket,client) do
#     {:ok,message,params} = decode_message(msgID,binaryData)
#     case {message,params} do
#       {:login,[account,password]} ->
#         case Login.login([account,password],socket) do
#           {:ok,user_id}->
#             client = client.update(user_id: user_id,logined: true)
#             parse_packet_loop(socket,client)
#           {:fail,_reason}->
#             parse_packet_loop(socket,client)
#           {:error,reason}->
#             do_error(socket,reason,client)
#         end
#       {:web_login,[user_id,auth_string]} ->
#           case Login.web_login([user_id,auth_string],socket) do
#             {:ok,user_id}->
#               client = client.update(user_id: user_id,logined: true)
#               parse_packet_loop(socket,client)
#             {:fail,_reason}->
#               parse_packet_loop(socket,client)
#             {:error,reason}->
#               do_error(socket,reason,client)
#           end
#       {:check_role_name,name} ->
#         Login.check_role_exist(name,socket)
#         parse_packet_loop(socket,client)
#       {:create_role,[name,avatar,card_type]} ->
#         Login.create_role([client.user_id,name,avatar,card_type],socket)
#         parse_packet_loop(socket,client)
#       {:delete_role,name} ->
#         Login.delete_role(name,socket)
#         parse_packet_loop(socket,client)
#       {:get_roles,[]} ->
#         Login.get_roles(client.user_id,socket)
#         parse_packet_loop(socket,client)
#       {:enter_game,role_id} ->
#         case Login.enter_game(role_id,socket) do
#           {:ok,player_pid}->
#             client=client.update(player_pid: player_pid,role_id: role_id)
#             parse_game_packet_loop(socket,client)
#           {:fail,reason}->
#             do_error(socket,reason,client)
#         end
#       other-> # we only process these message above
#         do_error(socket,{:out_of_pre_entergame_area_message,other},client)
#     end
#   end

#   def process_message_packet(msgLength,msgID,socket,client) when msgLength<4 do
#     do_error(socket,{:messag_length_invalid,msgID,msgLength},client)
#   end

#   def process_message_packet 4,msgID,socket,client do
#     {:ok,data} = decode_message(msgID,<<>>)
#     route_message_to_player client,socket,msgID,data
#   end

#   def process_message_packet msgLength,msgID,socket,client do
#     case :gen_tcp.recv(socket,msgLength-4,2000) do
#       {:ok,binaryData} ->
#         {:ok,data} = decode_message(msgID,binaryData)
#         route_message_to_player client,socket,msgID,data
#       {:error,:timeout} ->
#         parse_game_packet_loop(socket,client)  
#       {:error,reason}->
#         do_error(socket,reason,client)
#     end
#   end

#   def parse_game_packet_loop(socket,client) do
#     case :gen_tcp.recv(socket,4,2000) do
#       {:ok,<<msgLength::size(16),msgID::size(16)>>} ->
#         Lager.debug "*** get game message *** [~p] length [~p] from client [~p]",[msgID,msgLength,client]
#         process_message_packet msgLength,msgID,socket,client
#       {:error,:timeout} ->
#         parse_game_packet_loop(socket,client)  
#       {:error,reason}->
#         do_error(socket,reason,client)
#     end
#   end
  
#   def route_message_to_player client,socket,msgID,data do
#     Yugioh.Player.socket_event(client.player_pid,msgID,data)
#     parse_game_packet_loop(socket,client)
#   end

# end