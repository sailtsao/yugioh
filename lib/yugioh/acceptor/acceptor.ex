defmodule Yugioh.Acceptor.Acceptor do  
  require Lager
  alias Yugioh.System.Login

  defrecord Client,account_id: 0,role_id: 0,logined: false,player_pid: nil

  def start_link(socket) do
    Lager.debug "new client connected at socket [~p]",[socket]
    {:ok,spawn_link(Yugioh.Acceptor.Acceptor,:parse_packet_loop, [socket,Client[]])}
  end
  
  def decode_message(cmd,bin) do
    [h1,h2,_,_,_] = integer_to_list(cmd)
    module = list_to_atom('Elixir.Yugioh.Proto.PT'++[h1,h2])
    # Lager.debug "get cmd belonged to module [~p]",[module]
    module.read(cmd,bin)
  end
  
  def do_error(socket,reason,client) do
    case reason do
      {:error,:timeout} ->
        #if it's just a timeout,not an error,then continue fetch message from socket
        parse_packet_loop(socket,client)
      other ->
        # it's an error, close the socket and die
        Lager.debug "client [~p] died by reason [~p]",[client,other]
        if is_pid(client.player_pid),do: Yugioh.Player.stop(client.player_pid)
        :gen_tcp.close(socket)
    end
  end
  
  def parse_packet_loop(socket,client) do
    case :gen_tcp.recv(socket,4,2000) do
      {:ok,<<msgLength::size(16),msgID::size(16)>>} ->
        Lager.debug "get message [~p] length [~p] from client [~p]",[msgID,msgLength,client]
        case msgLength > 4 do
          true->
            case :gen_tcp.recv(socket,msgLength-4,2000) do
              {:ok,binaryData} ->
                route_message(msgID,binaryData,socket,client)
              other ->
                do_error(socket,other,client)
            end        
          false->
            route_message(msgID,<<>>,socket,client)
        end
      other ->
        do_error(socket,other,client)
    end
  end
  
  defp route_message(msgID,binaryData,socket,client) do
    case decode_message(msgID,binaryData) do
      {:ok,:login,loginData} ->
        case Login.login(loginData,socket) do
          {:ok,account_id}->
            client = client.update(account_id: account_id,logined: true)
            parse_packet_loop(socket,client)
          {:fail,reason}->
            do_error(socket,reason,client)
        end
      {:ok,:check_role_name,name} ->
        Login.check_role_exist(name,socket)
        parse_packet_loop(socket,client)
      {:ok,:create_role,[name,avatar,card_type]} ->
        Login.create_role([client.account_id,name,avatar,card_type],socket)
        parse_packet_loop(socket,client)
      {:ok,:delete_role,name} ->
        Login.delete_role(name,socket)
        parse_packet_loop(socket,client)
      {:ok,:get_roles} ->
        Login.get_roles(client.account_id,socket)
        parse_packet_loop(socket,client)
      {:ok,:enter_game,role_id} ->
        case Login.enter_game(role_id,socket) do
          {:ok,player_pid}->
            client=client.update(player_pid: player_pid,role_id: role_id)
            parse_game_packet_loop(socket,client)
          {:fail,reason}->
            do_error(socket,reason,client)
        end
      other-> # out of pre-enter-game area message
        do_error(socket,other,client)
    end
  end

  def parse_game_packet_loop(socket,client) do
      case :gen_tcp.recv(socket,4,2000) do
      {:ok,<<msgLength::size(16),msgID::size(16)>>} ->
        Lager.debug "*** get game message *** [~p] length [~p] from client [~p]",[msgID,msgLength,client]
        case msgLength > 4 do
          true->
            case :gen_tcp.recv(socket,msgLength-4,2000) do
              {:ok,binaryData} ->
                case decode_message(msgID,binaryData) do
                  {:ok,data}->
                    case :gen_server.call(client.player_pid,{"SOCKET_EVENT",msgID,data}) do
                      :ok->
                        parse_game_packet_loop(socket,client)
                      {:error,reason}->
                        do_error(socket,reason,client)
                    end
                  other->
                    do_error(socket,other,client)
                end
              other ->
                do_error(socket,other,client)
            end        
          false->
            case decode_message(msgID,<<>>) do
              {:ok,data}->
                case :gen_server.call(client.player_pid,{"SOCKET_EVENT",msgID,data}) do
                  :ok->
                    parse_game_packet_loop(socket,client)
                  {:error,reason}->
                    do_error(socket,reason,client)
                end
              other->
                do_error(socket,other,client)
            end
        end
      other ->
        do_error(socket,other,client)
    end
  end
  
end