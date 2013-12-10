
defmodule Yugioh.Acceptor.Worker do
  use GenServer.Behaviour
  
  defrecordp :record_player,[account_id: 0,login: false,role_data: nil]

  def start_link(socket) do
    :gen_server.start_link(__MODULE__,[socket],[])
    # start loop to receive message
    # spawn_link(fn->parse_packet(socket,record_player())end)    
  end

  def init(socket) do
    :gen_server.cast(self,:accept)
    {:ok,socket}
  end

  def handle_call(_msg, _from, state) do
    reply = :ok
    {:reply, reply, state}
  end

  def handle_cast(:accept,[socket]) do
    parse_packet(socket,record_player())
    # IO.puts "stop"
    {:stop,:normal,socket}
  end 

  def handle_cast(_msg, state) do
    {:noreply, state}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end
  
  def terminate(_reason,state) do
    # IO.puts "terminate"
    :ok
  end
  
  def code_change(_oldVsn, state, _extra) do
    {:ok, state}
  end  

  def decode_message(cmd,bin) do
    [h1,h2,_,_,_] = integer_to_list(cmd)
    module = list_to_atom('Elixir.Yugioh.Proto.PT'++[h1,h2])
    module.read(cmd,bin)
  end
  
  def check_error(socket,reason,c) do
    case reason do
      {:error,:timeout} ->
        parse_packet(socket,c)
      other ->
        :gen_tcp.close(socket)
    end
  end
  
  # first 
  def parse_packet(socket,player) do    
    case :gen_tcp.recv(socket,4,2000) do
      {:ok,<<msgLength::size(16),msgID::size(16)>>} ->
        case msgLength > 4 do
          true->
            case :gen_tcp.recv(socket,msgLength-4,2000) do
              {:ok,binaryData} ->                
                route_message(msgID,binaryData,socket,player)
              other ->
                check_error(socket,other,player)
            end        
          false->
            route_message(msgID,<<>>,socket,player)
        end
      other ->
        check_error(socket,other,player)
    end
  end
  
  defp route_message(msgID,binaryData,socket,player) do
    case decode_message(msgID,binaryData) do
      {:ok,:login,loginData} ->
        case Yugioh.Module.Account.login(loginData,socket) do
          {:ok,account_id}->
            player=record_player(player,account_id: account_id)
            player=record_player(player,login: true)
            parse_packet(socket,player)
          {:fail,reason}->
            check_error(socket,reason,player)
        end
      {:ok,:check_role_name,name} ->
        Yugioh.Module.Account.check_role_exist(name,socket)
        parse_packet(socket,player)
      {:ok,:create_role,[name,avatar,card_type]} ->
        Yugioh.Module.Account.create_role([record_player(player,:account_id),name,avatar,card_type],socket)
        parse_packet(socket,player)
      {:ok,:delete_role,name} ->
        Yugioh.Module.Account.delete_role(name,socket)
        parse_packet(socket,player)
      {:ok,:get_roles} ->
        Yugioh.Module.Account.get_roles(record_player(player,:account_id),socket)
        parse_packet(socket,player)
      {:ok,:enter_game,role_id} ->
        case Yugioh.Module.Player.enter_game(role_id,socket) do
          {:ok,role_data}->
            IO.inspect role_data
            player=record_player(player,role_data: role_data)
            parse_packet(socket,player)
          {:fail,reason}->
            check_error(socket,reason,player)
        end
      {:ok,:create_room,name} ->
        Yugioh.SharedModule.Room.create_room(name,socket)
        parse_packet(socket,player)
      {:ok,:get_rooms} ->
        Yugioh.SharedModule.Room.get_rooms(socket)
        parse_packet(socket,player)
      {:ok,:enter_room,room_id} ->
        Yugioh.SharedModule.Room.enter_room(room_id,socket)
        parse_packet(socket,player)
      {:ok,:battle_ready} ->
        parse_packet(socket,player)
      other-> # out of pre-enter-game area message
        check_error(socket,other,player)
    end
  end
end