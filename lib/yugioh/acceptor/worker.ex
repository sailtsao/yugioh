
defmodule Yugioh.Acceptor.Worker do
  use GenServer.Behaviour
  
  defrecordp :record_client,[account_id: 0,login: false,role_data: nil]

  def start_link(socket) do
    :gen_server.start_link(__MODULE__,[socket],[])
    # start loop to receive message
    # spawn_link(fn->parse_packet(socket,record_client())end)    
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
    parse_packet(socket,record_client())
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
  def parse_packet(socket,client) do    
    case :gen_tcp.recv(socket,4,2000) do
      {:ok,<<msgLength::size(16),msgID::size(16)>>} ->
        case msgLength > 4 do
          true->
            case :gen_tcp.recv(socket,msgLength-4,2000) do
              {:ok,binaryData} ->                
                route_message(msgID,binaryData,socket,client)
              other ->
                check_error(socket,other,client)
            end        
          false->
            route_message(msgID,<<>>,socket,client)
        end
      other ->
        check_error(socket,other,client)
    end
  end
  
  defp route_message(msgID,binaryData,socket,client) do
    case decode_message(msgID,binaryData) do
      {:ok,:login,loginData} ->
        case Yugioh.Module.Account.login(loginData,socket) do
          {:ok,account_id}->
            client=record_client(client,account_id: account_id)
            client=record_client(client,login: true)
            parse_packet(socket,client)
          {:fail,reason}->
            check_error(socket,reason,client)
        end
      {:ok,:check_role_name,name} ->
        Yugioh.Module.Account.check_role_exist(name,socket)
        parse_packet(socket,client)
      {:ok,:create_role,[name,gender]} ->
        Yugioh.Module.Account.create_role([record_client(client,:account_id),name,gender],socket)
        parse_packet(socket,client)
      {:ok,:delete_role,name} ->
        Yugioh.Module.Account.delete_role(name,socket)
        parse_packet(socket,client)
      {:ok,:get_roles} ->
        Yugioh.Module.Account.get_roles(record_client(client,:account_id),socket)
        parse_packet(socket,client)
      {:ok,:enter_game,role_id} ->
        case Yugioh.Module.Account.enter_game(role_id,socket) do
          {:ok,role_data}->
            parse_packet(socket,client)
          {:fail,reason}->
            check_error(socket,reason,client)
        end
      {:ok,:create_room,name} ->
        Yugioh.Module.Room.create_room(name,socket)
        parse_packet(socket,client)
      {:ok,:get_rooms} ->
        Yugioh.Module.Room.get_rooms(socket)
        parse_packet(socket,client)
      {:ok,:enter_room,room_id} ->
        Yugioh.Module.Room.enter_room(room_id,socket)
        parse_packet(socket,client)
      {:ok,:battle_ready} ->
        parse_packet(socket,client)
      other-> # out of pre-enter-game area message
        check_error(socket,other,client)
    end
  end
end