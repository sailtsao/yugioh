defmodule Yugioh.System.Room do
  require Lager
  alias Yugioh.Singleton.Room
  alias Yugioh.Proto.PT11

  def handle({:create_room,name,type},player_state) do
    case player_state.in_room_id do
      0 ->
        {:ok,room_info} = Room.create_room(name,type)
        spawn(fn-> 
          :gen_tcp.send(player_state.socket,PT11.write(11000,[1,room_info])) 
        end )
        {:ok,player_state.update(in_room_id: room_info.id)}
      other ->
        # spawn(fn-> :gen_tcp.send(player_state.socket,PT11.write(11000,[0,RoomInfo.new])) end)
        {:error,:already_in_room}
    end
  end
  
  def handle(:get_rooms,player_state) do
    {:ok,rooms} = Room.get_rooms
    rooms_data = Enum.map(rooms,fn(room_info)-> 
      <<
      room_info.id::size(32),
      size(room_info.name)::size(16),room_info.name::bitstring, 
      room_info.type::size(16)
      >> 
    end)
    data = <<length(rooms)::size(16),iolist_to_binary(rooms_data)::binary>>
    :gen_tcp.send(player_state.socket,PT11.write(11001,data))
    {:ok,player_state}
  end

  def handle({:enter_room,room_id},player_state) do
    case player_state.in_room_id do
      0 ->
        {:ok,room_info} = Room.enter_room(room_id)
        spawn(fn -> :gen_tcp.send(player_state.socket,PT11.write(11002,[1,room_info])) end)
        {:ok,player_state.update(in_room_id: room_id)}
      other ->
        # spawn(fn -> :gen_tcp.send(player_state.socket,PT11.write(11002,[0,RoomInfo.new])) end)
        {:error,:already_in_room}
    end
  end

  def handle(:leave_room,player_state) do
    case player_state.in_room_id do
      0 ->
        {:error,:not_in_room}
      other ->        
        case Room.leave_room(player_state.in_room_id) do
          :ok->
            :gen_tcp.send(player_state.socket,PT11.write(11004,1))
            {:ok,player_state.update(in_room_id: 0)}
          reason->
            {:error,reason}
        end
    end
  end

  def handle(:battle_ready,player_state) do
    case player_state.in_room_id do
      0 ->
        {:error,:not_in_room}
      other ->        
        case Room.battle_ready(player_state.in_room_id) do
          :ok->
            {:ok,player_state}
          reason->
            {:error,reason}
        end
    end
  end

  def handle(:battle_start,player_state) do
    case player_state.in_room_id do
      0 ->
        {:error,:not_in_room}
      other ->
        case Room.battle_start(player_state.in_room_id) do
          :ok -> 
            {:ok,player_state}
          reason->
            {:error,reason}
        end
    end
  end

end