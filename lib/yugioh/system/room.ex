defmodule Yugioh.System.Room do
  require Lager
  alias Yugioh.Singleton.Room
  alias Yugioh.Library.Sender
  alias Yugioh.Proto.PT11

  def handle(11000,{:create_room,name,type},player_state) do
    case player_state.in_room_id do
      0 ->
        {:ok,room_info} = Room.create_room(name,type)
        spawn(fn-> :gen_tcp.send(player_state.socket,PT11.write(11000,[1,room_info])) end )
        player_state.update(in_room_id: room_info.id)
      other ->
        spawn(fn-> :gen_tcp.send(player_state.socket,PT11.write(11000,[0,RoomInfo.new])) end)
        player_state
    end
  end
  
  def handle(11001,{:get_rooms},player_state) do
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
    player_state
  end

  def handle(11002,{:enter_room,room_id},player_state) do
    case player_state.in_room_id do
      0 ->
        {:ok,room_info} = Room.enter_room(room_id)
        spawn(fn -> :gen_tcp.send(player_state.socket,PT11.write(11002,[1,room_info])) end)
        player_state.update(in_room_id: room_id)
      other ->
        spawn(fn -> :gen_tcp.send(player_state.socket,PT11.write(11002,[0,RoomInfo.new])) end)
        player_state
    end
  end

  def handle(11004,{:leave_room},player_state) do
    case player_state.in_room_id do
      0 ->
        :gen_tcp.send(player_state.socket,PT11.write(11004,0))
        player_state
      other ->        
        :ok = Room.leave_room(player_state.in_room_id)
        :gen_tcp.send(player_state.socket,PT11.write(11004,1))
        player_state.update(in_room_id: 0)
    end
  end
end