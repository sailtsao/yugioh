defrecord RoomInfo,id: 0,name: "",type: 0

defmodule Yugioh.Module.Room do
  
  defrecord State,auto_id: 0

  use GenServer.Behaviour

  def start() do
    :gen_server.start({:local,__MODULE__},__MODULE__,[],[])
  end
  
  def init(_args) do
    :gen_server.cast(self,:init)
    {:ok,State.new}
  end

  def handle_call({:create_room,name},from,state) do
    room_info = RoomInfo.new
    room_info = RoomInfo.new(id: state.auto_id,name: name)
    :ets.insert(:room, room_info)
    {:reply,{:ok,room_info.id},State.new(auto_id: state.auto_id+1)}
  end

  def handle_call(:get_rooms, from, state) do
    {:reply,{:ok,:ets.tab2list(:room)},state}
  end

  def handle_call({:enter_room,room_id}, from, state) do
    {:reply,:ok,state}
  end

  def handle_call(_msg, _from, state) do
    reply = :ok
    {:reply, reply, state}
  end

  def handle_cast(:init, state) do
    :ets.new(:room,[{:keypos,RoomInfo.__record__(:index,:id)+1},:named_table,:set])
    {:noreply, state}
  end

  def handle_cast(_msg, state) do
    {:noreply, state}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end
  
  def terminate(_reason,state) do
    :ok
  end
  
  def code_change(_oldVsn, state, _extra) do
    {:ok, state}
  end

  def create_room(name,socket) do
    {:ok,room_id}=:gen_server.call(__MODULE__,{:create_room,name})
    :gen_tcp.send(socket,Yugioh.Proto.PT10.write(10006,[1,room_id]))
  end

  def get_rooms(socket) do
    {:ok,rooms}=:gen_server.call(__MODULE__,:get_rooms)
    rooms_data = Enum.map(rooms,fn(room_info)-> <<room_info.id::size(32),size(room_info.name)::size(16),room_info.name::bitstring,room_info.type::size(16)>> end)    
    data = <<length(rooms)::size(16),iolist_to_binary(rooms_data)::binary>>
    :gen_tcp.send(socket,Yugioh.Proto.PT10.write(10007,data))
  end

  def enter_room(room_id,socket) do
    :gen_server.call(__MODULE__,{:enter_room,room_id})
    :gen_tcp.send(socket,Yugioh.Proto.PT10.write(10008,1))
  end
  
end