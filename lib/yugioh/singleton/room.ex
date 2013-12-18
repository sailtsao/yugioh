defmodule Yugioh.Singleton.Room do
  require Lager

  use GenServer.Behaviour

  def start() do
    :gen_server.start({:local,__MODULE__},__MODULE__,[],[])
  end
  
  def init(_) do
    :gen_server.cast(self,:init)
    {:ok, 1}
  end

  def handle_call({:create_room,name,type},from,auto_id) do
    {pid,_} = from
    room_info = RoomInfo.new(id: auto_id,type: type,status: :wait,name: name,owner_pid: pid,members:  HashDict.new([{1,pid}]))
    :ets.insert(:room, room_info)
    Lager.debug "create new room with name ~s,room_info:~p,room count ~p",[name,room_info,:ets.info(:room,:size)]
    {:reply,{:ok,room_info},auto_id+1}
  end

  def handle_call(:get_rooms, _from, state) do
    {:reply,{:ok,:ets.tab2list(:room)},state}
  end

  def handle_call({:enter_room,room_id}, from, state) do
    case :ets.lookup(:room,room_id) do
      []->
        {:reply,{:invalid_room_id,RoomInfo.new},state}
      [room_info]->
        avaible_seat = :lists.subtract([1,2],Dict.keys(room_info.members))
        [seat|_rest] = avaible_seat
        {pid,_} = from
        Enum.map Dict.to_list(room_info.members),fn({_,other_player_pid}) ->
          other_player_pid <- {:new_room_member,seat,pid}
        end
        new_members = Dict.put(room_info.members,seat,pid)
        new_room_info = room_info.update(members: new_members)
        Lager.debug "new enter room room_info:~p",[new_room_info]
        :ets.insert :room,new_room_info
        new_room_info
        {:reply,{:ok,new_room_info},state}
    end
  end

  def handle_call({:leave_room,room_id},from,state) do
    {pid,_} = from
    case :ets.lookup(:room,room_id) do
      [room_info]->
        Enum.map Dict.to_list(room_info.members),fn({seat,player_pid}) ->
          if pid === player_pid do            
            members = Dict.drop(room_info.members,[seat])
            case Dict.size(members) do
              0->
                :ets.delete(:room,room_id)
                Lager.debug "leave room, room count ~p",[:ets.info(:room,:size)]
              other->                
                owner_pid = case room_info.owner_pid do
                  ^pid->
                    [new_owner_pid|_] = Dict.values(members)
                    new_owner_pid
                  other->
                    room_info.owner_pid
                end
                new_room_info = room_info.update( members: members,owner_pid: owner_pid)
                :ets.insert :room,new_room_info
                Enum.map Dict.to_list(members),fn({_,other_player_pid}) ->
                  other_player_pid <- {:refresh_room_info,new_room_info}
                end
                Lager.debug "leave room room_info:~p,room count ~p",[new_room_info,:ets.info(:room,:size)]
            end
          end
        end
        {:reply,:ok,state}
      []->
        {:reply,{:invalid_room_id,RoomInfo.new},state}
    end
  end

  def handle_call(msg, from, state) do
    reply = {:no_call_handler, msg ,from}
    {:reply, reply, state}
  end

  def handle_cast(:init, state) do
    # :ets.new(:room,[{:keypos,RoomInfo.__record__(:index,:id)+1},:named_table,:set])
    :ets.new(:room,[{:keypos,RoomInfo.__record__(:index,:id)+1},:named_table,:set,:public])
    {:noreply, state}
  end

  def handle_cast(_msg, state) do
    {:noreply, state}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end
  
  def terminate(_reason,_state) do
    :ok
  end
  
  def code_change(_oldVsn, state, _extra) do
    {:ok, state}
  end

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

  def create_room(name,type) do
    :gen_server.call(__MODULE__,{:create_room,name,type})
  end

  def get_rooms() do
    :gen_server.call(__MODULE__,:get_rooms)    
  end

  def enter_room(room_id) do
    :gen_server.call(__MODULE__,{:enter_room,room_id})
  end
  
  def leave_room(room_id) do
    :gen_server.call(__MODULE__,{:leave_room,room_id})
  end

end