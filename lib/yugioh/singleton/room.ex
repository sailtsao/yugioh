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
    room_info = RoomInfo.new(id: auto_id,type: type,status: :wait,name: name,owner_pid: pid,members:  HashDict.new([{1,{pid,:ready}}]))
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
        {:reply,:invalid_room_id,state}
      [room_info]->
        avaible_seat = :lists.subtract([1,2],Dict.keys(room_info.members))
        [seat|_rest] = avaible_seat
        {pid,_} = from
        Enum.each Dict.to_list(room_info.members),fn({_,{other_player_pid,_}}) ->
          other_player_pid <- {:new_room_member,seat,pid}
        end
        new_members = Dict.put(room_info.members,seat,{pid,:unready})
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
        Enum.each Dict.to_list(room_info.members),fn({seat,{player_pid,_}}) ->
          if pid === player_pid do            
            members = Dict.drop(room_info.members,[seat])
            case Dict.size(members) do
              0->
                :ets.delete(:room,room_id)
                Lager.debug "leave room, room count ~p",[:ets.info(:room,:size)]
              other->                
                new_room_info = case room_info.owner_pid do
                  ^pid->
                    [{new_owner_seat,{new_owner_pid,_}}|_] = Dict.to_list(members)
                    new_members=Dict.put(members,new_owner_seat,{new_owner_pid,:ready})
                    room_info.update( members: new_members,owner_pid: new_owner_pid)
                  other->
                    room_info.update( members: members)
                end
                :ets.insert :room,new_room_info
                Enum.each Dict.to_list(members),fn({_,{other_player_pid,_}}) ->
                  other_player_pid <- {:refresh_room_info,new_room_info}
                end
                Lager.debug "leave room room_info:~p,room count ~p",[new_room_info,:ets.info(:room,:size)]
            end
          end
        end
        {:reply,:ok,state}
      []->
        {:reply,:invalid_room_id,state}
    end
  end

  def handle_call({:battle_ready,room_id},from,state) do
    {pid,_} = from
    case :ets.lookup(:room,room_id) do
      [room_info]->
        Enum.each Dict.to_list(room_info.members),fn({seat,{player_pid,ready_state}}) ->
          if pid === player_pid do
            new_ready_state = case ready_state do
              :ready->
                :unready
              :unready->
                :ready
            end
            members = Dict.put(room_info.members,seat,{player_pid,new_ready_state})
            new_room_info = room_info.update(members: members)
            :ets.insert :room,new_room_info
            Lager.debug "battle ready new_room_info:~p",[new_room_info]
            Enum.each Dict.to_list(room_info.members),fn({_,{pid,_}}) ->
              pid <- {:refresh_ready_state,seat,new_ready_state}
            end
          end
        end
        {:reply,:ok,state}
      []->
        {:reply,:invalid_room_id,state}
    end
  end

  def handle_call({:battle_start,room_id},from,state) do
    {pid,_} = from
    case :ets.lookup(:room,room_id) do
      [room_info]->
        cond do
          Dict.size(room_info.members) != 2 ->
            {:reply,:not_enough_members,state}

          pid == room_info.owner_pid ->
            unready_list = Enum.filter Dict.to_list(room_info.members),fn({_,{player_pid,ready_state}}) ->
              ready_state == :unready
            end

            case Enum.empty?(unready_list) do 
              true->
                {player1_pid,_} = Dict.get room_info.members,1
                {player2_pid,_} = Dict.get room_info.members,2
                {:ok,battle_pid} = Yugioh.Battle.start({player1_pid,player2_pid})
                {:reply,:ok,state}
              false->
                {:reply,:unready,state}
            end

          true->
            {:reply,:not_owner,state}
        end
      []->
        {:reply,:invalid_room_id,state}
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
  
  def terminate(reason,state) do
    Lager.debug "room process [~p] died with reason [~p] state",[self,reason,state]
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

  def battle_ready(room_id) do
    :gen_server.call(__MODULE__,{:battle_ready,room_id})
  end
  
  def battle_start(room_id) do
    :gen_server.call(__MODULE__,{:battle_start,room_id})
  end

end