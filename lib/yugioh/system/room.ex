defmodule System.Room do
  require Lager
  use ExActor.GenServer, export: :room_server

  defrecord State,auto_id: 1,rooms_dict: HashDict.new
  definit do
    initial_state(State.new)
  end

  # if recycle_room_ids is empty,then use the auto_id to create a new id
  
  defcall create_room([],player_state),from: {pid,_},state: state do
    room_player_info = RoomPlayerInfo.new(id: player_state.id,player_pid: pid,socket: player_state.socket,name: player_state.name,
      avatar: player_state.avatar,ready_state: :ready)
    room_info = RoomInfo.new(id: state.auto_id,status: :wait,name: "Room#{state.auto_id}",owner_seat: 1,
      members_dict:  HashDict.new([{1,room_player_info}]))
    rooms_dict = Dict.put state.rooms_dict,state.auto_id,room_info
    state = state.update [auto_id: state.auto_id+1,rooms_dict: rooms_dict]


    player_state = player_state.room_id(room_info.id)
    send pid,{:send,Proto.PT11.write(:create_room,[1,room_info])}
    Lager.debug "create new room,room_info:[~p],room count:[~p]",[room_info,Dict.size(rooms_dict)]
    set_and_reply(state,{:ok,player_state})
  end

  defcall get_rooms([],player_state),from: {pid,_},state: state do
    send pid,{:send,Proto.PT11.write(:get_rooms,[Dict.values(state.rooms_dict)])}
    set_and_reply(state,{:ok,player_state})
  end

  defcall enter_room([room_id],player_state),from: {pid,_},state: state do
    result = :ok
    room_info = Dict.get(state.rooms_dict,room_id)
    if room_info == nil do
      result = :invalid_room_id
    else
      if Dict.size(room_info.members_dict) == 2 do
        result = :room_already_full
      end
    end
    
    if result == :ok do
      avaible_seat = :lists.subtract([1,2],Dict.keys(room_info.members_dict))
      [seat|_rest] = avaible_seat            
      room_player_info = RoomPlayerInfo.new(id: player_state.id,player_pid: pid,socket: player_state.socket,name: player_state.name,
        avatar: player_state.avatar,ready_state: :unready)
      Enum.each room_info.members_dict,fn({_,other_room_player_info})->
        send other_room_player_info.player_pid,{:send,Proto.PT11.write(:new_members,[seat,room_player_info])}
      end
      members_dict = Dict.put(room_info.members_dict,seat,room_player_info)
      room_info = room_info.members_dict members_dict
      rooms_dict = Dict.put state.rooms_dict,room_info.id,room_info
      state = state.rooms_dict(rooms_dict)
      

      player_state = player_state.room_id(room_info.id)
      send pid,{:send,Proto.PT11.write(:enter_room,[1,room_info])}
      Lager.debug "enter room room_info:[~p]",[room_info]
    end
    set_and_reply(state,{result,player_state})
  end

  defcall leave_room([],player_state),from: {pid,_},state: state do     
    result = :ok
    room_info = Dict.get(state.rooms_dict,player_state.room_id)
    if room_info == nil do
      result = :invalid_room_id
    end
    
    if result == :ok do
      [{seat,room_player_info}] = Enum.filter room_info.members_dict,fn({_,room_player_info})->
        room_player_info.player_pid == pid
      end
      members_dict = Dict.drop(room_info.members_dict,[seat])
      room_info = room_info.members_dict members_dict
      case Dict.size(members_dict) do
        0->
          state = Dict.drop(state.rooms_dict,[room_info.id]) |> state.rooms_dict
          Lager.debug "leave room,room count [~p]",[Dict.size(state.rooms_dict)]
        1->
          if seat == room_info.owner_seat do
            if room_info.owner_seat == 1 do
              room_info = room_info.owner_seat 2
            else
              room_info = room_info.owner_seat 1
            end                        
          end
          owner_seat = room_info.owner_seat
          owner_room_player_info = Dict.get(members_dict,room_info.owner_seat)
          owner_room_player_info = owner_room_player_info.ready_state(:ready)
          
          room_info = Dict.put(members_dict,owner_seat,owner_room_player_info)|> room_info.members_dict

          state = Dict.put(state.rooms_dict,room_info.id,room_info) |> state.rooms_dict

          Enum.each members_dict,fn({_,other_room_player_info}) ->
            send other_room_player_info.player_pid,{:send,Proto.PT11.write(:refresh_roominfo,[room_info])}
          end          
          Lager.debug "leave room room_info:[~p]",[room_info]
        _->
          result = :error
          Lager.debug "[~p] [~p]",[seat,members_dict]
      end      
      send room_player_info.player_pid,{:send,Proto.PT11.write(:leave_room,[1])}      
    end
    player_state = player_state.room_id 0
    set_and_reply state,{result,player_state}
  end

  defcall refresh_roominfo([],player_state),from: {pid,_},state: state do
    result = :ok
    room_info = Dict.get(state.rooms_dict,player_state.room_id)
    if room_info == nil do
      result = :invalid_room_id
    end
    if result == :ok do
      send pid,{:send,Proto.PT11.write(:refresh_roominfo,[room_info])}
    end
    set_and_reply state,{:ok,player_state}
  end

  defcall battle_ready([],player_state),from: {pid,_},state: state do
    result = :ok
    room_info = Dict.get(state.rooms_dict,player_state.room_id)
    if room_info == nil do
      result = :invalid_room_id
    else    
      owner_room_player_info = Dict.get room_info.members_dict,room_info.owner_seat
      if pid == owner_room_player_info.player_pid do
        result = :room_owner_cant_unready
      end    
    end

    if result == :ok do
      [{seat,room_player_info}] = Enum.filter room_info.members_dict,fn({_,room_player_info})->
        room_player_info.player_pid == pid
      end
      
      ready_state = case room_player_info.ready_state do
        :ready->
          :unready
        :unready->
          :ready
      end
      room_player_info = room_player_info.ready_state(ready_state)
      
      room_info =  Dict.put(room_info.members_dict,seat,room_player_info) |> room_info.members_dict 
      state = Dict.put(state.rooms_dict,room_info.id,room_info) |> state.rooms_dict

      Lager.debug "battle ready room_info:~p",[room_info]
      Enum.each room_info.members_dict,fn({_,room_player_info}) ->
        send room_player_info.player_pid, {:send,Proto.PT11.write(:refresh_ready_state,[seat,ready_state])}
      end
    end
    set_and_reply state,{:ok,player_state}
  end

  defcall battle_start([],player_state),from: {pid,_},state: state do
    result = :ok
    room_info = Dict.get(state.rooms_dict,player_state.room_id)
    if room_info == nil do
      result = :invalid_room_id
    else
      if Dict.size(room_info.members_dict) < 2 do
        result = :battle_start_not_enough_members
      end

      unready_list = Enum.filter room_info.members_dict,fn({_,room_player_info}) ->
        room_player_info.ready_state == :unready
      end
      if Enum.count(unready_list)>0 do
        result = :battle_start_not_all_members_ready
      end

      owner_room_player_info = Dict.get room_info.members_dict,room_info.owner_seat
      if pid != owner_room_player_info.player_pid do
        result = :battle_start_not_room_owner
      end
    end

    if result == :ok do
      room_player1_info = Dict.get room_info.members_dict,1
      room_player2_info = Dict.get room_info.members_dict,2
      {:ok,_} = System.Battle.start({room_player1_info.player_pid,room_player2_info.player_pid})
    
      members_dict = List.foldl [1,2],room_info.members_dict,fn(seat,members_dict)->
        if seat != room_info.owner_seat do
          room_player_info = Dict.get members_dict,seat
          room_player_info = room_player_info.ready_state(:unready)
          Dict.put members_dict,seat,room_player_info
        else
          members_dict
        end
      end
      
      room_info = room_info.members_dict members_dict
      state = Dict.put(state.rooms_dict,room_info.id,room_info) |> state.rooms_dict
      Lager.debug "battle start room_info:~p",[room_info]
    end
    set_and_reply state,{result,player_state}
  end

  def handle({:get_rooms,params},player_state) do
    get_rooms(params,player_state)
  end
  

  def handle({func_atom,params},player_state) 
  when func_atom in [:create_room,:enter_room] do

    case player_state.room_id do
      0 ->
        apply(__MODULE__,func_atom,[params,player_state])
      _->
        :already_in_room
    end
  end

  def handle({func_atom,params},player_state) do

    case player_state.room_id do
      0 ->
        :player_not_in_room
      _->        
        apply(__MODULE__,func_atom,[params,player_state])
    end    
  end  
  

  def terminate(reason,state) do
    Lager.debug "room died with reason [~p] state [~p]",[reason,state]
  end

end