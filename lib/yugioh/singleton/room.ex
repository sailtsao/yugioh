defmodule Yugioh.Singleton.Room do
  require Lager
  use ExActor.GenServer, export: :room_server

  definit do
    init_cast
    initial_state(1)
  end

  defcall create_room(),from: {pid,_},state: auto_id do
    room_info = RoomInfo.new(id: auto_id,status: :wait,name: "Room#{auto_id}",owner_pid: pid,members:  HashDict.new([{1,{pid,:ready}}]))
    :ets.insert(:room,room_info)
    Lager.debug "create new room with name ~s,room_info:~p,room count ~p",[name,room_info,:ets.info(:room,:size)]
    set_and_reply(auto_id+1,{:ok,room_info})
  end

  defcall get_rooms,do: reply({:ok,:ets.tab2list(:room)})

  defcall enter_room(room_id),from: {pid,_} do
    case :ets.lookup(:room,room_id) do
      []->
        reply :invalid_room_id
      [room_info]->
        avaible_seat = :lists.subtract([1,2],Dict.keys(room_info.members))
        case avaible_seat do
          []->
            reply :room_already_full
          _->
            [seat|_rest] = avaible_seat
            Enum.each Dict.to_list(room_info.members),fn({_,{other_player_pid,_}}) ->
              send other_player_pid , {:new_room_member,seat,pid}
            end
            new_members = Dict.put(room_info.members,seat,{pid,:unready})
            new_room_info = room_info.update(members: new_members)
            Lager.debug "new enter room room_info:~p",[new_room_info]
            :ets.insert :room,new_room_info
            reply {:ok,new_room_info}
        end        
    end
  end

  defcall leave_room(room_id),from: {pid,_} do     
    case :ets.lookup(:room,room_id) do
      [room_info]->
        Enum.each Dict.to_list(room_info.members),fn({seat,{player_pid,_}}) ->
          if pid === player_pid do            
            members = Dict.drop(room_info.members,[seat])
            case Dict.size(members) do
              0->
                :ets.delete(:room,room_id)
                Lager.debug "leave room, room count ~p",[:ets.info(:room,:size)]
              _->
                new_room_info = case room_info.owner_pid do
                  ^pid->
                    [{new_owner_seat,{new_owner_pid,_}}|_] = Dict.to_list(members)
                    new_members=Dict.put(members,new_owner_seat,{new_owner_pid,:ready})
                    room_info.update( members: new_members,owner_pid: new_owner_pid)
                  _->
                    room_info.update( members: members)
                end
                :ets.insert :room,new_room_info
                Enum.each Dict.to_list(members),fn({_,{other_player_pid,_}}) ->
                  send other_player_pid , {:refresh_room_info,new_room_info}
                end
                Lager.debug "leave room room_info:~p,room count ~p",[new_room_info,:ets.info(:room,:size)]
            end
          end
        end
        reply :ok
      []->
        reply :invalid_room_id
    end
  end

  defcall refresh_roominfo(room_id),from: {pid,_} do
    case :ets.lookup(:room,room_id) do
      [room_info]->
        send pid , {:refresh_room_info,room_info}
        reply :ok
      []->
        reply :invalid_room_id
    end
  end

  defcall battle_ready(room_id),from: {pid,_} do
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
              send pid , {:refresh_ready_state,seat,new_ready_state}
            end
          end
        end
        reply :ok
      []->
        reply :invalid_room_id
    end
  end

  defcall battle_start(room_id),from: {pid,_} do
    case :ets.lookup(:room,room_id) do
      [room_info]->
        cond do
          Dict.size(room_info.members) != 2 ->
            reply :not_enough_members

          pid == room_info.owner_pid ->
            unready_list = Enum.filter Dict.to_list(room_info.members),
            fn({_,{_player_pid,ready_state}}) ->
              ready_state == :unready
            end

            case Enum.empty?(unready_list) do 
              true->
                {player1_pid,_} = Dict.get room_info.members,1
                {player2_pid,_} = Dict.get room_info.members,2
                {:ok,_battle_pid} = Yugioh.Battle.start({player1_pid,player2_pid})
                new_members =
                cond do 
                  player1_pid != room_info.owner_pid ->
                    Dict.put room_info.members,1,{player1_pid,:unready}
                  player2_pid != room_info.owner_pid ->
                    Dict.put room_info.members,2,{player2_pid,:unready}
                end
                new_room_info = room_info.update(members: new_members)
                :ets.insert :room,new_room_info
                Lager.debug "battle start new_room_info:~p",[new_room_info]
                reply :ok
              false->
                reply :unready
            end

          true->
            reply :not_owner
        end
      []->
        reply :invalid_room_id
    end
  end

  defcast init_cast do
    :ets.new(:room,[{:keypos,RoomInfo.__record__(:index,:id)+1},:named_table,:set,:public])
    noreply
  end  
  
  # def terminate(reason,state) do
  #   Lager.debug "room process [~p] died with reason [~p] state",[self,reason,state]
  #   :ok
  # end

end