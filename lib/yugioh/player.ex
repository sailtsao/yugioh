defmodule Player do
  require Lager
  use ExActor.GenServer

  definit ({player_state,socket}) do
    Lager.debug "player process [~p] with status [~p] created",[self,player_state]    
    init_cast(self,socket)
    initial_state(player_state)
  end    

  defcall player_state,state: player_state do
    reply player_state
  end

  defcall update_player_state(params),state: player_state do
    player_state = player_state.update(params)
    set_and_reply player_state,player_state
  end

  defcast init_cast(socket),state: player_state do
    player_state = player_state.socket(socket)

    # Yugioh.Library.Online.add_onine_player(player_state.id,self)

    new_state player_state
  end

  defcast stop_cast,state: player_state do
    {:stop, :normal, player_state}
  end  

  defcast socket_event(cmd,data),state: player_state do
    Lager.debug "receive socket event cmd [~p] data [~p]",[cmd,data]
    [h1,h2,_,_,_] = integer_to_list(cmd)
    case [h1,h2] do
      '11'->
        result = Yugioh.System.Room.handle(data,player_state)
      '12'->
        result = Yugioh.System.Battle.handle(data,player_state)
    end
    case result do
      :ok->
        set_and_reply player_state,:ok
      reason->
        {:stop,:normal,{:error,reason},player_state}
    end
  end

  definfo {:new_room_member,seat,pid},state: player_state do
    other_player_state = Yugioh.Player.player_state(pid)
    :gen_tcp.send(player_state.socket,Yugioh.Proto.PT11.write(11003,[seat,other_player_state]))
    noreply
  end

  definfo {:refresh_room_info,room_info},state: player_state do
    spawn(fn-> :gen_tcp.send(player_state.socket,Yugioh.Proto.PT11.write(11005,room_info)) end)
    noreply
  end
  
  definfo {:refresh_ready_state,seat,ready_state},state: player_state do
    ready_data = case ready_state do
      :ready->
        1
      :unready->
        0
    end
    :gen_tcp.send(player_state.socket,Yugioh.Proto.PT11.write(11006,[seat,ready_data]))
    noreply
  end

  definfo {:send,data},state: player_state do
    :gen_tcp.send(player_state.socket,data)
    Lager.debug "send data [~p] to player [~p]",[data,self]
    noreply
  end
  
  def terminate(reason,player_state) do
    # update online system
    Yugioh.Library.Online.remove_online_player(player_state.id)

    if player_state.in_room_id != 0 do
      Yugioh.Singleton.Room.leave_room(player_state.in_room_id)
    end

    if player_state.battle_pid != nil do
      Yugioh.Battle.stop_cast player_state.battle_pid
    end

    :gen_tcp.close(player_state.socket)
    Lager.debug "player process [~p] died reason [~p]",[self,reason]
  end  
    
end