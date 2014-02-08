defmodule Yugioh.Player do
  require Lager
  use GenServer.Behaviour

  def start(player_state,socket) do
    :gen_server.start(__MODULE__,[player_state,socket],[])
  end
  
  def init([player_state,socket]) do
    Lager.debug "player process [~p] with status [~p] created",[self,player_state]    
    :gen_server.cast(self,{:init,socket})
    {:ok,player_state}
  end  

  def handle_call({"SOCKET_EVENT",cmd,data},_from,player_state) do
    Lager.debug "receive socket event cmd [~p] data [~p]",[cmd,data]
    [h1,h2,_,_,_] = integer_to_list(cmd)
    result = case [h1,h2] do
      '11'->
        Yugioh.System.Room.handle(data,player_state)
      '12'->
        Yugioh.System.Battle.handle(data,player_state)
    end
    case result do
      {:ok,new_player_state}->
        {:reply,:ok,new_player_state}
      {:error,reason}->
        {:stop,reason,{:error,reason},player_state}
    end
  end

  def handle_call(:player_state,_from,player_state) do
    {:reply, player_state, player_state}
  end

  def handle_call({:update_player_state,new_player_state},_from,_player_state) do
    {:reply, :ok, new_player_state}
  end

  def handle_call(_msg, _from, player_state) do
    reply = :ok
    {:reply, reply, player_state}
  end
  

  def handle_cast({:init,socket},player_state) do
    player_state = player_state.socket(socket)

    # update online system
    Yugioh.Library.Online.add_onine_player(player_state.id,self)

    {:noreply, player_state}
  end

  def handle_cast({:stop,reason}, player_state) do
    {:stop, reason, player_state}
  end

  def handle_cast(_msg, state) do
    {:noreply, state}
  end

  def handle_info({:new_room_member,seat,pid}, player_state) do
    other_player_state = Yugioh.Player.player_state(pid)
    :gen_tcp.send(player_state.socket,Yugioh.Proto.PT11.write(11003,[seat,other_player_state]))
    {:noreply, player_state}
  end

  def handle_info({:refresh_room_info,room_info},player_state) do
    spawn(fn-> :gen_tcp.send(player_state.socket,Yugioh.Proto.PT11.write(11005,room_info)) end)
    {:noreply, player_state}
  end
  
  def handle_info({:refresh_ready_state,seat,ready_state},player_state) do
    ready_data = case ready_state do
      :ready->
        1
      :unready->
        0
    end
    :gen_tcp.send(player_state.socket,Yugioh.Proto.PT11.write(11006,[seat,ready_data]))
    {:noreply, player_state}
  end

  def handle_info({:send,data},player_state) do
    :gen_tcp.send(player_state.socket,data)
    Lager.debug "send data [~p] to player [~p]",[data,self]
    {:noreply, player_state}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end
  
  def terminate(reason,player_state) do
    # update online system
    Yugioh.Library.Online.remove_online_player(player_state.id)

    if player_state.in_room_id != 0 do
      Yugioh.Singleton.Room.leave_room(player_state.in_room_id)
    end

    if player_state.battle_pid != nil do
      Yugioh.Battle.stop player_state.battle_pid
    end
    Lager.debug "player process [~p] died reason [~p]",[self,reason]
    :gen_tcp.close player_state.socket
  end
  
  def code_change(_oldVsn, state, _extra) do
    {:ok, state}
  end

  def player_state(pid) do
    :gen_server.call(pid, :player_state)
  end

  def stop(pid,reason//:normal) do
    if is_pid(pid), do: :gen_server.cast(pid, {:stop,reason})
  end

end