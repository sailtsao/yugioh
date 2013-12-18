defmodule Yugioh.Player do
  require Lager
  use GenServer.Behaviour

  def start_link(player_state,socket) do
    :gen_server.start_link(__MODULE__,[player_state,socket],[])
  end
  
  def init([player_state,socket]) do
    Lager.debug "player process [~p] with status [~p] created",[self,player_state]    
    :gen_server.cast self,{:init,socket}
    {:ok,player_state}
  end

  defp routing(cmd,data,player_state) do
    [h1,h2,_,_,_] = integer_to_list(cmd)
    player_state = case [h1,h2] do
      '11'->
        Yugioh.System.Room.handle(cmd,data,player_state)
    end
    player_state
  end

  def handle_call({"SOCKET_EVENT",cmd,data},_from,player_state) do
    Lager.debug "receive socket event cmd [~p] data [~p]",[cmd,data]
    new_player_state = routing cmd,data,player_state
    {:reply,:ok,new_player_state}
  end

  def handle_call(:player_state,_from,player_state) do
    {:reply, player_state, player_state}
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

  def handle_cast(:stop, player_state) do
    # update online system
    Yugioh.Library.Online.remove_online_player(player_state.id)

    if player_state.in_room_id != 0 do
      Lager.info "player state : ~p",[player_state]
      Yugioh.Singleton.Room.leave_room(player_state.in_room_id)
    end
    {:stop, :normal, player_state}
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
    Lager.info "refresh"
    spawn(fn-> :gen_tcp.send(player_state.socket,Yugioh.Proto.PT11.write(11005,room_info)) end)
    {:noreply, player_state}
  end
  
  def handle_info(_msg, state) do
    {:noreply, state}
  end
  
  def terminate(_reason,player_state) do
    Lager.debug "player process [~p] died",[self]
  end
  
  def code_change(_oldVsn, state, _extra) do
    {:ok, state}
  end

  def player_state(pid) do
    :gen_server.call(pid, :player_state)
  end

  def stop(pid) do
    if is_pid(pid), do: :gen_server.cast(pid, :stop)
  end

end