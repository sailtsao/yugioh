defmodule System.Player do
  require Lager
  use ExActor.GenServer

  definit ({player_state,socket}) do
    init_cast(self,socket)
    initial_state(player_state)
  end

  defcall player_state,state: player_state do
    reply player_state
  end

  defcall update_player_state(params),state: player_state do
    player_state = player_state.update(params)
    set_and_reply player_state,:ok
  end

  defcall socket_event(message_id,binary_data),state: player_state do
    {:ok,{func_atom,params}} = ProtoUtil.decode_message(message_id,binary_data)
    Lager.debug "message_id [~p] func_atom [~p] params [~p]",[message_id,func_atom,params]
    [h1,h2,_,_,_] = integer_to_list(message_id)
    case [h1,h2] do
      '11'->
        {result,player_state} = System.Room.handle({func_atom,params},player_state)
      '12'->
        {result,player_state} = System.Battle.handle({func_atom,params},player_state)
      '13'->
        {result,player_state} = apply(PlayerCore,func_atom,[player_state,params])
    end
    case result do
      :ok->
        set_and_reply player_state,:ok
      reason->
        {:stop,:normal,{:error,reason},player_state}
    end
  end

  defcast init_cast(socket),state: player_state do
    player_state = player_state.socket(socket)

    # Library.Online.add_onine_player(player_state.id,self)

    new_state player_state
  end

  defcast stop_cast,state: player_state do
    {:stop, :normal, player_state}
  end

  definfo {:send,data},state: player_state do
    :gen_tcp.send(player_state.socket,data)
    <<_::16,message_id::16,_rest::binary>>=data
    Lager.debug "send data message_id [~p] data [~p] to player [~p]",[message_id,data,self]
    noreply
  end

  def terminate(reason,player_state) do
    Lager.debug "player state [~p] died reason [~p]",[player_state,reason]

    # update online system
    # Library.Online.remove_online_player(player_state.id)

    if player_state.room_id != 0 do
      System.Room.leave_room([],player_state)
    end

    if player_state.battle_pid != nil do
      System.Battle.stop_cast player_state.battle_pid
    end

    :gen_tcp.close(player_state.socket)

  end

end