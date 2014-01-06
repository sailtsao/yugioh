defmodule Yugioh.Battle do
  require Lager
  use GenServer.Behaviour  

  def start({player1_pid,player2_pid}) do
    :gen_server.start(__MODULE__,{player1_pid,player2_pid},[])
  end

  def stop(pid) do
    if is_pid(pid), do: :gen_server.cast(pid, :stop)
  end

  def init({player1_pid,player2_pid}) do
    Lager.debug "battle process [~p] for player [~p] created",[self,{player1_pid,player2_pid}]
    :gen_server.cast self,{:init,player1_pid,player2_pid}
    {:ok,{}}
  end

  def handle_call(_msg, _from, state) do
    reply = :ok
    {:reply, reply, state}
  end
  
  def handle_cast({:init,player1_pid,player2_pid},state) do

    player1_state = :gen_server.call(player1_pid,:player_state)
    player2_state = :gen_server.call(player2_pid,:player_state)

    :ok = :gen_server.call(player1_pid,{:update_player_state,player1_state.battle_pid(self)})
    :ok = :gen_server.call(player2_pid,{:update_player_state,player2_state.battle_pid(self)})

    player1_cards = Enum.shuffle(player1_state.cards)
    player2_cards = Enum.shuffle(player2_state.cards)

    {player1_handcards,player1_cards} = Enum.split(player1_cards,3)
    {player2_handcards,player2_cards} = Enum.split(player2_cards,2)

    player1_battle_state = BattleInfo[player_pid: player1_pid,maxhp: player1_state.hp,curhp: player1_state.hp,handcards: player1_handcards,
    remaincards: player1_cards,socket: player1_state.socket]
    player2_battle_state = BattleInfo[player_pid: player2_pid,maxhp: player2_state.hp,curhp: player2_state.hp,handcards: player2_handcards,
    remaincards: player2_cards,socket: player2_state.socket]

    # wait for player to decide who first 
    # order_game
    # send battle info
    # random 5 cards 
    message_data = [1,player1_state.id,player1_state,player1_battle_state,player2_state,player2_battle_state]
    player1_pid <- {:send,Yugioh.Proto.PT11.write(11007,message_data)}
    player2_pid <- {:send,Yugioh.Proto.PT11.write(11007,message_data)}
    battle_state_dict = [{player1_state.id,player1_battle_state},{player2_state.id,player2_battle_state}]
    {:noreply, [turn_count: 1,operator_id: player1_state.id,battle_states: battle_state_dict]}
  end

  def handle_cast(:stop, state) do
    {:stop, :normal, state}
  end

  def handle_cast(_msg, state) do
    {:noreply, state}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end

  def terminate(reason,state) do
    Lager.debug "battle process [~p] state [~p] died for reason [~p]",[self,state,reason]
  end

  def code_change(_oldVsn, state, _extra) do
    {:ok, state}
  end
end