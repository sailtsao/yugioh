defmodule System.Battle do
  require Lager
  use ExActor.GenServer
  alias Data.Cards

  # init
  definit {player1_pid,player2_pid} do
    init_cast self,player1_pid,player2_pid
    initial_state {}
  end

  defcall get_cards_of_scene_type(player_id,[target_player_id,scene_type]),state: battle_data do
    {result,battle_data} = GetCardsCore.get_cards_of_scene(player_id,target_player_id,scene_type,battle_data)
    set_and_reply battle_data,result
  end

  # get card operations
  defcall get_card_operations(player_id,[scene_type,index]),state: battle_data do
    {result,battle_data} = OperationsCore.get_operations player_id,scene_type,index,battle_data
    set_and_reply battle_data,result
  end

  # fire effect
  defcall fire_effect(player_id,[scene_type,index]),state: battle_data do
    {result,battle_data} = FireEffectCore.fire_effect(player_id,scene_type,index,battle_data)
    set_and_reply battle_data,result
  end

# battle_load_finish
# 0->1->first dp
# phase is atom,0 and 1 is used to count the ready message
  defcall battle_load_finish(_,[]),state: battle_data=BattleData[phase: :wait_load_finish_1] do
    set_and_reply battle_data.phase(:wait_load_finish_2),:ok
  end

  defcall battle_load_finish(_,[]),state: battle_data=BattleData[phase: :wait_load_finish_2] do
    send self, :new_turn_draw_phase
    set_and_reply battle_data.phase(:dp),:ok
  end

  defcall battle_load_finish(_,[]) do
    reply :invalid_battle_load_finish
  end

  defcall summon(player_id,[handcards_index,presentation,summon_type]),state: battle_data do
    {result,battle_data} = SummonCore.summon(player_id,handcards_index,presentation,summon_type,battle_data)
    set_and_reply battle_data,result
  end

  # callback message
  defcall chain_answer(_,[answer]),state: battle_data do
    result = :ok
    if battle_data.answer_callback == nil do
      result = :none_answer_callback
    end
    if result == :ok do
      {result,battle_data} = battle_data.answer_callback.(answer,battle_data)
    end
    set_and_reply battle_data,result
  end

  defcall choose_card(_,[choose_scene_list]),state: battle_data do
    result = :ok
    if battle_data.choose_callback == nil do
      result = :none_choose_callback
    end
    if result == :ok do
      {result,battle_data} = battle_data.choose_callback.(choose_scene_list,battle_data)
    end
    set_and_reply battle_data,result
  end

  defcall choose_card(_,[[]]) do
    reply :empty_choose_card
  end

# flip card in mp1 mp2 phase
  defcall flip_card(_,[card_index]),state: battle_data do
    {result,battle_data} = FlipCardCore.flip_card :order,card_index,battle_data
    set_and_reply battle_data,result
  end

  defcall attack(player_id,[source_card_index]),state: battle_data do
    {result,battle_data} = AttackCore.attack player_id,source_card_index,battle_data
    set_and_reply battle_data,result
  end

# change phase
  defcall change_phase_to(_,[phase]),state: battle_data do
    {result,battle_data} = ChangePhaseCore.change_phase phase,battle_data
    set_and_reply battle_data,result
  end

#################
# cast
# init cast
  defcast init_cast(player1_pid,player2_pid) do

    # set battle pid to player state
    player1_state = Player.player_state player1_pid
    player2_state = Player.player_state player2_pid

    Player.update_player_state(player1_pid,[{:battle_pid,self}])
    Player.update_player_state(player2_pid,[{:battle_pid,self}])

    # set random seed
    :random.seed(:erlang.now)

    # shuffle deckcards
    player1_deckcards = Enum.shuffle(player1_state.deck)
    player2_deckcards = Enum.shuffle(player2_state.deck)

    # initialize handcards
    {player1_handcards,player1_deckcards} = Enum.split(player1_deckcards,5)
    {player2_handcards,player2_deckcards} = Enum.split(player2_deckcards,5)

    # initialize player_battle_info
    player1_battle_info = BattlePlayerInfo[id: player1_state.id,player_pid: player1_pid,hp: player1_state.hp,handcards: player1_handcards,
    deckcards: player1_deckcards,socket: player1_state.socket]
    player2_battle_info = BattlePlayerInfo[id: player2_state.id,player_pid: player2_pid,hp: player2_state.hp,handcards: player2_handcards,
    deckcards: player2_deckcards,socket: player2_state.socket]

    # wait for player to decide who first
    # order_game

    # # !!!!!!!!!!test for fire effect
    # spell_trap = Cards.get(11).become_spell_trap
    # player1_battle_info = Dict.put(player1_battle_info.spell_trap_zone,2,spell_trap) |> player1_battle_info.spell_trap_zone
    # player2_battle_info = Dict.put(player2_battle_info.spell_trap_zone,2,spell_trap) |> player2_battle_info.spell_trap_zone

    # send battle_start message
    params = [1,player1_state.id,:dp,player1_state,player1_battle_info,player2_state,player2_battle_info.hide_handcards]
    send player1_pid,{:send,Proto.PT11.write(:battle_start,params)}

    params = [1,player1_state.id,:dp,player1_state,player1_battle_info.hide_handcards,player2_state,player2_battle_info]
    send player2_pid,{:send,Proto.PT11.write(:battle_start,params)}

    new_state BattleData[turn_count: 0,phase: :wait_load_finish_1,turn_player_id: player1_state.id ,operator_id: player1_state.id,
      player1_id: player1_state.id,player2_id: player2_state.id,
      player1_battle_info: player1_battle_info,player2_battle_info: player2_battle_info]
  end

# stop cast
  defcast stop_cast,state: state do
    {:stop, :normal, state}
  end

###############
# info
# new turn
  definfo :new_turn_draw_phase,state: battle_data do
    last_turn_player_id = battle_data.turn_player_id
    new_turn_player_id = battle_data.new_turn_operator_id
    last_player_atom = battle_data.get_player_atom last_turn_player_id
    player_atom = battle_data.get_player_atom new_turn_player_id
    player_battle_info = battle_data.get_player_battle_info new_turn_player_id
    last_player_battle_info = battle_data.get_player_battle_info last_turn_player_id
    [draw_card_id|deckcards] = player_battle_info.deckcards

    handcards = player_battle_info.handcards++[draw_card_id]

    # !!!!!!!!!test for special summon
    # handcards = [8,7,7,7,7,7]

    # !!!!!!!!!test for fire effect
    # handcards = [11,11,11,11,11]

    monster_zone = Enum.map(player_battle_info.monster_zone,fn({index,monster})-> {index,monster.turn_reset} end)
    spell_trap_zone = Enum.map(player_battle_info.spell_trap_zone,fn({index,spell_trap})-> {index,spell_trap.count(spell_trap.count+1)} end)
    last_spell_trap_zone = Enum.map(last_player_battle_info.spell_trap_zone,fn({index,spell_trap})-> {index,spell_trap.count(spell_trap.count+1)} end)
    player_battle_info = player_battle_info.update(deck: deckcards,handcards: handcards,monster_zone: monster_zone,spell_trap_zone: spell_trap_zone)

    last_player_battle_info = last_player_battle_info.update(spell_trap_zone: last_spell_trap_zone)
    battle_data = battle_data.update([{player_atom,player_battle_info},{last_player_atom,last_player_battle_info},{:turn_count,battle_data.turn_count+1},
      {:phase,:dp},{:normal_summoned,false},{:turn_player_id, new_turn_player_id},{:operator_id,new_turn_player_id}])

    message = Proto.PT12.write(:new_turn_draw,[battle_data.turn_count,battle_data.phase,new_turn_player_id,draw_card_id])
    message_masked = Proto.PT12.write(:new_turn_draw,[battle_data.turn_count,battle_data.phase,new_turn_player_id,0])
    battle_data.send_message_to_all_with_mask new_turn_player_id,message,message_masked

    send self, :standby_phase
    new_state battle_data
  end

# stand by
  definfo :standby_phase,state: battle_data do
      battle_data = battle_data.phase(:sp)
      # Lager.debug "battle_state when standby phase [~p]",[battle_data]
      Proto.PT12.write(:change_phase_to,[:sp]) |> battle_data.send_message_to_all
      send self , :main_phase_1
      new_state battle_data
  end

# mp1
  definfo :main_phase_1,state: battle_data do
      battle_data = battle_data.phase(:mp1)
      Proto.PT12.write(:change_phase_to,[:mp1]) |> battle_data.send_message_to_all
      Lager.debug "battle_state when main phase 1 [~p]",[battle_data]
      new_state battle_data
  end

# bp
  definfo :battle_phase,state: battle_data do
      battle_data = battle_data.phase(:bp)
      Proto.PT12.write(:change_phase_to,[:bp]) |> battle_data.send_message_to_all
      # Lager.debug "battle_state when battle phase [~p]",[battle_data]
      new_state battle_data
  end

# mp2
  definfo :main_phase_2,state: battle_data do
      battle_data = battle_data.phase(:mp2)
      Proto.PT12.write(:change_phase_to,[:mp2]) |> battle_data.send_message_to_all
      # Lager.debug "battle_state when main phase 2 [~p]",[battle_data]
      new_state battle_data
  end

# battle end
  definfo :battle_end,state: battle_data do
    {result,lose_player_id,win_player_id} = cond do
      battle_data.player1_battle_info.hp <= 0 ->
        {:win,battle_data.player1_id,battle_data.player2_id}
      battle_data.player2_battle_info.hp <= 0 ->
        {:win,battle_data.player2_id,battle_data.player1_id}
      true->
        # no cards,draw situation
        {:draw,0,0}
    end
    Proto.PT12.write(:battle_end,[result,win_player_id,lose_player_id]) |> battle_data.send_message_to_all
    stop_cast self
    noreply
  end

  def handle({func_atom,params},player_state) do
    case is_pid(player_state.battle_pid) do
      true ->
        result = apply(__MODULE__,func_atom,[player_state.battle_pid,player_state.id,params])
        {result,player_state}
      false ->
        {:invalid_battle_pid,player_state}
    end
  end

  def terminate(reason,battle_data) do
    Lager.debug "battle_state when battle termianted with reason [~p] : [~p]",[reason,battle_data]
  end
end