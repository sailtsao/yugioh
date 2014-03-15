defmodule System.Battle do
  require Lager
  use ExActor.GenServer
  alias Yugioh.Data.Cards

  # init
  definit {player1_pid,player2_pid} do
    init_cast self,player1_pid,player2_pid
    initial_state {}
  end

  # spell trap fire effect (handcard)
  defcall fire_effect(_,[:handcard_zone,index]),state: battle_data do
    result = :ok

    player_battle_info = battle_data.operator_battle_info
    card_id = Enum.at(player_battle_info.handcards,index)
    card_data = Cards.get(card_id)
    if card_data.card_type == :monster_card do
      result = :cant_fire_handcard_monster_card_effect
    end

    # TODO: multi skills situation unhandled
    [skill] = Util.get_normal_skills(card_data.skills)

    if ConditionCore.is_skill_conditions_satisfied(skill,battle_data) != true do
      result = :card_cant_fire_effect
    end    

    if result == :ok do
      {_result,battle_data,pos} = SummonCore.summon_spell_card_for_fire(card_data,index,battle_data)
      player_atom = battle_data.operator_atom
      player_battle_info = battle_data.operator_battle_info

      spell_trap = Dict.get player_battle_info.spell_trap_zone,pos
      spell_trap = spell_trap.state :casting
      player_battle_info = Dict.put(player_battle_info.spell_trap_zone,pos,spell_trap) |> player_battle_info.spell_trap_zone
      battle_data = battle_data.update([{player_atom,player_battle_info}])
      effect_callback = fn(battle_data)->
        battle_data = battle_data.effect_callback nil
        BattleCore.destroy_card_to_graveyard(battle_data,battle_data.operator_id,:spell_trap_zone,pos)
      end
      {result,battle_data} = EffectCore.execute_skill_effects(skill,battle_data,[{:effect_callback,effect_callback}])
    end  

    set_and_reply battle_data,result
  end

  # spell trap fire effect (placed in spell_trap_zone)
  defcall fire_effect(_,[:spell_trap_zone,index]),state: battle_data do
    player_battle_info = battle_data.operator_battle_info
    spell_trap = Dict.get(player_battle_info.spell_trap_zone,index)
    card_data = Cards.get(spell_trap.id)
    [skill] = Util.get_normal_skills(card_data.skills)
    result = :ok

    if ConditionCore.is_skill_conditions_satisfied(skill,battle_data) != true do
      result = :card_cant_fire_effect
    end

    if result == :ok do
      card_presentation_change_effect = BattleCore.create_card_presentation_change_effect(spell_trap.id,:attack,battle_data.operator_id,:spell_trap_zone,index)
      Proto.PT12.write(:effects,[card_presentation_change_effect])|>battle_data.send_message_to_all            
      player_atom = battle_data.operator_atom
      player_battle_info = battle_data.operator_battle_info
      spell_trap = Dict.get player_battle_info.spell_trap_zone,index
      spell_trap = spell_trap.state :casting
      player_battle_info = Dict.put(player_battle_info.spell_trap_zone,index,spell_trap) |> player_battle_info.spell_trap_zone
      battle_data = battle_data.update([{player_atom,player_battle_info}])
      effect_callback = fn(battle_data)->
        battle_data = battle_data.effect_callback nil
        BattleCore.destroy_card_to_graveyard(battle_data,battle_data.operator_id,:spell_trap_zone,index)
      end
      {result,battle_data} = EffectCore.execute_skill_effects(skill,battle_data,[{:effect_callback,effect_callback}])      
    end  

    set_and_reply battle_data,result
  end

  # effect monster fire effect
  defcall fire_effect(_,[:monster_card_zone,index]),state: battle_data do
    player_atom = battle_data.operator_atom
    player_battle_info = battle_data.operator_battle_info
    card_id = Dict.get(player_battle_info.monster_card_zone,index).id
    card_data = Cards.get(card_id)
    [skill] = Util.get_normal_skills(card_data.skills)
    result = :ok

    if ConditionCore.is_skill_conditions_satisfied(skill,battle_data) != true,do: result = :card_cant_fire_effect

    if result == :ok do
      {_,battle_data} = EffectCore.execute_skill_effects(skill,battle_data,[])

      monster = Dict.get(player_battle_info.monster_card_zone,index)
      
      monster = monster.effect_fired(true)      
      monster_card_zone = Dict.put(player_battle_info.monster_card_zone,index,monster)
      player_battle_info = player_battle_info.update(monster_card_zone: monster_card_zone)
      battle_data = battle_data.update([{player_atom,player_battle_info}])
    end
    
    set_and_reply battle_data,result
  end

  defcall fire_effect(_,_) do
    reply :invalid_fire_effect
  end

  defcall get_cards_of_scene_type(_,[player_id,scene_type]),from: {pid,_},state: battle_data,
  when: scene_type in [:graveyard_zone,:extra_deck_zone,:banished_zone,:deck_zone] do
    player_battle_info = battle_data.get_player_battle_info player_id
    cards = Util.get_cards_from_scene player_battle_info,scene_type
    message_data = Proto.PT12.write(:get_cards_of_scene_type,[player_id,scene_type,cards])
    SendUtil.send_message pid,message_data
    set_and_reply battle_data,:ok
  end
  
  defcall get_cards_of_scene_type(_,_) do
    reply :invalid_get_cards_of_scene_type
  end
  
  # get card operations  

  # handcard operations in phase mp1 or mp2
  defcall get_card_operations(_,[:handcard_zone,index]),
  from: {pid,_},
  state: battle_data = BattleData[phase: phase],
  when: phase == :mp1 or phase == :mp2 do    
    player_battle_info = battle_data.operator_battle_info
    card_data = Enum.at(player_battle_info.handcards,index) |> Cards.get
    operations = card_data.get_operations(battle_data)
    send pid,{:send,Proto.PT12.write(:get_card_operations,operations)}
    set_and_reply battle_data,:ok
  end

  # handcard operations except mp1 or mp2 phase
  defcall get_card_operations(_,[:handcard_zone,_index]),from: {pid,_} do
    send pid,{:send,Proto.PT12.write(:get_card_operations,[])}
    reply :ok
  end

  # graveyard operations
  defcall get_card_operations(_,[:graveyard_zone,_index]),from: {pid,_} do
    send pid,{:send,Proto.PT12.write(:get_card_operations,[])}
    reply :ok
  end

  # monster operations in bp phase
  defcall get_card_operations(_,[:monster_card_zone,index]),from: {pid,_},
    state: battle_data = BattleData[phase: :bp,turn_count: turn_count] do

    player_battle_info = battle_data.operator_battle_info

    operations = []
    monster = Dict.get player_battle_info.monster_card_zone,index
    if monster.attacked == false and turn_count > 1 and monster.presentation == :attack do
      operations = [:attack_operation]
    end
    send pid,{:send,Proto.PT12.write(:get_card_operations,operations)}
    set_and_reply battle_data,:ok
  end

  # monster operations in mp1 or mp2
  defcall get_card_operations(_,[:monster_card_zone,index]),from: {pid,_},
    state: battle_data = BattleData[phase: phase],
  when: phase == :mp1 or phase == :mp2 do

    player_battle_info = battle_data.operator_battle_info
    monster = Dict.get player_battle_info.monster_card_zone,index 

    operations = BattleCore.get_monster_zone_operations monster,battle_data    

    send pid,{:send,Proto.PT12.write(:get_card_operations,operations)}
    set_and_reply battle_data,:ok
  end

  # monster operations in dp sp ep phase
  defcall get_card_operations(_,[:monster_card_zone,_index]),from: {pid,_} do
    send pid,{:send,Proto.PT12.write(:get_card_operations,[])}
    reply :ok
  end

  # magic trap operations
  defcall get_card_operations(_,[:spell_trap_zone,_index]),from: {pid,_} do
    send pid,{:send,Proto.PT12.write(:get_card_operations,[:fire_effect_operation])}
    reply :ok
  end

  defcall get_card_operations(_,_) do
    reply :invalid_get_card_operations
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

  defcall summon(_,[handcards_index,presentation,:special_summon]),
  state: battle_data = BattleData[phase: phase],
  when: phase in [:mp1,:mp2] do
    
    result = :ok

    operator_battle_info = battle_data.operator_battle_info

    card_id = Enum.at(operator_battle_info.handcards,handcards_index)
    card_data = Yugioh.Data.Cards.get(card_id)
    skill = card_data.get_special_summon_skill
    
    if ConditionCore.is_skill_conditions_satisfied(skill,battle_data) != true do
      result = :card_cant_be_special_summoned
    end

    if result == :ok do      
      {_,battle_data} = EffectCore.execute_skill_effects(skill,battle_data,[{:handcards_index,handcards_index},{:presentation,presentation}])
    end    
    
    set_and_reply battle_data,result
  end

# normal summon in mp1 mp2 phase
  defcall summon(_,[handcards_index,presentation,:normal_summon]),
  state: battle_data = BattleData[phase: phase],
  when: phase in [:mp1,:mp2] do
    
    player_battle_info = battle_data.operator_battle_info
    
    card_id = Enum.at(player_battle_info.handcards,handcards_index)
    card_data = Cards.get(card_id)

    {result,battle_data} = SummonCore.summon_card(card_data,handcards_index,presentation,battle_data)

    set_and_reply battle_data,result
  end

# normal summon when already summmoned
  defcall summon(_,[_,_,:normal_summon]),state: BattleData[normal_summoned: true] do
    reply :cant_normal_summon_twice_in_one_turn
  end

  defcall summon(_,_) do    
    reply :invalid_summon
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
  defcall flip_card(_,[card_index]),
  state: battle_data = BattleData[phase: phase],
  when: phase in [:mp1,:mp2] do
    player_id = battle_data.operator_id
    player_atom = battle_data.operator_atom
    player_battle_info = battle_data.operator_battle_info
    result = :ok
    monster = Dict.get(player_battle_info.monster_card_zone,card_index)
    if monster.presentation_changed do
      result = :already_changed_presentation_in_one_turn
    end
    if result == :ok do
      monster = case monster.presentation do
        :defense_down->
          monster.update(presentation: :attack,presentation_changed: true)
        :defense_up->
          monster.update(presentation: :attack,presentation_changed: true)
        :attack->
          monster.update(presentation: :defense_up,presentation_changed: true)
      end
      monster_card_zone = Dict.put(player_battle_info.monster_card_zone,card_index,monster)
      player_battle_info = player_battle_info.monster_card_zone monster_card_zone
      battle_data = battle_data.update([{player_atom,player_battle_info}])

      presentation_change_effect = BattleCore.create_card_presentation_change_effect(monster.id,monster.presentation,player_id,:monster_card_zone,card_index)
      battle_data.send_message_to_all Proto.PT12.write(:effects,[presentation_change_effect])
      set_and_reply battle_data,:ok
    end
  end

  # flip card in wrong phase
  defcall flip_card(_,_) do
    reply :invalid_flip_card
  end  

  defcall attack(_,[source_card_index]),state: battle_data = BattleData[phase: :bp] do
    {result,battle_data} = AttackCore.attack source_card_index,battle_data
    set_and_reply battle_data,result
  end
        
  defcall attack(_,_),state: BattleData[turn_count: 1] do
    reply :cant_attack_at_first_turn
  end

  defcall attack(_,_) do
    reply :attack_in_invalid_phase
  end  

# change phase
  defcall change_phase_to(_,[phase]),state: battle_data do
    old_phase = battle_data.phase
    result = case phase do
      :bp when old_phase in [:mp1]->
        battle_data = battle_data.phase(:bp)
        :ok
      :mp2 when old_phase in [:mp1,:bp]->
        battle_data = battle_data.phase(:mp2)
        :ok
      :ep when old_phase in [:mp1,:bp,:mp2]->
        send self , :new_turn_draw_phase
        :ok
      _->
        :invalid_phase_change
    end        
    if result == :ok do
      Proto.PT12.write(:change_phase_to,[phase]) |> battle_data.send_message_to_all
    end
    Lager.debug "phase changed [~p] ",[battle_data]
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
    player1_battle_info = BattlePlayerInfo[player_pid: player1_pid,hp: player1_state.hp,handcards: player1_handcards,
    deckcards: player1_deckcards,socket: player1_state.socket]
    player2_battle_info = BattlePlayerInfo[player_pid: player2_pid,hp: player2_state.hp,handcards: player2_handcards,
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

    new_state BattleData[turn_count: 0,phase: :wait_load_finish_1,operator_id: player1_state.id,
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
    new_operator_id = battle_data.new_turn_operator_id
    player_atom = battle_data.get_player_atom new_operator_id
    player_battle_info = battle_data.get_player_battle_info new_operator_id
    [draw_card_id|deckcards] = player_battle_info.deckcards

    handcards = player_battle_info.handcards++[draw_card_id]    

    # !!!!!!!!!test for special summon
    # handcards = [8,7,7,7,7,7]
    
    # !!!!!!!!!test for fire effect
    # handcards = [11,11,11,11,11]

    monster_card_zone = Enum.map(player_battle_info.monster_card_zone,fn({index,monster})-> {index,monster.turn_reset} end)
    player_battle_info = player_battle_info.update(deck: deckcards,handcards: handcards,monster_card_zone: monster_card_zone)
    battle_data = battle_data.update([{player_atom,player_battle_info},{:turn_count,battle_data.turn_count+1},
      {:phase,:dp},{:normal_summoned,false},{:operator_id,new_operator_id}])
    
    message = Proto.PT12.write(:new_turn_draw,[battle_data.turn_count,battle_data.phase,battle_data.operator_id,draw_card_id])
    message_masked = Proto.PT12.write(:new_turn_draw,[battle_data.turn_count,battle_data.phase,battle_data.operator_id,0])
    battle_data.send_message_to_all_with_mask new_operator_id,message,message_masked    

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