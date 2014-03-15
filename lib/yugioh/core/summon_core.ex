defmodule SummonCore do
  require Lager

  # place spell trap for fire effect
  def summon_spell_card_for_fire(card_data,handcards_index,battle_data)do
    Lager.debug "battle_data [~p]",[battle_data]

    player_id = battle_data.operator_id
    player_atom = battle_data.operator_atom
    player_battle_info = battle_data.operator_battle_info

    result = :ok
    pos = 0

    if player_battle_info.is_spell_trap_zone_full? do
      result = :spell_trap_zone_full
    end

    if result == :ok do
      handcards = List.delete_at(player_battle_info.handcards,handcards_index)
      pos = player_battle_info.get_spell_trap_available_pos

      spell_trap = card_data.become_spell_trap 
      spell_trap_zone = Dict.put(player_battle_info.spell_trap_zone,pos,spell_trap)

      player_battle_info = player_battle_info.update(handcards: handcards,spell_trap_zone: spell_trap_zone)

      battle_data = battle_data.update [{player_atom,player_battle_info}]

      targets = BattleCore.create_effect_targets player_id,:spell_trap_zone,[pos]
      summon_effect = BattleCore.create_summon_effect handcards_index,card_data.id,:attack,targets      
      message_data = Proto.PT12.write :effects,[summon_effect]
      battle_data.send_message_to_all message_data
    end
    Lager.debug "battle_data [~p]",[battle_data]
    {result,battle_data,pos}
  end

  def summon_monster_after_tribute_choose([{_,:monster_card_zone,choose_index_list}],handcards_index,presentation,battle_data) do
    player_id = battle_data.operator_id
    player_atom = battle_data.operator_atom
    player_battle_info = battle_data.operator_battle_info    
    handcards = player_battle_info.handcards
    summon_card_id = Enum.at(handcards,handcards_index)
    card_data = Yugioh.Data.Cards.get(summon_card_id)    
    tribute_number = card_data.get_normal_summon_tribute_amount

    result = :ok

    if Enum.count(choose_index_list) != tribute_number do
      result = :wrong_tribute_choose_number
    end
    
    if result == :ok do
      monster_card_zone = player_battle_info.monster_card_zone
      graveyardcards = player_battle_info.graveyardcards      

      # move tribute card to graveyard
      graveyardcards = List.foldl choose_index_list,graveyardcards,&([Dict.get(monster_card_zone,&1).id|&2])
      # delete the monster card on monster_card_zone
      player_battle_info = Dict.drop(monster_card_zone,choose_index_list) |> player_battle_info.monster_card_zone
      # find the pos of the new summon monster
      pos = player_battle_info.get_monster_available_pos
      # remove the summon card from handcards
      handcards = List.delete_at(handcards,handcards_index)

      monster = card_data.become_monster
      monster = monster.update(presentation: presentation,presentation_changed: true)
      monster_card_zone = Dict.put monster_card_zone,pos,monster

      player_battle_info = player_battle_info.update(monster_card_zone: monster_card_zone,graveyardcards: graveyardcards,handcards: handcards)
      battle_data = battle_data.update([{player_atom,player_battle_info},{:normal_summoned,true}])

      targets = BattleCore.create_effect_targets player_id,:monster_card_zone,choose_index_list
      move_to_graveyard_effect = BattleCore.create_move_to_graveyard_effect targets,battle_data
      
      targets = BattleCore.create_effect_targets player_id,:monster_card_zone,[pos]
      summon_effect = BattleCore.create_summon_effect handcards_index,summon_card_id,presentation,targets
      message_data = Proto.PT12.write(:effects,[move_to_graveyard_effect,summon_effect])
      if presentation == :defense_down do
        summon_effect_masked = BattleCore.create_summon_effect handcards_index,0,presentation,targets
        message_data_masked = Proto.PT12.write(:effects,[move_to_graveyard_effect,summon_effect_masked])
        battle_data.send_message_to_all_with_mask player_id,message_data,message_data_masked        
      else
        battle_data.send_message_to_all message_data
      end
    end

    {result,battle_data}
  end
  
  # place spell trap
  def summon_card(card_data = Card[card_type: card_type],handcards_index,_presentation,battle_data)
  when card_type in [:magic_card,:trap_card] do
    Lager.debug "battle_data [~p]",[battle_data]

    player_id = battle_data.operator_id
    player_atom = BattleCore.get_operator_atom battle_data
    player_battle_info = battle_data.operator_battle_info battle_data

    result = :ok

    if player_battle_info.is_spell_trap_zone_full? do
      result = :spell_trap_zone_full
    end

    if result == :ok do
      handcards = List.delete_at(player_battle_info.handcards,handcards_index)
      pos = player_battle_info.get_spell_trap_available_pos

      spell_trap = card_data.become_spell_trap 
      spell_trap_zone = Dict.put(player_battle_info.spell_trap_zone,pos,spell_trap)

      player_battle_info = player_battle_info.update(handcards: handcards,spell_trap_zone: spell_trap_zone)

      battle_data = battle_data.update [{player_atom,player_battle_info}]

      targets = BattleCore.create_effect_targets player_id,:spell_trap_zone,[pos]
      summon_effect = BattleCore.create_summon_effect handcards_index,card_data.id,:place,targets
      message_data = Proto.PT12.write :effects,[summon_effect]
      summon_effect_masked = BattleCore.create_summon_effect handcards_index,0,:place,targets
      message_data_masked = Proto.PT12.write :effects,[summon_effect_masked]
      battle_data.send_message_to_all_with_mask player_id,:effects,message_data,message_data_masked
    end
    Lager.debug "battle_data [~p]",[battle_data]
    {result,battle_data}
  end

  # normal summon monster that need tribute
  def summon_card(card_data = Card[card_type: :monster_card,level: level],handcards_index,presentation,battle_data)
  when level>=5 do
    Lager.debug "battle_data [~p]",[battle_data]
    player_battle_info = battle_data.operator_battle_info

    result = :ok

    id_index_list = Enum.map player_battle_info.monster_card_zone,fn({index,monster})->
      {monster.id,index}
    end
    summoned_count = player_battle_info.monster_summoned_amount
    if card_data.can_be_normal_summoned?(summoned_count) == false do
      result = :not_enough_tribute_monster_for_normal_summon
    end

    if result == :ok do
      tribute_number = card_data.get_normal_summon_tribute_amount
      message_data = Proto.PT12.write(:choose_card,
        [:tribute_choose,tribute_number,[{battle_data.operator_id,:monster_card_zone,id_index_list}]])
      player_battle_info.send_message message_data
      choose_callback = fn(choose_scene_list,battle_data)->
        battle_data.choose_callback nil
        summon_monster_after_tribute_choose(choose_scene_list,handcards_index,presentation,battle_data)
      end
      battle_data = battle_data.choose_callback choose_callback
    end    
    Lager.debug "battle_data [~p]",[battle_data]
    {result,battle_data}
  end

  # normal summon monster without tribute
  def summon_card(card_data = Card[card_type: :monster_card,level: level],handcards_index,presentation,battle_data)
  when level<5 do
    Lager.debug "battle_data [~p]",[battle_data]
    player_id = battle_data.operator_id
    player_atom = battle_data.operator_atom 
    player_battle_info = battle_data.operator_battle_info 

    result = :ok

    if player_battle_info.is_monster_card_zone_full? do
      result = :already_have_5_monsters
    end

    if result == :ok do
      handcards = List.delete_at(player_battle_info.handcards,handcards_index)

      pos = player_battle_info.get_monster_available_pos
      
      monster = card_data.become_monster
      monster = monster.update(presentation: presentation,presentation_changed: true)
      monster_card_zone = Dict.put(player_battle_info.monster_card_zone,pos,monster)      

      player_battle_info = player_battle_info.update(handcards: handcards,monster_card_zone: monster_card_zone)

      battle_data = battle_data.update [{player_atom,player_battle_info},{:normal_summoned,true}]

      
      targets = BattleCore.create_effect_targets(player_id,:monster_card_zone,[pos])
      summon_effect = BattleCore.create_summon_effect(handcards_index,monster.id,presentation,targets)
      message_data = Proto.PT12.write :effects,[summon_effect]
      summon_effect_masked = BattleCore.create_summon_effect(handcards_index,0,presentation,targets)
      message_data_masked = Proto.PT12.write :effects,[summon_effect_masked]

      if presentation == :defense_down do        
        battle_data.send_message_to_all_with_mask player_id,:effects,message_data,message_data_masked
      else
        Proto.PT12.write(:effects,[summon_effect]) |> battle_data.send_message_to_all
      end
    end
    Lager.debug "battle_data [~p]",[battle_data]
    {result,battle_data}
  end    
end