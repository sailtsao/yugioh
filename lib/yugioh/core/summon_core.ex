defmodule SummonCore do
  require Lager

  def summon(player_id,handcards_index,presentation,:special_summon,battle_data = BattleData[phase: phase])
  when phase in [:mp1,:mp2] do
    result = :ok
    player_battle_info = battle_data.get_player_battle_info player_id

    card_id = Enum.at(player_battle_info.handcards,handcards_index)
    card_data = Data.Cards.get(card_id)
    skill = card_data.get_special_summon_skill

    if ConditionCore.is_skill_conditions_satisfied(player_id,:handcard_zone,handcards_index,skill,battle_data,[]) != true do
      result = :card_cant_be_special_summoned
    end

    if result == :ok do
      # summon this card after execute effects of special summon
      effect_callback = fn(battle_data)->
        player_atom = battle_data.get_player_atom player_id
        player_battle_info = battle_data.get_player_battle_info player_id
        monster = card_data.become_monster
        monster = monster.update(presentation: presentation,presentation_changed: true)
        handcards = List.delete_at(player_battle_info.handcards,handcards_index)
        avaible_pos = :lists.subtract([2,1,3,0,4],Dict.keys(player_battle_info.monster_zone))
        pos = hd avaible_pos
        monster_zone = Dict.put(player_battle_info.monster_zone,pos,monster)
        player_battle_info = player_battle_info.update(handcards: handcards,monster_zone: monster_zone)
        battle_data = battle_data.update [{player_atom,player_battle_info}]
        targets = BattleCore.create_effect_targets player_id,:monster_zone,[pos]
        summon_effect = BattleCore.create_summon_effect handcards_index,card_id,presentation,targets
        message_data = Proto.PT12.write(:effects,[summon_effect])
        if presentation == :defense_down do
          summon_effect_masked = BattleCore.create_summon_effect handcards_index,0,presentation,targets
          message_data_masked = Proto.PT12.write(:effects,[summon_effect_masked])
          battle_data.send_message_to_all_with_mask player_id,message_data,message_data_masked
        else
          message_data = Proto.PT12.write(:effects,[summon_effect])
          battle_data.send_message_to_all message_data
        end
        {:ok,battle_data}
      end

      # choose callback
      callback = fn(choose_scene_list,battle_data)->
        {:ok,battle_data} = EffectCore.execute_skill_effects(player_id,skill,choose_scene_list,battle_data,[effect_callback: effect_callback])
      end

      {result,battle_data} = ChooseCore.execute_skill_choose skill,battle_data,
        [{:info,{player_id,:handcard_zone,handcards_index}},{:callback,callback}]
    end
    {result,battle_data}
  end

  def summon(player_id,handcards_index,presentation,:normal_summon,battle_data = BattleData[phase: phase])
  when phase in [:mp1,:mp2] do
    player_battle_info = battle_data.get_player_battle_info player_id

    card_id = Enum.at(player_battle_info.handcards,handcards_index)
    card_data = Data.Cards.get(card_id)

    SummonCore.summon_card(player_id,card_data,handcards_index,presentation,battle_data)
  end

  def summon(_,_,_,:normal_summon,battle_data = BattleData[normal_summoned: true]) do
    {:already_normal_summoned,battle_data}
  end

  def summon(_,_,_,_,battle_data) do
    {:invalid_summon,battle_data}
  end
  # place spell trap for fire effect
  def summon_spell_card_for_fire(player_id,card_data,handcards_index,battle_data)do
    result = :ok
    pos = 0

    player_atom = battle_data.get_player_atom player_id
    player_battle_info = battle_data.get_player_battle_info player_id

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
    {result,battle_data,pos}
  end

  def summon_monster_after_tribute_choose(player_id,[{_,:monster_zone,choose_index_list}],handcards_index,presentation,battle_data) do
    result = :ok

    player_atom = battle_data.get_player_atom player_id
    player_battle_info = battle_data.get_player_battle_info player_id
    handcards = player_battle_info.handcards
    summon_card_id = Enum.at(handcards,handcards_index)
    card_data = Data.Cards.get(summon_card_id)
    tribute_number = card_data.get_normal_summon_tribute_amount


    if Enum.count(choose_index_list) != tribute_number do
      result = :wrong_tribute_choose_number
    end

    if result == :ok do
      monster_zone = player_battle_info.monster_zone
      graveyardcards = player_battle_info.graveyardcards

      # move tribute card to graveyard
      graveyardcards = List.foldl choose_index_list,graveyardcards,&([Dict.get(monster_zone,&1).id|&2])
      # delete the monster card on monster_zone
      player_battle_info = Dict.drop(monster_zone,choose_index_list) |> player_battle_info.monster_zone
      # find the pos of the new summon monster
      pos = player_battle_info.get_monster_available_pos
      # remove the summon card from handcards
      handcards = List.delete_at(handcards,handcards_index)

      monster = card_data.become_monster
      monster = monster.update(presentation: presentation,presentation_changed: true)
      monster_zone = Dict.put monster_zone,pos,monster

      player_battle_info = player_battle_info.update(monster_zone: monster_zone,graveyardcards: graveyardcards,handcards: handcards)
      battle_data = battle_data.update([{player_atom,player_battle_info},{:normal_summoned,true}])

      targets = BattleCore.create_effect_targets player_id,:monster_zone,choose_index_list
      move_to_graveyard_effect = BattleCore.create_move_to_graveyard_effect targets,battle_data

      targets = BattleCore.create_effect_targets player_id,:monster_zone,[pos]
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
  def summon_card(player_id,card_data = Card[card_type: card_type],handcards_index,_presentation,battle_data)
  when card_type in [:spell_card,:trap_card] do

    player_atom = battle_data.get_player_atom player_id
    player_battle_info = battle_data.get_player_battle_info player_id

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
      battle_data.send_message_to_all_with_mask player_id,message_data,message_data_masked
    end
    {result,battle_data}
  end

  # normal summon monster that need tribute
  def summon_card(player_id,card_data = Card[card_type: :monster_card,level: level],handcards_index,presentation,battle_data)
  when level > 4 do
    player_battle_info = battle_data.get_player_battle_info player_id

    result = :ok

    id_index_list = Enum.map player_battle_info.monster_zone,fn({index,monster})->
      {monster.id,index}
    end
    summoned_count = player_battle_info.monster_summoned_amount
    if card_data.can_be_tribute_normal_summoned?(summoned_count) == false do
      result = :not_enough_tribute_monster_for_normal_summon
    end

    if result == :ok do
      tribute_number = card_data.get_normal_summon_tribute_amount
      message_data = Proto.PT12.write(:choose_card,
        [:tribute_choose,tribute_number,[{battle_data.operator_id,:monster_zone,id_index_list}]])
      player_battle_info.send_message message_data
      choose_callback = fn(choose_scene_list,battle_data)->
        battle_data = battle_data.choose_callback nil
        summon_monster_after_tribute_choose(player_id,choose_scene_list,handcards_index,presentation,battle_data)
      end
      battle_data = battle_data.choose_callback choose_callback
    end
    {result,battle_data}
  end

  # normal summon monster without tribute
  def summon_card(player_id,card_data = Card[card_type: :monster_card,level: level],handcards_index,presentation,battle_data)
  when level<5 do
    player_atom = battle_data.get_player_atom player_id
    player_battle_info = battle_data.get_player_battle_info player_id

    result = :ok

    if player_battle_info.is_monster_zone_full? do
      result = :already_have_5_monsters
    end

    if result == :ok do
      handcards = List.delete_at(player_battle_info.handcards,handcards_index)

      pos = player_battle_info.get_monster_available_pos

      monster = card_data.become_monster
      monster = monster.update(presentation: presentation,presentation_changed: true)
      monster_zone = Dict.put(player_battle_info.monster_zone,pos,monster)

      player_battle_info = player_battle_info.update(handcards: handcards,monster_zone: monster_zone)

      battle_data = battle_data.update [{player_atom,player_battle_info},{:normal_summoned,true}]


      targets = BattleCore.create_effect_targets(player_id,:monster_zone,[pos])
      summon_effect = BattleCore.create_summon_effect(handcards_index,monster.id,presentation,targets)
      message_data = Proto.PT12.write :effects,[summon_effect]
      summon_effect_masked = BattleCore.create_summon_effect(handcards_index,0,presentation,targets)
      message_data_masked = Proto.PT12.write :effects,[summon_effect_masked]

      if presentation == :defense_down do
        battle_data.send_message_to_all_with_mask player_id,message_data,message_data_masked
      else
        Proto.PT12.write(:effects,[summon_effect]) |> battle_data.send_message_to_all
      end
    end
    {result,battle_data}
  end
end