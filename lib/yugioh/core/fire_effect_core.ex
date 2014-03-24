defmodule FireEffectCore do
  require Lager

  def fire_effect(player_id,:handcard_zone,index,battle_data) do
    result = :ok
    player_battle_info = battle_data.get_player_battle_info player_id
    card_id = Enum.at(player_battle_info.handcards,index)
    card_data = Data.Cards.get(card_id)
    if card_data.card_type != :spell_card do
      result = :only_spell_card_can_fire_from_hand
    end

    # TODO: multi skills situation unhandled
    [skill] = card_data.get_normal_skills

    if skill.is_conditions_satisfied?(player_id,:handcard_zone,index,battle_data) != true do
      result = :card_cant_fire_effect
    end

    if result == :ok do
      # summon this card first
      {:ok,battle_data,pos} = SummonCore.summon_spell_card_for_fire(player_id,card_data,index,battle_data)
      # get new player_battle_info after summon
      player_battle_info = battle_data.get_player_battle_info player_id

      spell_trap = Dict.get player_battle_info.spell_trap_zone,pos

      {result,battle_data} = ChooseCore.choose player_id,:spell_trap_zone,pos,skill,battle_data,fn(choose_result_list,battle_data)->
        fire_effect_declare player_id,:spell_trap_zone,pos,skill,spell_trap.id,choose_result_list,battle_data
      end
    end
    {result,battle_data}
  end

  def fire_effect(player_id,:spell_trap_zone,index,battle_data) do
    result = :ok
    player_battle_info = battle_data.get_player_battle_info player_id
    spell_trap = Dict.get(player_battle_info.spell_trap_zone,index)

    [skill] = spell_trap.get_normal_skills

    if skill.is_conditions_satisfied?(player_id,:spell_trap_zone,index,battle_data) != true do
      result = :card_cant_fire_effect
    end

    if result == :ok do
      card_presentation_change_effect = BattleCore.create_card_presentation_change_effect(spell_trap.id,:attack,player_id,:spell_trap_zone,index)
      message = Proto.PT12.write(:effects,[card_presentation_change_effect])
      battle_data.send_message_to_all message
      {result,battle_data} = ChooseCore.choose player_id,:spell_trap_zone,index,skill,battle_data,fn(choose_result_list,battle_data)->
        fire_effect_declare player_id,:spell_trap_zone,index,skill,spell_trap.id,choose_result_list,battle_data
      end
    end
    {result,battle_data}
  end

  def fire_effect(player_id,:monster_zone,index,battle_data) do
    result = :ok
    # player_atom = battle_data.get_player_atom player_id
    player_battle_info = battle_data.get_player_battle_info player_id
    monster = Dict.get(player_battle_info.monster_zone,index)
    [skill] = monster.get_normal_skills

    if ConditionCore.is_skill_conditions_satisfied(player_id,:monster_zone,index,skill,battle_data,[]) == false do
      result = :card_cant_fire_effect
    end

    if result == :ok do
      {result,battle_data} = ChooseCore.choose player_id,:monster_zone,index,skill,battle_data,fn(choose_result_list,battle_data)->
        fire_effect_declare player_id,:monster_zone,index,skill,monster.id,choose_result_list,battle_data
      end
    end
    {result,battle_data}
  end

  def fire_effect_declare player_id,scene_type,index,skill,card_id,choose_result_list,battle_data do
    battle_data = battle_data.chain_queue([{player_id,scene_type,index,choose_result_list,skill}|battle_data.chain_queue])
    opponent_player_id = battle_data.get_opponent_player_id player_id
    # opponent_battle_info = battle_data.get_player_battle_info opponent_player_id
    if ChainCore.skill_chain_available?(player_id,scene_type,index,:opponent_fire_effect_phase,battle_data) do
      answer_callback = fn(answer,battle_data)->
        battle_data = battle_data.answer_callback nil
        case answer do
          :no->
            pause_message = Proto.PT12.write(:pause,[])
            battle_data.send_message player_id,pause_message
            ChainCore.execute_chain_queue battle_data
          :yes->
            # chained
            player_atom = battle_data.get_player_atom player_id
            player_battle_info = battle_data.get_player_battle_info player_id
            spell_trap = Dict.get(player_battle_info.spell_trap_zone,index).state :chained
            spell_trap_zone = Dict.put player_battle_info.spell_trap_zone,index,spell_trap
            player_battle_info = player_battle_info.spell_trap_zone spell_trap_zone
            battle_data = battle_data.update([{player_atom,player_battle_info}])
            battle_data = battle_data.operator_id opponent_player_id
            {:ok,battle_data}
        end
      end
      message = Proto.PT12.write(:chain_ask,[card_id])
      battle_data.send_message opponent_player_id,message

      battle_data = battle_data.answer_callback answer_callback

      # pause self becuase opoonent is operating
      pause_message = Proto.PT12.write(:pause,[])
      battle_data.send_message player_id,pause_message

      # send pause to oppoenent becuase he is pause state when chain_queue > 1
      if(length(battle_data.chain_queue) > 1) do
        pause_message = Proto.PT12.write(:pause,[])
        battle_data.send_message opponent_player_id,pause_message
      end
      {:ok,battle_data}
    else
      # no chain available
      # send
      if(length(battle_data.chain_queue) > 1) do
        pause_message = Proto.PT12.write(:pause,[])
        battle_data.send_message opponent_player_id,pause_message
      end
      ChainCore.execute_chain_queue battle_data
    end
  end

end