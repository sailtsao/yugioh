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

    if ConditionCore.is_skill_conditions_satisfied(player_id,:handcard_zone,index,skill,battle_data,[]) != true do
      result = :card_cant_fire_effect
    end

    if result == :ok do
      # summon this card first
      {:ok,battle_data,pos} = SummonCore.summon_spell_card_for_fire(player_id,card_data,index,battle_data)
      # get new player_battle_info after summon
      player_battle_info = battle_data.get_player_battle_info player_id

      spell_trap = Dict.get player_battle_info.spell_trap_zone,pos

      # choose callback
      choose_callback = fn(choose_result_list,battle_data)->
        Lager.info "choose_result_list [~p]",[choose_result_list]
        EffectCore.fire_effect_declare player_id,:spell_trap_zone,pos,skill,spell_trap.id,choose_result_list,battle_data
      end
      {result,battle_data} = ChooseCore.choose player_id,:spell_trap_zone,pos,skill,choose_callback,battle_data
    end
    {result,battle_data}
  end

  def fire_effect(player_id,:spell_trap_zone,index,battle_data) do
    result = :ok
    player_battle_info = battle_data.get_player_battle_info player_id
    spell_trap = Dict.get(player_battle_info.spell_trap_zone,index)

    [skill] = spell_trap.get_normal_skills

    if ConditionCore.is_skill_conditions_satisfied(player_id,:spell_trap_zone,index,skill,battle_data,[]) != true do
      result = :card_cant_fire_effect
    end

    if result == :ok do
      card_presentation_change_effect = BattleCore.create_card_presentation_change_effect(spell_trap.id,:attack,player_id,:spell_trap_zone,index)
      message = Proto.PT12.write(:effects,[card_presentation_change_effect])
      battle_data.send_message_to_all message
      player_battle_info = battle_data.get_player_battle_info player_id
      spell_trap = Dict.get player_battle_info.spell_trap_zone,index
      choose_callback = fn(choose_result_list,battle_data)->
        EffectCore.fire_effect_declare player_id,:spell_trap_zone,index,skill,spell_trap.id,choose_result_list,battle_data
      end
      {result,battle_data} = ChooseCore.choose player_id,:spell_trap_zone,index,skill,choose_callback,battle_data
    end
    {result,battle_data}
  end

  def fire_effect(player_id,:monster_zone,index,battle_data) do
    result = :ok
    player_atom = battle_data.get_player_atom player_id
    player_battle_info = battle_data.get_player_battle_info player_id
    monster = Dict.get(player_battle_info.monster_zone,index)
    [skill] = monster.get_normal_skills

    if ConditionCore.is_skill_conditions_satisfied(player_id,:monster_zone,index,skill,battle_data,[]) == false do
      result = :card_cant_fire_effect
    end

    if result == :ok do
      # execute_skill_func = fn(battle_data)->
      #   {result,battle_data} = EffectCore.execute_skill_effects(skill,battle_data,[])
      #   if result == :ok do
      #     player_battle_info = battle_data.get_player_battle_info player_id
      #     monster = Dict.get(player_battle_info.monster_zone,index)
      #     monster = monster.effect_fired(true)
      #     monster_zone = Dict.put(player_battle_info.monster_zone,index,monster)
      #     player_battle_info = player_battle_info.update(monster_zone: monster_zone)
      #     battle_data = battle_data.update([{player_atom,player_battle_info}])
      #   end
      #   {result,battle_data}
      # end
      choose_callback = fn(choose_result_list,battle_data)->
        EffectCore.fire_effect_declare player_id,:monster_zone,index,skill,monster.id,choose_result_list,battle_data
      end
      {result,battle_data} = ChooseCore.choose player_id,:monster_zone,index,skill,choose_callback,battle_data
      # {result,battle_data} = EffectCore.fire_effect_declare battle_data,:monster_zone,index,player_id,monster.id,execute_skill_func
    end
    {result,battle_data}
  end
end