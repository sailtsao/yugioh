defmodule SummonCore do
  require Lager

  @doc """
  special summon
  """
  def summon(player_id,handcards_index,presentation,:special_summon,battle_data = BattleData[phase: phase])
  when phase in [:mp1,:mp2] do
    result = :ok
    player_battle_info = battle_data.get_player_battle_info player_id

    card_id = Enum.at(player_battle_info.handcards,handcards_index)
    card_data = Data.Cards.get(card_id)
    skill = card_data.get_special_summon_skill

    if skill.is_conditions_satisfied?(player_id,:handcard_zone,handcards_index,battle_data) != true do
      result = :card_cant_be_special_summoned
    end

    if result == :ok do
      {result,battle_data} = ChooseCore.choose player_id,:handcard_zone,handcards_index,
      skill,battle_data,fn(choose_result_list,battle_data)->
        EffectCore.execute_skill_effects player_id,:handcard_zone,handcards_index,skill,choose_result_list,battle_data,fn(_,index,battle_data)->
          # Lager.info "~p",[battle_data]
          # Lager.info "~p",[index]
          battle_data = battle_data.summon_handcard_monster player_id,index,presentation,:special_summon
          {:ok,battle_data}
        end
      end
    end
    {result,battle_data}
  end

  @doc """
  normal summon
  """
  def summon(player_id,handcards_index,presentation,:normal_summon,battle_data = BattleData[phase: phase])
  when phase in [:mp1,:mp2] do
    player_battle_info = battle_data.get_player_battle_info player_id

    card_id = Enum.at(player_battle_info.handcards,handcards_index)
    card_data = Data.Cards.get(card_id)

    summon_card(player_id,card_data,handcards_index,presentation,battle_data)
  end

  def summon(_,_,_,:normal_summon,battle_data = BattleData[normal_summoned: true]) do
    {:already_normal_summoned,battle_data}
  end

  def summon(_,_,_,_,battle_data) do
    {:invalid_summon,battle_data}
  end

  # normal summon mosnter level greater than 4
  def summon_card(player_id,card_data = Card[card_type: :monster_card,level: level],handcards_index,presentation,battle_data)
  when level > 4 do

    result = :ok

    player_battle_info = battle_data.get_player_battle_info player_id
    summoned_count = player_battle_info.monster_zone_size
    if card_data.can_be_tribute_normal_summoned?(summoned_count) == false do
      result = :not_enough_tribute_monster_for_normal_summon
    end

    if result == :ok do
      tribute_number = card_data.get_normal_summon_tribute_amount
      {result,battle_data} = ChooseCore.tribute_choose player_id,tribute_number,battle_data,fn(choose_scene_list,battle_data)->
        tribute_summon(player_id,choose_scene_list,handcards_index,presentation,battle_data)
      end
    end
    {result,battle_data}
  end

  # normal summon monster level less than 5
  def summon_card(player_id,Card[card_type: :monster_card,level: level],handcards_index,presentation,battle_data)
  when level<5 do

    result = :ok

    player_battle_info = battle_data.get_player_battle_info player_id
    if player_battle_info.is_monster_zone_full? do
      result = :already_have_5_monsters
    end

    if result == :ok do
      battle_data = battle_data.summon_handcard_monster player_id,handcards_index,presentation,:normal_summon
    end
    {result,battle_data}
  end

  # place spell trap
  def summon_card(player_id,Card[card_type: card_type],handcards_index,_,battle_data)
  when card_type in [:spell_card,:trap_card] do

    result = :ok

    player_battle_info = battle_data.get_player_battle_info player_id
    if player_battle_info.is_spell_trap_zone_full? do
      result = :spell_trap_zone_full
    end

    if result == :ok do
      battle_data = battle_data.place_handcard_spell_trap player_id,handcards_index
    end
    {result,battle_data}
  end

  # tribute monster summon
  def tribute_summon(player_id,[{_,:monster_zone,choose_index_list}],handcards_index,presentation,battle_data) do
    result = :ok

    player_battle_info = battle_data.get_player_battle_info player_id
    handcards = player_battle_info.handcards
    summon_card_id = Enum.at(handcards,handcards_index)
    card_data = Data.Cards.get(summon_card_id)
    tribute_number = card_data.get_normal_summon_tribute_amount


    if Enum.count(choose_index_list) != tribute_number do
      result = :wrong_tribute_choose_number
    end

    if result == :ok do
      battle_data = battle_data.move_cards_to_graveryard player_id,:monster_zone,choose_index_list
      battle_data = battle_data.summon_handcard_monster player_id,handcards_index,presentation,:normal_summon
    end
    {result,battle_data}
  end

   # place spell trap
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
end