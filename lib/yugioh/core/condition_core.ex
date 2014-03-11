defmodule ConditionCore do
  def is_skill_conditions_satisfied(skill,battle_data) do
    and_result = List.foldl skill.and_conditions,true,&(&2 && ConditionCore.is_condition_satisfied(&1.id,&1.params,battle_data))
    or_result = List.foldl skill.or_conditions,true,&(&2 || ConditionCore.is_condition_satisfied(&1.id,&1.params,battle_data))
    and_result && or_result
  end

  def is_condition_satisfied(1,params_str,battle_data) do
    player_battle_info = BattleCore.get_operator_battle_info battle_data
    [scene_type_id,card_count,level_limit,attribute_id] = String.split(params_str,";",trim: true) |> Enum.map &(binary_to_integer &1)
    scene_type = Yugioh.Proto.PT12.scene_type_from scene_type_id
    attribute = Yugioh.Proto.PT12.attribute_from attribute_id    
    cards = Util.get_cards_from_scene player_battle_info,scene_type
    cards = Enum.filter cards,fn(card_id)->
      card_data = Yugioh.Data.Cards.get(card_id)
      if attribute == :none do
        card_data.level >= level_limit
      else
        (card_data.level >= level_limit)&&(card_data.atrribute == attribute)
      end
    end
    Enum.count(cards)>=card_count
  end

  def is_condition_satisfied(2,params_str,battle_data) do
    [target_type_id,scene_type_id,compare_id,limit_count] = String.split(params_str,";",trim: true) |> Enum.map &(binary_to_integer &1)
    target_type = case target_type_id do
      0->
        :all_target
      1->
        :self_target
      2->
        :opponent_target
    end
    scene_type = Yugioh.Proto.PT12.scene_type_from scene_type_id

    cards_count =  case target_type do
      :all_target->
        Enum.count(Util.get_cards_from_scene(scene_type,battle_data.player1_battle_info))+Enum.count(Util.get_cards_from_scene(scene_type,battle_data.player2_battle_info))
      :self_target->
        Enum.count(Util.get_cards_from_scene(scene_type,BattleCore.get_operator_battle_info(battle_data)))
      :opponent_target->
        Enum.count(Util.get_cards_from_scene(scene_type,BattleCore.get_opponent_player_battle_info(battle_data)))
    end 

    case compare_id do
      0->
        cards_count == limit_count
      1->
        cards_count > limit_count
      2->
        cards_count < limit_count
    end
  end

end