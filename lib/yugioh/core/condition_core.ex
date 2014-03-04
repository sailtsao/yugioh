defmodule ConditionCore do
  def is_skill_conditions_satisfied(skill,player_battle_info) do
    and_result = List.foldl skill.and_conditions,true,&(&2 && ConditionCore.is_condition_satisfied(&1.id,&1.params,player_battle_info))
    or_result = List.foldl skill.or_conditions,true,&(&2 || ConditionCore.is_condition_satisfied(&1.id,&1.params,player_battle_info))
    and_result && or_result
  end

  def is_condition_satisfied(1,params_str,player_battle_info) do
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
end