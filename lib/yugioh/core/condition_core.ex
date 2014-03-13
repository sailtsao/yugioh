defmodule ConditionCore do
  require Lager

  def is_skill_conditions_satisfied(skill,battle_data) do
    and_result = List.foldl skill.and_conditions,true,&(&2 && ConditionCore.is_condition_satisfied(&1.id,&1.params,battle_data))
    or_result = List.foldl skill.or_conditions,true,&(&2 || ConditionCore.is_condition_satisfied(&1.id,&1.params,battle_data))
    Lager.debug "and or result [~p]",[and_result,or_result]
    and_result && or_result
  end
# {自己1/对方2/双方0}{手卡7,怪兽1,魔法陷阱区域2,地形5,额外6}{大于5星/不限0}{暗属性1/不限0}{怪兽1/魔法2/陷阱卡3/不限0}数量{大于1/小于2/等于0}{1}  
  # 1;1;7;5;1;1;3
  # "1;7;5;1;1;1;0"
  # 1;0;1,2;5;1;1;3 也可以这样表示双方战斗区域
  def is_condition_satisfied(1,params_str,battle_data) do    
    Lager.debug "params [~p]",[params_str]
    [scene_belong_str,scene_type_ids_str,level_limit_str,attribute_id_str,card_type_id_str,compare_id_str,limit_count_str] =  
      String.split(params_str,";",trim: true)

    scene_belong = binary_to_integer(scene_belong_str) |> IDUtil.scene_belong_from
    source_scene_types = String.split(scene_type_ids_str,",",trim: true) |>
      Enum.map(&(binary_to_integer &1)) |> 
      Enum.map(&(IDUtil.scene_type_from(&1)))
    level_limit = binary_to_integer(level_limit_str)
    attribute = binary_to_integer(attribute_id_str) |> IDUtil.attribute_from
    card_type = binary_to_integer(card_type_id_str) |> IDUtil.card_type_from 
    compare = binary_to_integer(compare_id_str) |> IDUtil.compare_from
    limit_count = binary_to_integer(limit_count_str)
        
    operator_battle_info = BattleCore.get_operator_battle_info battle_data
    
    opponent_battle_info = BattleCore.get_opponent_player_battle_info battle_data

    cards_count = case scene_belong do
      :self->
        List.foldl source_scene_types,0,fn(scene_type,count)->
          Lager.debug "params [~p]",[[operator_battle_info,scene_type,card_type,attribute,level_limit]]
          count + length(Util.get_id_index_list_from_scene(operator_battle_info,scene_type,card_type,attribute,level_limit))
        end
      :opponent->
        List.foldl source_scene_types,0,fn(scene_type,count)->
          count + length(Util.get_id_index_list_from_scene(opponent_battle_info,scene_type,card_type,attribute,level_limit))
        end
      :both->
        List.foldl source_scene_types,0,fn(scene_type,count)->
          operator_id_index_list = Util.get_id_index_list_from_scene(operator_battle_info,scene_type,card_type,attribute,level_limit)
          opponent_id_index_list = Util.get_id_index_list_from_scene(opponent_battle_info,scene_type,card_type,attribute,level_limit)
          count + length(operator_id_index_list++opponent_id_index_list)
        end
    end    
    Lager.debug "cards_count [~p]",[[compare,cards_count,limit_count]]
    case compare do
      :greater->
        cards_count > limit_count
      :less->
        cards_count < limit_count
      :equal->
        cards_count == limit_count
    end    
  end

  # def is_condition_satisfied(2,params_str,battle_data) do
  #   [target_type_id,scene_type_id,compare_id,limit_count] = String.split(params_str,";",trim: true) |> Enum.map &(binary_to_integer &1)
  #   target_type = case target_type_id do
  #     0->
  #       :all_target
  #     1->
  #       :self_target
  #     2->
  #       :opponent_target
  #   end
  #   scene_type = IDUtil.scene_type_from scene_type_id

  #   cards_count =  case target_type do
  #     :all_target->
  #       Enum.count(Util.get_cards_from_scene(scene_type,battle_data.player1_battle_info))+Enum.count(Util.get_cards_from_scene(scene_type,battle_data.player2_battle_info))
  #     :self_target->
  #       Enum.count(Util.get_cards_from_scene(scene_type,BattleCore.get_operator_battle_info(battle_data)))
  #     :opponent_target->
  #       Enum.count(Util.get_cards_from_scene(scene_type,BattleCore.get_opponent_player_battle_info(battle_data)))
  #   end 

  #   case compare_id do
  #     0->
  #       cards_count == limit_count
  #     1->
  #       cards_count > limit_count
  #     2->
  #       cards_count < limit_count
  #   end
  # end

end