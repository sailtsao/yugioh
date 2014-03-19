defmodule ChooseCore do
  def choose player_id,scene_type,index,skill,callback,battle_data do
    execute_skill_choose skill,battle_data,[{:info,{player_id,scene_type,index}},{:callback,callback}]
  end

  def execute_skill_choose(skill,battle_data,params_dict) do
    skill_effects = Enum.sort skill.skill_effects,&(&1.priority<&2.priority)
    case skill_effects do
      []->
        params_dict[:callback].([],battle_data)
      _->
        [skill_effect|rest_skill_effects] = skill_effects
        execute_choose(skill_effect.id,skill_effect.params,battle_data,rest_skill_effects,params_dict,[])
    end
  end

  def choose_finish rest_skill_effects,battle_data,params_dict,choose_result_list do
    case rest_skill_effects do
      []->
        params_dict[:callback].(choose_result_list,battle_data)
      _->
        [skill_effect|rest_skill_effects] = rest_skill_effects
        execute_choose(skill_effect.id,skill_effect.params,battle_data,rest_skill_effects,params_dict,choose_result_list)
    end
  end


  def execute_choose(1,params_str,battle_data,rest_skill_effects,params_dict,choose_result_list) do
    [card_count_str,scene_belong_str,scene_type_ids_str,level_limit_str,attribute_id_str,card_type_id_str,_target_scene_type_id_str] =
      String.split(params_str,";",trim: true)
    card_count = binary_to_integer card_count_str
    scene_belong = binary_to_integer(scene_belong_str) |> IDUtil.scene_belong_from
    source_scene_types = String.split(scene_type_ids_str,",",trim: true) |>
      Enum.map(&(binary_to_integer &1)) |>
      Enum.map(&(IDUtil.scene_type_from(&1)))
    level_limit = binary_to_integer level_limit_str
    attribute = binary_to_integer(attribute_id_str) |> IDUtil.attribute_from
    card_type = binary_to_integer(card_type_id_str) |> IDUtil.card_type_from
    # target_scene_type = binary_to_integer(target_scene_type_id_str) |> IDUtil.scene_type_from

    operator_id = battle_data.operator_id
    operator_battle_info = battle_data.operator_battle_info

    opponent_id = battle_data.opponent_player_id
    opponent_battle_info = battle_data.opponent_player_battle_info

    info = params_dict[:info]
    choose_scene_list = []
    case scene_belong do
      :self->
        choose_scene_list = Enum.map source_scene_types,fn(scene_type)->
          id_index_list = Util.get_id_index_list_from_scene(operator_battle_info,scene_type,card_type,attribute,level_limit,info)
          {operator_id,scene_type,id_index_list}
        end
      :opponent->
        choose_scene_list = Enum.map source_scene_types,fn(scene_type)->
          id_index_list = Util.get_id_index_list_from_scene(opponent_battle_info,scene_type,card_type,attribute,level_limit,info)
          {opponent_id,scene_type,id_index_list}
        end
      :both->
        choose_scene_list = List.foldl source_scene_types,choose_scene_list,fn(scene_type,choose_scene_list)->
          operator_id_index_list = Util.get_id_index_list_from_scene(operator_battle_info,scene_type,card_type,attribute,level_limit,info)
          opponent_id_index_list = Util.get_id_index_list_from_scene(opponent_battle_info,scene_type,card_type,attribute,level_limit,info)
          if Enum.empty?(operator_id_index_list) == false do
            choose_scene_list = choose_scene_list++[{operator_id,scene_type,operator_id_index_list}]
          end
          if Enum.empty?(opponent_id_index_list) == false do
            choose_scene_list++[{opponent_id,scene_type,opponent_id_index_list}]
          end
        end
    end
    message_data = Proto.PT12.write(:choose_card,[:handcard_tribute_choose,card_count,choose_scene_list])
    operator_battle_info.send_message message_data
    choose_callback = fn(choose_scene_list,battle_data)->
      battle_data = battle_data.choose_callback nil
      choose_result_list = choose_result_list ++ [choose_scene_list]
      choose_finish rest_skill_effects,battle_data,params_dict,choose_result_list
    end
    battle_data = battle_data.choose_callback choose_callback
    {:ok,battle_data}
  end

  def execute_choose(2,params_str,battle_data,rest_skill_effects,params_dict,choose_result_list) do
    choose_finish rest_skill_effects,battle_data,params_dict,choose_result_list
  end
  def execute_choose(3,params_str,battle_data,rest_skill_effects,params_dict,choose_result_list) do
    choose_finish rest_skill_effects,battle_data,params_dict,choose_result_list
  end
  def execute_choose(4,params_str,battle_data,rest_skill_effects,params_dict,choose_result_list) do
    choose_finish rest_skill_effects,battle_data,params_dict,choose_result_list
  end
end