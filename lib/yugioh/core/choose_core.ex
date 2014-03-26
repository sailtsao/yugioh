defmodule ChooseCore do
  require Lager
  def drop_handcard_choose player_id,battle_data,callback do
    player_battle_info = battle_data.get_player_battle_info player_id
    id_index_list = player_battle_info.get_id_index_list_of_scene(:handcard_zone)
    drop_number = length(id_index_list)-6
    message = Proto.PT12.write(:choose_card,[:drop_handcard_choose,drop_number,[{player_id,:handcard_zone,id_index_list}]])
    battle_data.send_message player_id,message
    choose_callback = fn(choose_scene_list,battle_data)->
      battle_data = battle_data.choose_callback nil
      callback.(choose_scene_list,battle_data)
    end
    battle_data = battle_data.choose_callback choose_callback
    {:ok,battle_data}
  end

  def tribute_choose player_id,tribute_number,battle_data,callback do
    player_battle_info = battle_data.get_player_battle_info player_id
    id_index_list = player_battle_info.get_id_index_list_of_scene(:monster_zone)
    message = Proto.PT12.write(:choose_card,[:tribute_choose,tribute_number,[{player_id,:monster_zone,id_index_list}]])
    battle_data.send_message player_id,message
    choose_callback = fn(choose_scene_list,battle_data)->
      battle_data = battle_data.choose_callback nil
      callback.(choose_scene_list,battle_data)
    end
    battle_data = battle_data.choose_callback choose_callback
    {:ok,battle_data}
  end

  def choose player_id,scene_type,index,skill,battle_data,callback do
    # get effects of skill & sort the effects by priority
    skill_effects = Enum.sort skill.skill_effects,&(&1.priority<&2.priority)
    execute_choose player_id,scene_type,index,skill_effects,battle_data,callback,[]
  end

  def execute_choose(_,_,_,[],battle_data,callback,choose_result_list) do
    Lager.debug "callback choose result list ~p",[choose_result_list]
    callback.(choose_result_list,battle_data)
  end

  def execute_choose(player_id,scene_type,index,[SkillEffect[id: 1,params: params_str]|skill_effects],
    battle_data,callback,choose_result_list) do

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

    player_battle_info = battle_data.get_player_battle_info player_id

    opponent_id = battle_data.get_opponent_player_id player_id
    opponent_battle_info = battle_data.get_opponent_player_battle_info player_id

    info = {player_id,scene_type,index}
    choose_scene_list = []
    case scene_belong do
      :self->
        choose_scene_list = Enum.map source_scene_types,fn(scene_type)->
          id_index_list = Util.get_id_index_list_from_scene(player_battle_info,scene_type,card_type,attribute,level_limit,info)
          {player_id,scene_type,id_index_list}
        end
      :opponent->
        choose_scene_list = Enum.map source_scene_types,fn(scene_type)->
          id_index_list = Util.get_id_index_list_from_scene(opponent_battle_info,scene_type,card_type,attribute,level_limit,info)
          {opponent_id,scene_type,id_index_list}
        end
      :both->
        choose_scene_list = List.foldl source_scene_types,choose_scene_list,fn(scene_type,choose_scene_list)->
          player_id_index_list = Util.get_id_index_list_from_scene(player_battle_info,scene_type,card_type,attribute,level_limit,info)
          opponent_id_index_list = Util.get_id_index_list_from_scene(opponent_battle_info,scene_type,card_type,attribute,level_limit,info)
          if Enum.empty?(player_id_index_list) == false do
            choose_scene_list = choose_scene_list++[{player_id,scene_type,player_id_index_list}]
          end
          if Enum.empty?(opponent_id_index_list) == false do
            choose_scene_list = choose_scene_list++[{opponent_id,scene_type,opponent_id_index_list}]
          end
          choose_scene_list
        end
    end
    message_data = Proto.PT12.write(:choose_card,[:handcard_tribute_choose,card_count,choose_scene_list])
    player_battle_info.send_message message_data
    choose_callback = fn(choose_scene_list,battle_data)->
      battle_data = battle_data.choose_callback nil
      choose_result_list = choose_result_list ++ [choose_scene_list]
      execute_choose player_id,scene_type,index,skill_effects,battle_data,callback,choose_result_list
    end
    battle_data = battle_data.choose_callback choose_callback
    {:ok,battle_data}
  end

  def execute_choose(player_id,scene_type,index,[SkillEffect[id: 2]|skill_effects],battle_data,callback,choose_result_list) do
    execute_choose player_id,scene_type,index,skill_effects,battle_data,callback,choose_result_list
  end
  def execute_choose(player_id,scene_type,index,[SkillEffect[id: 3]|skill_effects],battle_data,callback,choose_result_list) do
    execute_choose player_id,scene_type,index,skill_effects,battle_data,callback,choose_result_list
  end
  def execute_choose(player_id,scene_type,index,[SkillEffect[id: 4]|skill_effects],battle_data,callback,choose_result_list) do
    execute_choose player_id,scene_type,index,skill_effects,battle_data,callback,choose_result_list
  end
end