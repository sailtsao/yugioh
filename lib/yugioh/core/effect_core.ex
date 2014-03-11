defmodule EffectCore do

  def execute_skill_effects(skill,battle_data,params_dict) do
    skill_effects = Enum.sort skill.skill_effects,&(&1.priority<&2.priority)
    case skill_effects do
      []->
        battle_data
      _->
        [skill_effect|rest_skill_effects] = skill_effects
        execute_effect(skill_effect.id,skill_effect.params,battle_data,rest_skill_effects,params_dict)
    end
  end  

############################################################
  # execute
############################################################  
  # 将{1}张{自己1/对方2/双方0}{手卡7,怪兽1,魔法陷阱区域2,地形5,额外6}{5星以上/不限0}{暗属性1/不限0}{怪兽1/魔法2/陷阱卡3/不限0}移去{墓地3,除外8,卡组4} 
  # 1;1;7;5;1;1;3
  # 1;0;1,2;5;1;1;3 也可以这样表示双方战斗区域
  def execute_effect(1,params_str,battle_data,rest_skill_effects,params_dict) do
    [card_count_str,scene_belong_str,scene_type_ids_str,level_limit_str,attribute_id_str,card_type_id_str,target_scene_type_id_str] =  
      String.split(params_str,";",trim: true)
    card_count = binary_to_integer card_count_str
    scene_belong = binary_to_integer scene_belong_str |> IDUtil.scene_belong_from
    source_scene_types = String.split(scene_type_ids_str,",",trim: true) |>
      Enum.map &(binary_to_integer &1) |> 
      Enum.map &(IDUtil.scene_type_from(&1))      
    level_limit = binary_to_integer level_limit_str
    attribute = binary_to_integer attribute_id_str |> IDUtil.attribute_from
    card_type = binary_to_integer card_type_id_str |> IDUtil.card_type_from
    target_scene_type = binary_to_integer(target_scene_type_id_str) |> IDUtil.scene_type_from

    operator_id = battle_data.operator_id
    operator_battle_info = BattleCore.get_operator_battle_info battle_data
    
    opponent_id = BattleCore.get_opponent_player_id battle_data
    opponent_battle_info = BattleCore.get_opponent_player_battle_info battle_data

    choose_scene_list = case scene_belong do
      :self->
        Enum.map source_scene_types,fn(scene_type)->
          id_index_list = Util.get_id_index_list_from_scene(operator_battle_info,scene_type,attribute,level_limit)
          {operator_id,scene_type,id_index_list}
        end
      :opponent->
        Enum.map source_scene_types,fn(scene_type)->
          id_index_list = Util.get_id_index_list_from_scene(opponent_battle_info,scene_type,attribute,level_limit)
          {opponent_id,scene_type,id_index_list}
        end
      :both->
        Enum.flat_map source_scene_types,fn(scene_type)->
          operator_id_index_list = Util.get_id_index_list_from_scene(operator_battle_info,scene_type,attribute,level_limit)
          opponent_id_index_list = Util.get_id_index_list_from_scene(opponent_battle_info,scene_type,attribute,level_limit)
          [{operator_id,scene_type,operator_id_index_list},{opponent_id,scene_type,opponent_id_index_list}]
        end
    end    

    BattleCore.send_choose_message operator_battle_info.player_pid,:handcard_tribute_choose,card_count,choose_scene_list
    battle_data.update(phase: {:choose_tribute_card_for_effect_1,battle_data.phase,params_str,rest_skill_effects,params_dict})
  end
  
  # 卡片上场
  def execute_effect(2,_params_str,battle_data,rest_skill_effects,params_dict) do
    handcards_index = params_dict[:handcards_index]
    presentation = params_dict[:presentation]

    player_id = battle_data.operator_id
    player_atom = BattleCore.get_operator_atom battle_data
    player_battle_info = BattleCore.get_operator_battle_info battle_data
    summon_card_id = Enum.at(player_battle_info.handcards,handcards_index)
    card = Yugioh.Data.Cards.get(summon_card_id)
    monster = Monster[id: card.id,attack: card.attack,defense: card.defense,level: card.level,presentation: presentation,presentation_changed: true]

    #!!!!!!!!! set delete card_id to 0 to delay the delete operation !!!!!!!!!!!!
    # handcards = List.replace_at(player_battle_info.handcards,handcards_index,0)
    handcards = List.delete_at(player_battle_info.handcards,handcards_index)

    avaible_pos = :lists.subtract([2,1,3,0,4],Dict.keys(player_battle_info.monster_card_zone))

    pos = hd(avaible_pos)
        
    monster_card_zone = Dict.put(player_battle_info.monster_card_zone,pos,monster)

    player_battle_info = player_battle_info.update(handcards: handcards,monster_card_zone: monster_card_zone)

    battle_data = battle_data.update [{player_atom,player_battle_info}]

    presentation_id = Yugioh.Proto.PT12.presentation_id_from presentation
    summon_effect = Effect.new(type: :summon_effect,params: "#{handcards_index};#{summon_card_id};#{presentation_id}",targets: [Target.new(player_id: player_id,scene_type: :monster_card_zone,index: pos)])
    summon_effect_masked = Effect.new(type: :summon_effect,params: "#{handcards_index};0;#{presentation_id}",targets: [Target.new(player_id: player_id,scene_type: :monster_card_zone,index: pos)])    

    if presentation == :defense_down do
      case player_atom do
        :player1_battle_info->
          message_data = Yugioh.Proto.PT12.write(:effects,[summon_effect])
          send battle_data.player2_battle_info.player_pid , {:send,message_data}
          message_data = Yugioh.Proto.PT12.write(:effects,[summon_effect_masked])
          send battle_data.player2_battle_info.player_pid , {:send,message_data}
        :player2_battle_info->
          message_data = Yugioh.Proto.PT12.write(:effects,[summon_effect_masked])
          send battle_data.player1_battle_info.player_pid , {:send,message_data}
          message_data = Yugioh.Proto.PT12.write(:effects,[summon_effect])
          send battle_data.player2_battle_info.player_pid , {:send,message_data}
      end
    else
      message_data = Yugioh.Proto.PT12.write(:effects,[summon_effect])
      send battle_data.player1_battle_info.player_pid , {:send,message_data}
      send battle_data.player2_battle_info.player_pid , {:send,message_data}          
    end

    case rest_skill_effects do
      []->
        battle_data
      _->
        [skill_effect|rest_skill_effects] = rest_skill_effects
        execute_effect(skill_effect.id,skill_effect.params,battle_data,rest_skill_effects,params_dict)
    end
  end  
  
  
############################################################
  # resume
############################################################  
  # 将{1}张{自己1/对方2/双方0}{手卡7,怪兽1,魔法陷阱区域2,地形5,额外6}{5星以上/不限0}{暗属性1/不限0}{怪兽1/魔法2/陷阱卡3/不限0}移去{墓地3,除外8,卡组4} 
  # 1;1;7;5;1;1;3
  # 1;0;1,2;5;1,1;3 也可以这样表示双方战斗区域
  def resume_execute_effect_after_choose choose_scene_list,
  battle_data = BattleData[phase: {:choose_tribute_card_for_effect_1,old_phase,params_str,rest_skill_effects,params_dict}] do
    [card_count_str,scene_belong_str,scene_type_ids_str,level_limit_str,attribute_id_str,card_type_id_str,target_scene_type_id_str] =  
      String.split(params_str,";",trim: true)

    card_count = binary_to_integer card_count_str
    scene_belong = binary_to_integer scene_belong_str |> IDUtil.scene_belong_from
    source_scene_types = String.split(scene_type_ids_str,",",trim: true) |>
      Enum.map &(binary_to_integer &1) |> 
      Enum.map &(IDUtil.scene_type_from(&1))      
    level_limit = binary_to_integer level_limit_str
    attribute_id = binary_to_integer attribute_id_str |> IDUtil.attribute_from
    card_type = binary_to_integer card_type_id_str |> IDUtil.card_type_from
    target_scene_type = binary_to_integer(target_scene_type_id_str) |> IDUtil.scene_type_from

    
    player_id = battle_data.operator_id
    player_atom = BattleCore.get_operator_atom battle_data
    player_battle_info = BattleCore.get_operator_battle_info battle_data

    battle_data = List.foldl choose_scene_list,battle_data,fn({player_id,scene_type,choose_index_list})->
      source_cards = Util.get_cards_from_scene(player_battle_info,scene_type)
      target_cards = Util.get_cards_from_scene(player_battle_info,target_scene_type)
      targets = BattleCore.create_effect_targets player_id,scene_type,choose_index_list
      target_cards = List.foldl choose_index_list,target_cards,&([Enum.at(source_cards,&1)|&2])
      if is_list source_cards do      
        source_cards = Enum.filter_map(Enum.with_index(source_cards),fn({_,index})-> !Enum.member?(choose_index_list,index) end,fn({card_id,_})-> card_id end)
        # update handcard_index if choose handcards
        if scene_type == :handcard_zone do
          params_dict = List.foldl choose_index_list,params_dict,fn(index,params_dict)->
            if (params_dict[:handcards_index] != nil) && (index < params_dict[:handcards_index]) do
              Dict.put params_dict,:handcards_index, params_dict[:handcards_index]-1
            else
              params_dict
            end
          end
        end

      # cards = List.foldl choose_index_list,cards,fn(index,cards)-> 
      #   List.replace_at cards,index,0
      # end
      else
        cards = Dict.drop source_cards,choose_index_list
      end
    end
    
    # cards_scene_atom = BattleCore.get_scene_atom scene_type

    # player_battle_info = player_battle_info.update([{:graveyardcards,graveyardcards},{cards_scene_atom,cards}])

    # battle_data = battle_data.update([{:phase, old_phase},{player_atom,player_battle_info}])

    # move_to_graveyard_effect = Effect.new(type: :move_to_graveyard_effect,
    #   params: BattleCore.get_graveyard_params_string(battle_data),
    #   targets: move_to_graveyard_targets)
    # message_data = Yugioh.Proto.PT12.write(:effects,[move_to_graveyard_effect])
    # send battle_data.player1_battle_info.player_pid , {:send,message_data}
    # send battle_data.player2_battle_info.player_pid , {:send,message_data}        
    # case rest_skill_effects do
    #   []->
    #     battle_data
    #   _->
    #     [skill_effect|rest_skill_effects] = rest_skill_effects
    #     execute_effect(skill_effect.id,skill_effect.params,battle_data,rest_skill_effects,params_dict)
    # end  
  end

end