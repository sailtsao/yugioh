defmodule EffectCore do
  require Lager

  def execute_skill_effects(skill,battle_data,params_dict) do
    skill_effects = Enum.sort skill.skill_effects,&(&1.priority<&2.priority)
    case skill_effects do
      []->
        {:ok,battle_data}
      _->
        [skill_effect|rest_skill_effects] = skill_effects
        execute_effect(skill_effect.id,skill_effect.params,battle_data,rest_skill_effects,params_dict)
    end
  end

  # def fire_effect_declare battle_data,player_id,scene_type,index do
  #   # check if opponent player have spell/trap card to chain this fire effect operation
  #   # :chained
  #   :ok
  # end

  def effect_finish rest_skill_effects,battle_data,params_dict do
    case rest_skill_effects do
      []->
        if params_dict[:effect_callback] != nil do
          {result,battle_data} = params_dict[:effect_callback].(battle_data)
          Lager.debug "battle_data after finish effect [~p]", [battle_data]
        else
          result = :ok
        end
        {result,battle_data}
      _->
        [skill_effect|rest_skill_effects] = rest_skill_effects
        {result,battle_data} = execute_effect(skill_effect.id,skill_effect.params,battle_data,rest_skill_effects,params_dict)
        Lager.debug "battle_data after effect [~p]", [battle_data]
        {result,battle_data}
    end          
  end  

  
############################################################
  # execute
############################################################  
  # 将{1}张{自己1/对方2/双方0}{手卡7,怪兽1,魔法陷阱区域2,地形5,额外6}{5星以上/不限0}{暗属性1/不限0}{怪兽1/魔法2/陷阱卡3/不限0}移去{墓地3,除外8,卡组4} 
  # 1;1;7;5;1;1;3
  # 1;0;1,2;5;1;1;3 也可以这样表示双方战斗区域
  def execute_effect(1,params_str,battle_data,rest_skill_effects,params_dict) do
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

    choose_scene_list = []
    case scene_belong do
      :self->
        choose_scene_list = Enum.map source_scene_types,fn(scene_type)->
          id_index_list = Util.get_id_index_list_from_scene(operator_battle_info,scene_type,card_type,attribute,level_limit)
          {operator_id,scene_type,id_index_list}
        end
      :opponent->
        choose_scene_list = Enum.map source_scene_types,fn(scene_type)->
          id_index_list = Util.get_id_index_list_from_scene(opponent_battle_info,scene_type,card_type,attribute,level_limit)
          {opponent_id,scene_type,id_index_list}
        end
      :both->
        choose_scene_list = List.foldl source_scene_types,choose_scene_list,fn(scene_type,choose_scene_list)->
          operator_id_index_list = Util.get_id_index_list_from_scene(operator_battle_info,scene_type,card_type,attribute,level_limit)
          opponent_id_index_list = Util.get_id_index_list_from_scene(opponent_battle_info,scene_type,card_type,attribute,level_limit)
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
      execute_effect_after_choose(1,choose_scene_list,params_str,rest_skill_effects,params_dict,battle_data)
    end
    battle_data = battle_data.choose_callback choose_callback
    {:ok,battle_data}
  end
  
  # 怪兽卡片上场
  def execute_effect(2,_params_str,battle_data,rest_skill_effects,params_dict) do
    handcards_index = params_dict[:handcards_index]
    presentation = params_dict[:presentation]

    player_id = battle_data.operator_id
    player_atom = battle_data.operator_atom
    player_battle_info = battle_data.operator_battle_info 
    summon_card_id = Enum.at(player_battle_info.handcards,handcards_index)
    card = Yugioh.Data.Cards.get(summon_card_id)
    monster = Monster[id: card.id,attack: card.attack,defense: card.defense,level: card.level,presentation: presentation,presentation_changed: true]

    handcards = List.delete_at(player_battle_info.handcards,handcards_index)

    avaible_pos = :lists.subtract([2,1,3,0,4],Dict.keys(player_battle_info.monster_card_zone))
    pos = hd(avaible_pos)
        
    monster_card_zone = Dict.put(player_battle_info.monster_card_zone,pos,monster)

    player_battle_info = player_battle_info.update(handcards: handcards,monster_card_zone: monster_card_zone)

    battle_data = battle_data.update [{player_atom,player_battle_info}]

    targets = BattleCore.create_effect_targets player_id,:monster_card_zone,[pos]
    summon_effect = BattleCore.create_summon_effect handcards_index,summon_card_id,presentation,targets    
    message_data = Proto.PT12.write(:effects,[summon_effect])

    if presentation == :defense_down do
      summon_effect_masked = BattleCore.create_summon_effect handcards_index,0,presentation,targets
      message_data_masked = Proto.PT12.write(:effects,[summon_effect_masked])
      battle_data.send_message_to_all_with_mask player_id,message_data,message_data_masked      
    else
      message_data = Proto.PT12.write(:effects,[summon_effect])
      battle_data.send_message_to_all message_data
    end
    effect_finish rest_skill_effects,battle_data,params_dict
  end  
  
  
############################################################
  # resume
############################################################  
  # 将{1}张{自己1/对方2/双方0}{手卡7,怪兽1,魔法陷阱区域2,地形5,额外6}{5星以上/不限0}{暗属性1/不限0}{怪兽1/魔法2/陷阱卡3/不限0}移去{墓地3,除外8,卡组4} 
  # 1;1;7;5;1;1;3
  # 1;0;1,2;5;1,1;3 也可以这样表示双方战斗区域
  def execute_effect_after_choose 1,choose_scene_list,params_str,rest_skill_effects,params_dict,battle_data do
    [card_count_str,scene_belong_str,scene_type_ids_str,level_limit_str,attribute_id_str,card_type_id_str,target_scene_type_id_str] =  
      String.split(params_str,";",trim: true)

    #TODO: check card count
    _card_count = binary_to_integer(card_count_str)
    #TODO: check scene belong
    _scene_belong = binary_to_integer(scene_belong_str) |> IDUtil.scene_belong_from
    #TODO: check source_scene_types
    _source_scene_types = String.split(scene_type_ids_str,",",trim: true) |>
      Enum.map(&(binary_to_integer &1)) |> 
      Enum.map(&(IDUtil.scene_type_from(&1)))
    #TODO: check level_limit
    _level_limit = binary_to_integer level_limit_str
    #TODO: check attribute_id
    _attribute_id = binary_to_integer(attribute_id_str) |> IDUtil.attribute_from
    #TODO: check card type
    _card_type = binary_to_integer(card_type_id_str) |> IDUtil.card_type_from
    target_scene_type = binary_to_integer(target_scene_type_id_str) |> IDUtil.scene_type_from    

    # TODO: refactor
    {battle_data,params_dict} = List.foldl choose_scene_list,{battle_data,params_dict},
    fn({player_id,source_scene_type,choose_index_list},{battle_data,params_dict})->
      player_atom = battle_data.get_player_atom player_id
      player_battle_info = battle_data.get_player_battle_info player_id

      case source_scene_type do
        # list mode
        x when x in [:handcard_zone,:graveyard_zone,:deck_zone,:extra_deck_zone,:banished_zone]->
          source_cards = Util.get_cards_from_scene(player_battle_info,source_scene_type)
          target_cards = Util.get_cards_from_scene(player_battle_info,target_scene_type)
          target_cards = List.foldl choose_index_list,target_cards,&([Enum.at(source_cards,&1)|&2])          
          source_cards = Enum.filter_map(Enum.with_index(source_cards),
            fn({_,index})-> !Enum.member?(choose_index_list,index) end,
            fn({card_id,_})-> card_id end)
          # update handcard_index if choose handcards
          if (source_scene_type == :handcard_zone) and (params_dict[:handcards_index] != nil) do
            params_dict = List.foldl choose_index_list,params_dict,fn(index,params_dict)->
              if index < params_dict[:handcards_index] do
                Dict.put params_dict,:handcards_index, params_dict[:handcards_index]-1
              else
                params_dict
              end
            end            
          end
        :spell_trap_zone->
          source_cards = Util.get_cards_from_scene(player_battle_info,:spell_trap_zone)
          target_cards = Util.get_cards_from_scene(player_battle_info,target_scene_type)
          target_cards = List.foldl choose_index_list,target_cards,&([Dict.get(source_cards,&1).id|&2])
          source_cards = Dict.drop source_cards,choose_index_list
        :monster_card_zone->
          source_cards = Util.get_cards_from_scene(player_battle_info,:monster_card_zone)
          target_cards = Util.get_cards_from_scene(player_battle_info,target_scene_type)
          target_cards = List.foldl choose_index_list,target_cards,&([Dict.get(source_cards,&1).id|&2])
          source_cards = Dict.drop source_cards,choose_index_list
      end      

      source_scene_atom = IDUtil.get_scene_atom source_scene_type
      target_scene_atom = IDUtil.get_scene_atom target_scene_type
      player_battle_info = player_battle_info.update([{target_scene_atom,target_cards},{source_scene_atom,source_cards}])
      battle_data = battle_data.update([{player_atom,player_battle_info}])
      case target_scene_type do
        x when x in [:graveyard_zone]->
          targets = BattleCore.create_effect_targets player_id,source_scene_type,choose_index_list
          move_to_graveyard_effect = BattleCore.create_move_to_graveyard_effect targets,battle_data          
          message_data = Proto.PT12.write(:effects,[move_to_graveyard_effect])
          battle_data.send_message_to_all message_data
      end
      {battle_data,params_dict}
    end
    effect_finish rest_skill_effects,battle_data,params_dict
  end

end