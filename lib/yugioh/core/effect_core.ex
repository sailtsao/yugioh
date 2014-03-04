# defrecord SkillEffect,id: 0,params: ""
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

  # 将{1}张{手卡}里{5}星以上{暗属性}怪兽移去墓地 -- 1;7;5;1
  def execute_effect(1,params_str,battle_data,rest_skill_effects,params_dict) do
    [card_count,scene_type_id,level_limit,attribute_id] = String.split(params_str,";",trim: true) |> Enum.map &(binary_to_integer &1)
    scene_type = Yugioh.Proto.PT12.scene_type_from scene_type_id
    attribute = Yugioh.Proto.PT12.attribute_from attribute_id

    player_battle_info = BattleCore.get_operator_battle_info battle_data

    # filter scene
    cards = Enum.with_index(Util.get_cards_from_scene(player_battle_info,scene_type))

    # filter attribute&level
    cards = Enum.filter cards,fn({card_id,_})->
      card_data = Yugioh.Data.Cards.get(card_id)
      if attribute == :none do
        card_data.level >= level_limit
      else
        (card_data.level >= level_limit)&&(card_data.atrribute == attribute)
      end
    end

    index_list = Enum.map cards,fn({card_id,index})->
      {card_id,index}
    end

    BattleCore.send_choose_message player_battle_info.player_pid,:handcard_tribute_choose,:self,scene_type,card_count,index_list
    battle_data.update(phase: {:choose_tribute_card_for_effect_1,battle_data.phase,params_str,rest_skill_effects,params_dict})
  end

  # 将{1}张{手卡}里{5}星以上{暗属性}怪兽移去墓地 -- 1;7;5;1
  def resume_execute_effect_after_choose choose_index_list,
  battle_data = BattleData[phase: {:choose_tribute_card_for_effect_1,old_phase,params_str,rest_skill_effects,params_dict}] do
    [_card_count,scene_type_id,_level_limit,_attribute_id] = String.split(params_str,";",trim: true) |> Enum.map &(binary_to_integer &1)
    scene_type = Yugioh.Proto.PT12.scene_type_from scene_type_id
    player_id = battle_data.operator_id
    player_atom = BattleCore.get_operator_atom battle_data
    player_battle_info = BattleCore.get_operator_battle_info battle_data

    cards=Util.get_cards_from_scene(player_battle_info,scene_type)
    move_to_graveyard_targets = List.foldl choose_index_list,[],&(&2 ++ [Target.new(player_id: player_id,scene_type: scene_type,index: &1)])
    graveyardcards = List.foldl choose_index_list,player_battle_info.graveyardcards,&([Enum.at(cards,&1)|&2])
    if is_list cards do      
      cards = Enum.filter_map Enum.with_index(cards),fn({_,index})-> !Enum.member?(choose_index_list,index) end,fn({card_id,_})-> card_id end
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
      cards = Dict.drop cards,choose_index_list
    end
    
    cards_scene_atom = BattleCore.get_scene_atom scene_type

    player_battle_info = player_battle_info.update([{:graveyardcards,graveyardcards},{cards_scene_atom,cards}])

    battle_data = battle_data.update([{:phase, old_phase},{player_atom,player_battle_info}])

    move_to_graveyard_effect = Effect.new(type: :move_to_graveyard_effect,
      params: BattleCore.get_graveyard_params_string(battle_data),
      targets: move_to_graveyard_targets)
    message_data = Yugioh.Proto.PT12.write(:effects,[move_to_graveyard_effect])
    send battle_data.player1_battle_info.player_pid , {:send,message_data}
    send battle_data.player2_battle_info.player_pid , {:send,message_data}        
    case rest_skill_effects do
      []->
        battle_data
      _->
        [skill_effect|rest_skill_effects] = rest_skill_effects
        execute_effect(skill_effect.id,skill_effect.params,battle_data,rest_skill_effects,params_dict)
    end  
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
end