defmodule EffectCore do
  require Lager

  def execute_skill_effects(player_id,scene_type,index,skill,choose_result_list,battle_data,callback \\ nil) do
    skill_effects = Enum.sort skill.skill_effects,&(&1.priority<&2.priority)
    execute_effect(player_id,scene_type,index,skill_effects,choose_result_list,battle_data,callback)
  end

############################################################
  # execute
############################################################
  def execute_effect(_player_id,scene_type,index,[],_,battle_data,callback) do
    result = :ok
    if callback != nil do
      # because the callback parameters is saved before effect execution, so the index is the old version
      # we need to execute the callback before clean_up_delete_cards
      {result,battle_data} = callback.(scene_type,index,battle_data)
    end
    # clean the delete card which set to 0
    # becuase all choose happended before effect execution so the delete operation need to be delay,so that the index will not be invalid
    # !!!!!!!!!! try some new idea !!!!!!!!!!!!!!!!
    # scene_types = [:handcard_zone,:graveyard_zone,:deck_zone,:extra_deck_zone,:banished_zone]
    # player_atom = battle_data.get_player_atom player_id
    # player_battle_info = battle_data.get_player_battle_info player_id
    # player_battle_info = List.foldl scene_types,player_battle_info,fn(scene_type,player_battle_info)->
    #   cards = player_battle_info.get_cards_of_scene scene_type
    #   cards = Enum.filter cards,&(&1!=0)
    #   scene_atom = IDUtil.get_scene_atom scene_type
    #   player_battle_info.update([{scene_atom,cards}])
    # end
    # battle_data = battle_data.update([{player_atom,player_battle_info}])
    {result,battle_data}
  end
  # 将{1}张{自己1/对方2/双方0}{手卡7,怪兽1,魔法陷阱区域2,地形5,额外6}{5星以上/不限0}{暗属性1/不限0}{怪兽1/魔法2/陷阱卡3/不限0}移去{墓地3,除外8,卡组4}
  # 1;1;7;5;1;1;3
  # 1;0;1,2;5;1;1;3 也可以这样表示双方战斗区域
  def execute_effect(player_id,scene_type,index,[SkillEffect[id: 1,params: params_str]|skill_effects],choose_result_list,battle_data,callback) do
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

    choose_scene_list = hd choose_result_list
    choose_result_list = tl choose_result_list

    {choose_result_list,index} = List.foldl choose_scene_list,{choose_result_list,index},
    fn({choose_player_id,choose_scene_type,choose_index_list},{choose_result_list,index})->
      # update fire efffect card index
      # Lager.info "~p ~p ~p",[choose_player_id,choose_scene_type,scene_type]
      if (choose_player_id == player_id) and (choose_scene_type == scene_type) and (choose_scene_type in [:handcard_zone,:graveyard_zone,:deck_zone,:extra_deck_zone,:banished_zone]) do
        index = List.foldl choose_index_list,index,fn(choose_index,index)->
          if choose_index < index do
            index - 1
          else
            index
          end
        end
      else
        index
      end
      # update choose result list of the remind
      choose_result_list = Enum.map choose_result_list,fn(choose_scene_list_1)->
        Enum.map choose_scene_list_1,fn({player_id_1,choose_scene_type_1,choose_index_list_1})->
          if (choose_player_id == player_id_1) and (choose_scene_type == choose_scene_type_1) do
            choose_index_list_1 = Enum.map choose_index_list_1,fn(choose_index_1)->
              List.foldl choose_index_list,choose_index_1,fn(choose_index,choose_index_1)->
                if choose_index < choose_index_1 do
                  choose_index_1 - 1
                else
                  choose_index_1
                end
              end
            end
            {player_id_1,choose_scene_type_1,choose_index_list_1}
          else
            {player_id_1,choose_scene_type_1,choose_index_list_1}
          end
        end
      end
      {choose_result_list,index}
    end

    # TODO: refactor
    battle_data = List.foldl choose_scene_list,battle_data,fn({player_id,source_scene_type,choose_index_list},battle_data)->
      player_atom = battle_data.get_player_atom player_id
      player_battle_info = battle_data.get_player_battle_info player_id
      source_cards = player_battle_info.get_cards_of_scene source_scene_type
      target_cards = player_battle_info.get_cards_of_scene target_scene_type

      # case source_scene_type do
      #   x when x in [:handcard_zone,:graveyard_zone,:deck_zone,:extra_deck_zone,:banished_zone]->
      #     choose_index_list = Enum.filter choose_index_list,&(Enum.at(source_cards,&1)!=0)
      #   x when x in [:spell_trap_zone,:monster_zone]->
      #     choose_index_list = Enum.filter choose_index_list,&(Dict.get(source_cards,&1)!=nil)
      # end

      if length(choose_index_list) > 0 do
        case source_scene_type do
          x when x in [:handcard_zone,:graveyard_zone,:deck_zone,:extra_deck_zone,:banished_zone]->
            target_cards = List.foldl choose_index_list,target_cards,&([Enum.at(source_cards,&1)|&2])
            # source_cards = List.foldl choose_index_list,source_cards,fn(choose_index,source_cards)->
            #   # List.replace_at source_cards,index,0
            # end
            source_id_index_list = player_battle_info.get_id_index_list_of_scene x
            source_cards = Enum.filter_map source_id_index_list,fn({_,index})-> !(index in choose_index_list) end,fn({id,_})-> id end
          x when x in [:spell_trap_zone,:monster_zone]->
            target_cards = List.foldl choose_index_list,target_cards,fn(choose_index,target_cards)->
              if Dict.get(source_cards,choose_index)==nil do
                target_cards
              else
                [Dict.get(source_cards,choose_index).id|target_cards]
              end
            end
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
      end
      battle_data
    end
    execute_effect player_id,scene_type,index,skill_effects,choose_result_list,battle_data,callback
  end

  # opponent darw one card
  def execute_effect(player_id,scene_type,index,[SkillEffect[id: 3]|skill_effects],choose_result_list,battle_data,callback) do
    opponent_player_id = battle_data.get_opponent_player_id player_id
    opponent_player_battle_info = battle_data.get_player_battle_info opponent_player_id
    draw_card_id = hd opponent_player_battle_info.deckcards
    deckcards = tl opponent_player_battle_info.deckcards
    handcards = opponent_player_battle_info.handcards++[draw_card_id]
    opponent_player_atom = battle_data.get_opponent_player_atom player_id
    opponent_player_battle_info = opponent_player_battle_info.update([{:deckcards, deckcards},{:handcards,handcards}])
    battle_data = battle_data.update([{opponent_player_atom,opponent_player_battle_info}])
    draw_effect = BattleCore.create_draw_card_effect opponent_player_id,draw_card_id
    message = Proto.PT12.write(:effects,[draw_effect])
    draw_effect_masked = BattleCore.create_draw_card_effect opponent_player_id,0
    message_masked = Proto.PT12.write(:effects,[draw_effect_masked])
    battle_data.send_message_to_all_with_mask opponent_player_id,message,message_masked
    execute_effect player_id,scene_type,index,skill_effects,choose_result_list,battle_data,callback
  end

  # destory opponent card,cancel the card effect
  def execute_effect(player_id,scene_type,index,[SkillEffect[id: 4]|skill_effects],choose_result_list,battle_data,callback) do
    chain = hd battle_data.chain_queue
    {another_player_id,chain_scene_type,chain_index,_choose_result_list,_skill} = chain
    another_player_atom = battle_data.get_player_atom another_player_id
    another_player_battle_info = battle_data.get_player_battle_info another_player_id
    case chain_scene_type do
      x when x in [:spell_trap_zone,:monster_zone]->
        cards = another_player_battle_info.get_cards_of_scene chain_scene_type
        card_id = Dict.get(cards,chain_index).id
        cards = Dict.drop cards,[chain_index]
        graveyardcards = [card_id|another_player_battle_info.graveyardcards]
        scene_atom = IDUtil.get_scene_atom chain_scene_type
        another_player_battle_info = another_player_battle_info.update([{scene_atom,cards},{:graveyardcards,graveyardcards}])
        battle_data = battle_data.update([{another_player_atom,another_player_battle_info}])
    end
    targets = BattleCore.create_effect_targets another_player_id,chain_scene_type,[chain_index]
    move_to_graveyard_effect = BattleCore.create_move_to_graveyard_effect targets,battle_data
    message = Proto.PT12.write(:effects,[move_to_graveyard_effect])
    battle_data.send_message_to_all message
    chain_queue = tl battle_data.chain_queue
    battle_data = battle_data.chain_queue chain_queue
    execute_effect player_id,scene_type,index,skill_effects,choose_result_list,battle_data,callback
  end

end
