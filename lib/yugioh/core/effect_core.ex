defmodule EffectCore do
  require Lager

  def execute_skill_effects(player_id,skill,choose_result_list,battle_data,params_dict) do
    skill_effects = Enum.sort skill.skill_effects,&(&1.priority<&2.priority)
    case skill_effects do
      []->
        {:ok,battle_data}
      _->
        [skill_effect|rest_skill_effects] = skill_effects
        execute_effect(player_id,skill_effect.id,skill_effect.params,choose_result_list,battle_data,rest_skill_effects,params_dict)
    end
  end

  def execute_chain_queue battle_data = BattleData[chain_queue: []] do
    battle_data = battle_data.operator_id battle_data.turn_player_id
    {:ok,battle_data}
  end

  def execute_chain_queue battle_data = BattleData[chain_queue: chain_queue] do
    chain = hd chain_queue
    {player_id,scene_type,index,choose_result_list,skill} = chain
    battle_data = battle_data.chain_queue(tl(chain_queue))
    {:ok,battle_data} = execute_skill_effects(player_id,skill,choose_result_list,battle_data,[])
    player_battle_info = battle_data.get_player_battle_info player_id
    player_atom = battle_data.get_player_atom player_id
    if scene_type == :spell_trap_zone do
      spell_trap = player_battle_info.spell_trap_zone[index]
      player_battle_info = player_battle_info.graveyardcards [spell_trap.id|player_battle_info.graveyardcards]
      spell_trap_zone = Dict.drop player_battle_info.spell_trap_zone,[index]
      player_battle_info = player_battle_info.spell_trap_zone spell_trap_zone
      battle_data = battle_data.update([{player_atom,player_battle_info}])
      targets = BattleCore.create_effect_targets player_id,scene_type,[index]
      move_to_graveyard_effect = BattleCore.create_move_to_graveyard_effect targets,battle_data
      message = Proto.PT12.write(:effects,[move_to_graveyard_effect])
      battle_data.send_message_to_all message
    end
    execute_chain_queue battle_data
  end


  def fire_effect_declare player_id,scene_type,index,skill,card_id,choose_result_list,battle_data do
    battle_data = battle_data.chain_queue([{player_id,scene_type,index,choose_result_list,skill}|battle_data.chain_queue])
    opponent_player_id = battle_data.get_opponent_player_id player_id
    opponent_battle_info = battle_data.get_player_battle_info opponent_player_id
    if ChainCore.skill_chain_available?(player_id,card_id,battle_data) do
      answer_callback = fn(answer,battle_data)->
        battle_data = battle_data.answer_callback nil
        case answer do
          :no->
            execute_chain_queue battle_data
          :yes->
            # chained
            player_atom = battle_data.get_player_atom player_id
            player_battle_info = battle_data.get_player_battle_info player_id
            spell_trap = Dict.get(player_battle_info.spell_trap_zone,index).state :chained
            spell_trap_zone = Dict.put player_battle_info.spell_trap_zone,index,spell_trap
            player_battle_info = player_battle_info.spell_trap_zone spell_trap_zone
            battle_data = battle_data.update([{player_atom,player_battle_info}])
            battle_data = battle_data.operator_id opponent_player_id
            {:ok,battle_data}
        end
      end
      message = Proto.PT12.write(:chain_ask,[card_id])
      opponent_battle_info.send_message message
      battle_data = battle_data.answer_callback answer_callback
      {:ok,battle_data}
    else
      # no chain available
      execute_chain_queue battle_data
    end
  end

  def effect_finish player_id,rest_skill_effects,choose_result_list,battle_data,params_dict do
    case rest_skill_effects do
      []->
        if params_dict[:effect_callback] != nil do
          {result,battle_data} = params_dict[:effect_callback].(battle_data)
        else
          result = :ok
        end
        battle_data = clean_up_delete_cards player_id,battle_data
        {result,battle_data}
      _->
        [skill_effect|rest_skill_effects] = rest_skill_effects
        execute_effect(player_id,skill_effect.id,skill_effect.params,choose_result_list,battle_data,rest_skill_effects,params_dict)
    end
  end

  def clean_up_delete_cards player_id,battle_data do

    scene_types = [:handcard_zone,:graveyard_zone,:deck_zone,:extra_deck_zone,:banished_zone]
    player_atom = battle_data.get_player_atom player_id
    player_battle_info = battle_data.get_player_battle_info player_id
    # Lager.info "effect finish player_battle_info [~p]",[player_battle_info]
    player_battle_info = List.foldl scene_types,player_battle_info,fn(scene_type,player_battle_info)->
      cards = Util.get_cards_from_scene(player_battle_info,scene_type)
      cards = Enum.filter cards,&(&1!=0)
      scene_atom = IDUtil.get_scene_atom scene_type
      player_battle_info.update([{scene_atom,cards}])
    end
    # Lager.info "clean up player_battle_info [~p] [~p]",[player_battle_info,battle_data]
    battle_data.update([{player_atom,player_battle_info}])
  end


############################################################
  # execute
############################################################
  # 将{1}张{自己1/对方2/双方0}{手卡7,怪兽1,魔法陷阱区域2,地形5,额外6}{5星以上/不限0}{暗属性1/不限0}{怪兽1/魔法2/陷阱卡3/不限0}移去{墓地3,除外8,卡组4}
  # 1;1;7;5;1;1;3
  # 1;0;1,2;5;1;1;3 也可以这样表示双方战斗区域
  def execute_effect(player_id,1,params_str,choose_result_list,battle_data,rest_skill_effects,params_dict) do
    [card_count_str,scene_belong_str,scene_type_ids_str,level_limit_str,attribute_id_str,card_type_id_str,target_scene_type_id_str] =
      String.split(params_str,";",trim: true)
    Lager.info "choose_result_list [~p]",[choose_result_list]
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
          source_cards = List.foldl choose_index_list,source_cards,fn(index,source_cards)->
            List.replace_at source_cards,index,0
          end
          # Lager.info "source_cards [~p]",[source_cards]
          # source_cards = Enum.filter_map(Enum.with_index(source_cards),
          #   fn({_,index})-> !Enum.member?(choose_index_list,index) end,
          #   fn({card_id,_})-> card_id end)
          # update handcard_index if choose handcards
          # if (source_scene_type == :handcard_zone) and (params_dict[:handcards_index] != nil) do
          #   params_dict = List.foldl choose_index_list,params_dict,fn(index,params_dict)->
          #     if index < params_dict[:handcards_index] do
          #       Dict.put params_dict,:handcards_index, params_dict[:handcards_index]-1
          #     else
          #       params_dict
          #     end
          #   end
          # end
        x when x in [:spell_trap_zone,:monster_zone]->
          source_cards = Util.get_cards_from_scene(player_battle_info,source_scene_type)
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
    # Lager.info "battle_data [~p]",[battle_data]
    effect_finish player_id,rest_skill_effects,choose_result_list,battle_data,params_dict
    # [card_count_str,scene_belong_str,scene_type_ids_str,level_limit_str,attribute_id_str,card_type_id_str,_target_scene_type_id_str] =
    #   String.split(params_str,";",trim: true)
    # card_count = binary_to_integer card_count_str
    # scene_belong = binary_to_integer(scene_belong_str) |> IDUtil.scene_belong_from
    # source_scene_types = String.split(scene_type_ids_str,",",trim: true) |>
    #   Enum.map(&(binary_to_integer &1)) |>
    #   Enum.map(&(IDUtil.scene_type_from(&1)))
    # level_limit = binary_to_integer level_limit_str
    # attribute = binary_to_integer(attribute_id_str) |> IDUtil.attribute_from
    # card_type = binary_to_integer(card_type_id_str) |> IDUtil.card_type_from
    # # target_scene_type = binary_to_integer(target_scene_type_id_str) |> IDUtil.scene_type_from

    # operator_id = battle_data.operator_id
    # operator_battle_info = battle_data.operator_battle_info

    # opponent_id = battle_data.opponent_player_id
    # opponent_battle_info = battle_data.opponent_player_battle_info

    # choose_scene_list = []
    # case scene_belong do
    #   :self->
    #     choose_scene_list = Enum.map source_scene_types,fn(scene_type)->
    #       id_index_list = Util.get_id_index_list_from_scene(operator_battle_info,scene_type,card_type,attribute,level_limit,[])
    #       {operator_id,scene_type,id_index_list}
    #     end
    #   :opponent->
    #     choose_scene_list = Enum.map source_scene_types,fn(scene_type)->
    #       id_index_list = Util.get_id_index_list_from_scene(opponent_battle_info,scene_type,card_type,attribute,level_limit,[])
    #       {opponent_id,scene_type,id_index_list}
    #     end
    #   :both->
    #     choose_scene_list = List.foldl source_scene_types,choose_scene_list,fn(scene_type,choose_scene_list)->
    #       operator_id_index_list = Util.get_id_index_list_from_scene(operator_battle_info,scene_type,card_type,attribute,level_limit,[])
    #       opponent_id_index_list = Util.get_id_index_list_from_scene(opponent_battle_info,scene_type,card_type,attribute,level_limit,[])
    #       if Enum.empty?(operator_id_index_list) == false do
    #         choose_scene_list = choose_scene_list++[{operator_id,scene_type,operator_id_index_list}]
    #       end
    #       if Enum.empty?(opponent_id_index_list) == false do
    #         choose_scene_list++[{opponent_id,scene_type,opponent_id_index_list}]
    #       end
    #     end
    # end
    # message_data = Proto.PT12.write(:choose_card,[:handcard_tribute_choose,card_count,choose_scene_list])
    # operator_battle_info.send_message message_data
    # choose_callback = fn(choose_scene_list,battle_data)->
    #   battle_data = battle_data.choose_callback nil
    #   execute_effect_after_choose(1,choose_scene_list,params_str,rest_skill_effects,params_dict,battle_data)
    # end
    # battle_data = battle_data.choose_callback choose_callback
    # {:ok,battle_data}
  end

  # 怪兽卡片上场
  # def execute_effect(_player_id,2,_params_str,_choose_result_list,battle_data,rest_skill_effects,params_dict) do
  #   effect_finish rest_skill_effects,choose_result_list,battle_data,params_dict
  # end

  # 对手抽卡1张
  def execute_effect(player_id,3,_params_str,choose_result_list,battle_data,rest_skill_effects,params_dict) do
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
    effect_finish player_id,rest_skill_effects,choose_result_list,battle_data,params_dict
  end

  def execute_effect(player_id,4,_params_str,choose_result_list,battle_data,rest_skill_effects,params_dict) do
    chain = hd battle_data.chain_queue
    {another_player_id,scene_type,index,choose_result_list,skill} = chain
    another_player_atom = battle_data.get_player_atom another_player_id
    another_player_battle_info = battle_data.get_player_battle_info another_player_id
    case scene_type do
      x when x in [:spell_trap_zone,:monster_zone]->
        cards = Util.get_cards_from_scene another_player_battle_info,scene_type
        card_id = Dict.get(cards,index).id
        cards = Dict.drop cards,[index]
        graveyardcards = [card_id|another_player_battle_info.graveyardcards]
        scene_atom = IDUtil.get_scene_atom scene_type
        another_player_battle_info = another_player_battle_info.update([{scene_atom,cards},{:graveyardcards,graveyardcards}])
        battle_data = battle_data.update([{another_player_atom,another_player_battle_info}])
    end
    targets = BattleCore.create_effect_targets another_player_id,scene_type,[index]
    move_to_graveyard_effect = BattleCore.create_move_to_graveyard_effect targets,battle_data
    message = Proto.PT12.write(:effects,[move_to_graveyard_effect])
    battle_data.send_message_to_all message
    chain_queue = tl battle_data.chain_queue
    battle_data = battle_data.chain_queue chain_queue
    effect_finish player_id,rest_skill_effects,choose_result_list,battle_data,params_dict
  end


############################################################
  # resume
############################################################
  # 将{1}张{自己1/对方2/双方0}{手卡7,怪兽1,魔法陷阱区域2,地形5,额外6}{5星以上/不限0}{暗属性1/不限0}{怪兽1/魔法2/陷阱卡3/不限0}移去{墓地3,除外8,卡组4}
  # 1;1;7;5;1;1;3
  # 1;0;1,2;5;1,1;3 也可以这样表示双方战斗区域
  # def execute_effect_after_choose 1,choose_scene_list,params_str,rest_skill_effects,params_dict,battle_data do
  #   [card_count_str,scene_belong_str,scene_type_ids_str,level_limit_str,attribute_id_str,card_type_id_str,target_scene_type_id_str] =
  #     String.split(params_str,";",trim: true)

  #   #TODO: check card count
  #   _card_count = binary_to_integer(card_count_str)
  #   #TODO: check scene belong
  #   _scene_belong = binary_to_integer(scene_belong_str) |> IDUtil.scene_belong_from
  #   #TODO: check source_scene_types
  #   _source_scene_types = String.split(scene_type_ids_str,",",trim: true) |>
  #     Enum.map(&(binary_to_integer &1)) |>
  #     Enum.map(&(IDUtil.scene_type_from(&1)))
  #   #TODO: check level_limit
  #   _level_limit = binary_to_integer level_limit_str
  #   #TODO: check attribute_id
  #   _attribute_id = binary_to_integer(attribute_id_str) |> IDUtil.attribute_from
  #   #TODO: check card type
  #   _card_type = binary_to_integer(card_type_id_str) |> IDUtil.card_type_from
  #   target_scene_type = binary_to_integer(target_scene_type_id_str) |> IDUtil.scene_type_from

  #   # TODO: refactor
  #   {battle_data,params_dict} = List.foldl choose_scene_list,{battle_data,params_dict},
  #   fn({player_id,source_scene_type,choose_index_list},{battle_data,params_dict})->
  #     player_atom = battle_data.get_player_atom player_id
  #     player_battle_info = battle_data.get_player_battle_info player_id

  #     case source_scene_type do
  #       # list mode
  #       x when x in [:handcard_zone,:graveyard_zone,:deck_zone,:extra_deck_zone,:banished_zone]->
  #         source_cards = Util.get_cards_from_scene(player_battle_info,source_scene_type)
  #         target_cards = Util.get_cards_from_scene(player_battle_info,target_scene_type)
  #         target_cards = List.foldl choose_index_list,target_cards,&([Enum.at(source_cards,&1)|&2])
  #         source_cards = Enum.filter_map(Enum.with_index(source_cards),
  #           fn({_,index})-> !Enum.member?(choose_index_list,index) end,
  #           fn({card_id,_})-> card_id end)
  #         # update handcard_index if choose handcards
  #         if (source_scene_type == :handcard_zone) and (params_dict[:handcards_index] != nil) do
  #           params_dict = List.foldl choose_index_list,params_dict,fn(index,params_dict)->
  #             if index < params_dict[:handcards_index] do
  #               Dict.put params_dict,:handcards_index, params_dict[:handcards_index]-1
  #             else
  #               params_dict
  #             end
  #           end
  #         end
  #       :spell_trap_zone->
  #         source_cards = Util.get_cards_from_scene(player_battle_info,:spell_trap_zone)
  #         target_cards = Util.get_cards_from_scene(player_battle_info,target_scene_type)
  #         target_cards = List.foldl choose_index_list,target_cards,&([Dict.get(source_cards,&1).id|&2])
  #         source_cards = Dict.drop source_cards,choose_index_list
  #       :monster_zone->
  #         source_cards = Util.get_cards_from_scene(player_battle_info,:monster_zone)
  #         target_cards = Util.get_cards_from_scene(player_battle_info,target_scene_type)
  #         target_cards = List.foldl choose_index_list,target_cards,&([Dict.get(source_cards,&1).id|&2])
  #         source_cards = Dict.drop source_cards,choose_index_list
  #     end

  #     source_scene_atom = IDUtil.get_scene_atom source_scene_type
  #     target_scene_atom = IDUtil.get_scene_atom target_scene_type
  #     player_battle_info = player_battle_info.update([{target_scene_atom,target_cards},{source_scene_atom,source_cards}])
  #     battle_data = battle_data.update([{player_atom,player_battle_info}])
  #     case target_scene_type do
  #       x when x in [:graveyard_zone]->
  #         targets = BattleCore.create_effect_targets player_id,source_scene_type,choose_index_list
  #         move_to_graveyard_effect = BattleCore.create_move_to_graveyard_effect targets,battle_data
  #         message_data = Proto.PT12.write(:effects,[move_to_graveyard_effect])
  #         battle_data.send_message_to_all message_data
  #     end
  #     {battle_data,params_dict}
  #   end
  #   effect_finish rest_skill_effects,battle_data,params_dict
  # end

end