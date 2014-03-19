defmodule Proto.PT12 do
  require Lager

  def read(12000,<<phase_number::8>>) do
    {:ok,{:change_phase_to,[IDUtil.phase_from(phase_number)]}}
  end

  def read(12001,bin) do
    <<handcards_index::8,presentation_id::8,summon_type_id::8>> = bin
    presentation = IDUtil.presentation_from presentation_id
    summon_type = IDUtil.summon_type_from summon_type_id
    {:ok,{:summon,[handcards_index,presentation,summon_type]}}
  end

  def read(12003,<<source_card_index::8>>) do
    {:ok,{:attack,[source_card_index]}}
  end

  def read(12004,<<card_index::8>>) do
    {:ok,{:flip_card,[card_index]}}
  end

  def read(12006,_) do
    {:ok,{:battle_load_finish,[]}}
  end

  def read(12007,bin) do
    <<scene_type_id::8,index::8>> = bin
    scene_type = IDUtil.scene_type_from(scene_type_id)
    {:ok,{:get_card_operations,[scene_type,index]}}
  end

  def read(12008,bin) do
    <<len::16,rest::binary>> = bin
    [_,scene_list] = List.foldl List.duplicate(1, len),[rest,[]],fn(_,[bin_data,scene_list])->
      <<player_id::32,scene_type_id::8,rest1::binary>> = bin_data
      <<len::16,rest2::binary>> = rest1
      [rest3,index_list] = List.foldl List.duplicate(1,len),[rest2,[]],fn(_,[bin_data,index_list])->
        <<index::8,rest::binary>> = bin_data
        index_list = [index|index_list]
        [rest, index_list]
      end
      scene_list = [{player_id,IDUtil.scene_type_from(scene_type_id),index_list}|scene_list]
      [rest3, scene_list]
    end
    {:ok,{:choose_card,[scene_list]}}
  end

  def read(12010,bin) do
    <<player_id::32,scene_type_id::8>> = bin
    {:ok,{:get_cards_of_scene_type,[player_id,IDUtil.scene_type_from(scene_type_id)]}}
  end

  def read(12011,bin) do
    <<scene_type_id::8,index::8>> = bin
    {:ok,{:fire_effect,[IDUtil.scene_type_from(scene_type_id),index]}}
  end

  def read(12012,<<answer_id::8>>) do
    {:ok,{:chain_answer,[IDUtil.answer_from(answer_id)]}}
  end


  def write(:change_phase_to,[phase]) do
    phase_number = IDUtil.phase_id_from phase
    ProtoUtil.pack(12000,<<phase_number::8>>)
  end

  def write(:new_turn_draw,[turn_count,phase,operator_id,draw_card_id]) do
    phase_number = IDUtil.phase_id_from phase
    data = <<turn_count::8,phase_number::8,operator_id::32,draw_card_id::32>>
    ProtoUtil.pack(12002,data)
  end

  def write(:flip_card,[player_id,card_index,card_id,new_status]) do
    new_status = IDUtil.presentation_id_from new_status
    data = <<player_id::32,card_index::8,card_id::32,new_status::8>>
    ProtoUtil.pack(12004,data)
  end

  def write(:battle_end,[result,win_player_id,lose_player_id]) do
    result_number = case result do
      :win ->
        1
      :draw ->
        2
    end
    data = <<result_number::8,win_player_id::32,lose_player_id::32>>
    ProtoUtil.pack(12005,data)
  end

  def write(:get_card_operations,operations) do
    operations_list = Enum.map operations,fn(op) ->
      op_id = IDUtil.operation_type_id_from(op)
      <<op_id::8>>
    end
    operations_binary = iolist_to_binary(operations_list)
    data = <<length(operations_list)::16,operations_binary::binary>>
    ProtoUtil.pack(12007,data)
  end

  def write(:choose_card,[choose_type,choose_number,choose_scenes_list]) do
    Lager.debug "choose_type [~p] choose_number [~p] choose_scenes_list [~p]",[choose_type,choose_number,choose_scenes_list]
    choose_scenes_binary = List.foldl choose_scenes_list,<<>>,fn({player_id,scene_type,id_index_list},bin)->
      id_index_binary = List.foldl id_index_list,<<>>,fn({card_id,index},bin)->
        bin <> <<card_id::32,index::8>>
      end
      bin <> <<player_id::32,IDUtil.scene_type_id_from(scene_type)::8,length(id_index_list)::16,id_index_binary::binary>>
    end
    data = <<IDUtil.choose_type_id_from(choose_type)::8,choose_number::8,length(choose_scenes_list)::16,choose_scenes_binary::binary>>
    ProtoUtil.pack(12008,data)
  end

  def write(:effects,effects) do
    Lager.debug "effects [~p]",[effects]
    effects_binary = List.foldl effects,<<>>,fn(effect,acc)->
      acc <> <<IDUtil.effect_type_id_from(effect.type)::32>> <> ProtoUtil.pack_string(effect.params) <>
      <<length(effect.targets)::16>> <>
      List.foldl(effect.targets,<<>>,&(&2 <> <<&1.player_id::32,IDUtil.scene_type_id_from(&1.scene_type)::8,&1.index::8>>))
    end
    data = <<length(effects)::16,effects_binary::binary>>
    ProtoUtil.pack(12009,data)
  end

  def write(:get_cards_of_scene_type,[player_id,scene_type,graveyardcards]) do
    Lager.debug "get_cards_of_scene_type [~p]",[[player_id,scene_type,graveyardcards]]
    graveyardcards_binary = List.foldl graveyardcards,<<>>,&(&2 <> <<&1::32>>)
    data = <<player_id::32,IDUtil.scene_type_id_from(scene_type)::8,length(graveyardcards)::16,graveyardcards_binary::binary>>
    ProtoUtil.pack(12010,data)
  end

  def write(:chain_ask,[card_id]) do
    ProtoUtil.pack(12012,<<card_id::32>>)
  end

end