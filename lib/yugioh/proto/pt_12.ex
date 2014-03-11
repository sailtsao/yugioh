defmodule Proto.PT12 do

  defp phase_id_from phase do
    case phase do
      :dp->
        1
      :sp->
        2
      :mp1->
        3
      :bp->
        4
      :mp2->
        5
      :ep->
        6
    end
  end

  defp phase_from phase_id do
    case phase_id do
      1->
        :dp
      2->
        :sp
      3->
        :mp1
      4->
        :bp
      5->
        :mp2
      6->
        :ep
    end
  end

  def check_phase_from check_phase_id do
    case check_phase_id do
      1->
        :draw_phase
      2->
        :suspend_phase
      3->
        :main_phase_1
      4->
        :battle_phase
      5->
        :main_phase_2
      6->
        :end_phase
    end
  end

  def check_phase_id_from check_phase do
    case check_phase do
      :draw_phase->
        1        
      :suspend_phase->
        2        
      :main_phase_1->
        3        
      :battle_phase->
        4        
      :main_phase_2->
        5        
      :end_phase->
        6        
    end
  end

  defp presentation_from presentation_id do
    case presentation_id do
      1->
        :attack
      2->
        :defense_down
      3->
        :defense_up
      4->
        :place
    end
  end

  def presentation_id_from presentation do
    case presentation do      
      :attack->
        1
      :defense_down->  
        2
      :defense_up->  
        3
      :place->
        4
    end
  end

  def scene_type_from scene_type_id do
    case scene_type_id do
      0->
        :player_zone
      1->
        :monster_card_zone
      2->
        :spell_trap_zone
      3->
        :graveyard_zone
      4->
        :deck_zone
      5->
        :field_card_zone
      6->
        :extra_deck_zone
      7->
        :handcard_zone
      8->
        :banished_zone
    end
  end

  def scene_type_id_from scene_type do
    case scene_type do
      :player_zone ->
        0
      :monster_card_zone->
        1
      :spell_trap_zone->
        2
      :graveyard_zone->
        3
      :deck_zone->
        4
      :field_card_zone->
        5
      :extra_deck_zone->
        6
      :handcard_zone->
        7
      :banished_zone->
        8
    end    
  end
  
  def operation_type_from operation_type_id do
    case operation_type_id do
      1->
        :summon_operation
      2->
        :place_operation
      3->
        :fire_effect_operation
      4->
        :attack_operation
      5->
        :change_to_attack_present_operation
      6->
        :change_to_defense_present_operation
      7->
        :reverse_operation
      8->
        :special_summon_operation
    end
  end

  def operation_type_id_from operation_type do
    case operation_type do
      :summon_operation->
        1
      :place_operation->
        2
      :fire_effect_operation->
        3
      :attack_operation->
        4
      :change_to_attack_present_operation->
        5
      :change_to_defense_present_operation->
        6
      :reverse_operation->
        7
      :special_summon_operation->
        8
    end    
  end

  def effect_type_from effect_type_id do
    case effect_type_id do
      1 ->
        :move_to_graveyard_effect
      2 ->
        :summon_effect
      3 ->
        :attack_effect
      4 ->
        :card_presentation_change_effect
    end
  end
  
  def effect_type_id_from effect_type do
    case effect_type do
      :move_to_graveyard_effect ->
        1
      :summon_effect ->
        2      
      :attack_effect ->
        3
      :card_presentation_change_effect ->
        4
    end
  end
  
  def choose_type_id_from choose_type do
    case choose_type do
      :tribute_choose ->
        1
      :attack_choose ->
        2
      :handcard_tribute_choose ->
        3
    end
  end

  def attribute_id_from attribute do
    case attribute do
      :none ->
        0
      :dark_attribute ->
        1
      # :light ->
      #   2
    end
  end
  
  def attribute_from attribute_id do
    case attribute_id do
      0 ->
        :none
      1 ->
        :dark_attribute
      # :light ->
      #   2
    end
  end

  def read(12000,bin) do
    <<phase_number::8>> = bin
    phase = phase_from phase_number
    {:ok,{:change_phase_to,[phase]}}
  end

  def read(12001,bin) do
    <<handcards_index::8,presentation_id::8,is_special_summon::8>> = bin
    presentation = presentation_from presentation_id
    summon_type = (&(
    case &1 do
      0->
        :normal_summon
      1->
        :special_summon
    end
    )).(is_special_summon)
    {:ok,{:summon,[handcards_index,presentation,summon_type]}}
  end

  def read(12003,bin) do
    <<source_card_index::8>> = bin
    {:ok,{:attack,[source_card_index]}}
  end

  def read(12004,bin) do
    <<card_index::8>> = bin
    {:ok,{:flip_card,[card_index]}}
  end

  def read(12006,_bin) do
    {:ok,{:battle_load_finish,[]}}
  end  

  def read(12007,bin) do
    <<scene_type_id::8,index::8>> = bin
    {:ok,{:get_card_operations,scene_type_from(scene_type_id),index}}
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
    {:ok,{:get_cards_of_scene_type,[player_id,scene_type_from(scene_type_id)]}}
  end
  
  def read(12011,bin) do
    <<scene_type_id::8,index::8>> = bin
    {:ok,{:fire_effect,[scene_type_from(scene_type_id),index]}}
  end
  
  
  def write(:change_phase_to,phase) do    
    phase_number = phase_id_from phase
    Yugioh.Proto.pack(12000,<<phase_number::8>>)
  end

  def write(:new_turn_draw,[turn_count,phase,operator_id,draw_card_id]) do
    phase_number = phase_id_from phase
    data = <<turn_count::8,phase_number::8,operator_id::32,draw_card_id::32>>
    Yugioh.Proto.pack(12002,data)
  end  

  # def write(:summon,[player_id,handcards_index,summon_card_id,summon_card_pos,presentation]) do
  #   presentation = presentation_id_from presentation
  #   Yugioh.Proto.pack(12001,<<player_id::32,handcards_index::8,summon_card_id::32,summon_card_pos::8,presentation::8>>)
  # end  
  

  # def write(:attack,[source_card_index,target_card_index,target_card_id,damage_player_id,hp_damage,destroy_cards,player1_id,graveyard_cards1,player2_id,graveyard_cards2]) do
  #   destroy_cards_list = Enum.map destroy_cards,fn({player_id,destroy_card_index}) ->          
  #     <<player_id::32,destroy_card_index::8>>
  #   end

  #   destroy_cards_binary = iolist_to_binary(destroy_cards_list)    

  #   graveyard_cards1_binary = iolist_to_binary(Enum.map(graveyard_cards1,fn(x)-> <<x::32>> end))
  #   graveyard_cards2_binary = iolist_to_binary(Enum.map(graveyard_cards2,fn(x)-> <<x::32>> end))

  #   data = <<source_card_index::8,target_card_index::8,target_card_id::32,damage_player_id::32,hp_damage::16,length(destroy_cards_list)::16,destroy_cards_binary::binary,
  #   player1_id::32,length(graveyard_cards1)::16,graveyard_cards1_binary::binary,player2_id::32,length(graveyard_cards2)::16,graveyard_cards2_binary::binary>>

  #   Yugioh.Proto.pack(12003,data)
  # end  

  def write(:flip_card,[player_id,card_index,card_id,new_status]) do
    new_status = presentation_id_from new_status
    data = <<player_id::32,card_index::8,card_id::32,new_status::8>>
    Yugioh.Proto.pack(12004,data)
  end
  
  def write(:battle_end,[result,win_player_id,lose_player_id]) do
    result_number = case result do
      :win ->
        1
      :draw ->
        2
    end
    data = <<result_number::8,win_player_id::32,lose_player_id::32>>
    Yugioh.Proto.pack(12005,data)
  end
  
  def write(:get_card_operations,operations) do
    operations_list = Enum.map operations,fn(op) ->
      op_id = operation_type_id_from(op)
      <<op_id::8>>
    end
    operations_binary = iolist_to_binary(operations_list)
    data = <<length(operations_list)::16,operations_binary::binary>>
    Yugioh.Proto.pack(12007,data)
  end
  
  def write(:choose_card,[choose_type,choose_number,choose_scenes_list]) do
    choose_scenes_binary = List.foldl choose_scenes_list,<<>>,fn({player_id,scene_type,id_index_list},bin)->
      id_index_binary = List.foldl id_index_list,<<>>,fn({card_id,index},bin)->
        bin <> <<card_id::32,index::8>>
      end
      bin <> <<player_id::32,scene_type_id_from(scene_type)::8,length(id_index_list)::16,id_index_binary::binary>> 
    end
    data = <<choose_type_id_from(choose_type)::8,choose_number::8,length(choose_scenes_list)::16,choose_scenes_binary::binary>>
    Yugioh.Proto.pack(12008,data)
  end

  def write(:effects,effects) do
    effects_binary = List.foldl effects,<<>>,fn(effect,acc)->
      acc <> <<effect_type_id_from(effect.type)::32>> <> Yugioh.Proto.pack_string(effect.params) <> 
      <<length(effect.targets)::16>> <>
      List.foldl(effect.targets,<<>>,&(&2 <> <<&1.player_id::32,scene_type_id_from(&1.scene_type)::8,&1.index::8>>))
    end
    data = <<length(effects)::16,effects_binary::binary>>
    Yugioh.Proto.pack(12009,data)
  end

  def write(:get_cards_of_scene_type,[player_id,scene_type,graveyardcards]) do
    graveyardcards_binary = List.foldl graveyardcards,<<>>,&(&2 <> <<&1::32>>)
    data = <<player_id::32,scene_type_id_from(scene_type)::8,length(graveyardcards)::16,graveyardcards_binary::binary>>
    Yugioh.Proto.pack(12010,data)
  end
end