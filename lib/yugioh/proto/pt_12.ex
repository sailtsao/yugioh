defmodule Yugioh.Proto.PT12 do

  defp phase_to_phase_number phase do
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

  defp phase_number_to_phase phase_number do
    case phase_number do
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

  defp decode_summon_type summon_type do
    case summon_type do
      1->
        :attack
      2->
        :defense_down
      3->
        :defense_up
    end
  end

  def encode_summon_type summon_type do
    case summon_type do      
      :attack->
        1
      :defense_down->  
        2
      :defense_up->  
        3
    end
  end

  def decode_scene_type_id scene_type_id do
    case scene_type_id do
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
    end
  end

  def encode_scene_type scene_type do
    case scene_type do
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
    end    
  end
  
  def decode_operation_type_id operation_type_id do
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
    end
  end

  def encode_operation_type operation_type do
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
    end    
  end

  def deocde_effect_type_id effect_type_id do
    case effect_type_id do
      1 ->
        :pay_tribute_effect
      2 ->
        :summon_effect
    end
  end
  
  def encode_effect_type effect_type do
    case effect_type do
      :pay_tribute_effect ->
        1
      :summon_effect ->
        2      
    end
  end
  

  def read(12000,bin) do
    <<phase_number::8>> = bin
    phase = phase_number_to_phase phase_number
    {:ok,{:change_phase_to,phase}}
  end

  def read(12001,bin) do
    <<handcards_index::8,summon_type::8>> = bin
    summon_type = decode_summon_type summon_type
    {:ok,{:summon,handcards_index,summon_type}}
  end

  def read(12003,bin) do
    <<attacker_pos::8,defender_pos::8>> = bin
    {:ok,{:attack,attacker_pos,defender_pos}}
  end

  def read(12004,bin) do
    <<card_index::8>> = bin
    {:ok,{:flip_card,card_index}}
  end

  def read(12006,_bin) do
    {:ok,:battle_load_finish}
  end  

  def read(12007,bin) do
    <<scene_type_id::8,index::8>> = bin
    {:ok,{:get_card_operations,decode_scene_type_id(scene_type_id),index}}
  end
  
  def read(12008,bin) do
    <<len::16,data::binary>> = bin
    fun = fn(_, [bin1,list]) ->
        <<index::8, rest::binary>> = bin1
        list = list ++ [index]
        [rest, list]
    end
    [_, index_list] = List.foldl(List.duplicate(1, len),[bin,[]],fun)
    {:ok,{:choose_card,index_list}}
  end

  def write(:change_phase_to,phase) do    
    phase_number = phase_to_phase_number phase
    Yugioh.Proto.pack(12000,<<phase_number::8>>)
  end

  def write(:new_turn_draw,[turn_count,phase,operator_id,draw_card_id]) do
    phase_number = phase_to_phase_number phase
    data = <<turn_count::8,phase_number::8,operator_id::32,draw_card_id::32>>
    Yugioh.Proto.pack(12002,data)
  end  

  def write(:summon,[player_id,handcards_index,summon_card_id,summon_card_pos,summon_type]) do
    summon_type = encode_summon_type summon_type
    Yugioh.Proto.pack(12001,<<player_id::32,handcards_index::8,summon_card_id::32,summon_card_pos::8,summon_type::8>>)
  end  
  

  def write(:attack,[source_card_index,target_card_index,target_card_id,damage_player_id,hp_damage,destroy_cards,player1_id,graveyard_cards1,player2_id,graveyard_cards2]) do
    destroy_cards_list = Enum.map destroy_cards,fn({player_id,destroy_card_index}) ->          
      <<player_id::32,destroy_card_index::8>>
    end

    destroy_cards_binary = iolist_to_binary(destroy_cards_list)    

    graveyard_cards1_binary = iolist_to_binary(Enum.map(graveyard_cards1,fn(x)-> <<x::32>> end))
    graveyard_cards2_binary = iolist_to_binary(Enum.map(graveyard_cards2,fn(x)-> <<x::32>> end))

    data = <<source_card_index::8,target_card_index::8,target_card_id::32,damage_player_id::32,hp_damage::16,length(destroy_cards_list)::16,destroy_cards_binary::binary,
    player1_id::32,length(graveyard_cards1)::16,graveyard_cards1_binary::binary,player2_id::32,length(graveyard_cards2)::16,graveyard_cards2_binary::binary>>

    Yugioh.Proto.pack(12003,data)
  end  

  def write(:flip_card,[player_id,card_index,card_id,new_status]) do
    new_status = encode_summon_type new_status
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
  
  def write(:get_card_operations,[operations]) do
    operations_list = Enum.map operations,fn(op) ->
      op_id = encode_operation_type(op)
      <<op_id::8>>
    end
    operations_binary = iolist_to_binary(operations_list)
    data = <<length(operations_list)::16,operations_binary::binary>>
    Yugioh.Proto.pack(12007,data)
  end
  
  def write(:choose_card,[scene_target,scene_type,choose_number,index_list]) do
    scene_target_id = (&(
    case &1 do
      :self->
        0
      :other->
        1
    end
    )).(scene_target)
    index_binary = List.foldl index_list,<<>>,&(&2 <> <<&1::8>>)
    data = <<scene_target_id::8,decode_scene_type_id(scene_type)::8,choose_number::8,length(index_list)::16,index_binary::binary>>
    Yugioh.Proto.pack(12008,data)
  end

  def write(:effects,[effects]) do
    effects_binary = List.foldl effects,<<>>,fn(effect,acc)->
      acc <> <<encode_effect_type(effect.type)::32>> <> Yugioh.Proto.pack_string(effect.params) <> 
      <<length(effect.targets)::16>> <>
      List.foldl(effect.targets,<<>>,&(&2 <> <<&1.player_id::32,encode_scene_type(&1.scene_type)::8,&1.index::8>>))
    end
    data = <<length(effects)::16,effects_binary::binary>>
    Yugioh.Proto.pack(12009,data)
  end
end