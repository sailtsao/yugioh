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

  defp encode_summon_type summon_type do
    case summon_type do      
      :attack->
        1
      :defense_down->  
        2
      :defense_up->  
        3
    end
  end

  def read(12000,bin) do
    <<phase_number::size(8)>> = bin
    phase = phase_number_to_phase phase_number
    {:ok,{:change_phase_to,phase}}
  end

  def read(12001,bin) do
    <<handcards_index::size(8),summon_type::size(8)>> = bin
    summon_type = decode_summon_type summon_type
    {:ok,{:summon,handcards_index,summon_type}}
  end

  def read(12003,bin) do
    <<attacker_pos::size(8),defender_pos::size(8)>> = bin
    {:ok,{:attack,attacker_pos,defender_pos}}
  end

  def read(12004,bin) do
    <<card_index::size(8)>> = bin
    {:ok,{:flip_card,card_index}}
  end

  def read(12006,bin//<<>>) do
    {:ok,:battle_load_finish}
  end
  
  def read(12007,bin) do
    <<player_id::size(32)>> = bin
    {:ok,{:get_graveyard,player_id}}
  end
  

  def write(:change_phase_to,phase) do    
    phase_number = phase_to_phase_number phase
    Yugioh.Proto.pack(12000,<<phase_number::size(8)>>)
  end

  def write(:new_turn_draw,[turn_count,phase,operator_id,draw_card_id]) do
    phase_number = phase_to_phase_number phase
    data = <<turn_count::size(8),phase_number::size(8),operator_id::size(32),draw_card_id::size(32)>>
    Yugioh.Proto.pack(12002,data)
  end  

  def write(:summon,[player_id,handcards_index,summon_card_id,summon_card_pos,summon_type]) do
    summon_type = encode_summon_type summon_type
    Yugioh.Proto.pack(12001,<<player_id::size(32),handcards_index::size(8),summon_card_id::size(32),summon_card_pos::size(8),summon_type::size(8)>>)
  end  
  

  def write(:attack,[source_card_index,target_card_index,target_card_id,damage_player_id,hp_damage,destroy_cards]) do
    destroy_cards_list = Enum.map destroy_cards,fn({player_id,destroy_card_index}) ->          
      <<player_id::size(32),destroy_card_index::size(8)>>
    end

    destroy_cards_binary = iolist_to_binary(destroy_cards_list)    
    data = <<source_card_index::size(8),target_card_index::size(8),target_card_id::size(32),damage_player_id::size(32),hp_damage::size(16),length(destroy_cards_list)::size(16),destroy_cards_binary::binary>>
    Yugioh.Proto.pack(12003,data)
  end  

  def write(:flip_card,[player_id,card_index,card_id,new_status]) do
    new_status = encode_summon_type new_status
    data = <<player_id::size(32),card_index::size(8),card_id::size(32),new_status::size(8)>>
    Yugioh.Proto.pack(12004,data)
  end
  
  def write(:battle_end,[result,win_player_id,lose_player_id]) do
    result_number = case result do
      :win ->
        1
      :draw ->
        2
    end
    data = <<result_number::size(8),win_player_id::size(32),lose_player_id::size(32)>>
    Yugioh.Proto.pack(12005,data)
  end

  def write(:get_graveyard,graveyard_cards) do
    graveyard_cards_binary = iolist_to_binary(Enum.map(graveyard_cards,fn(x)-> <<x::size(32)>> end))
    <<length(graveyard_cards)::size(16),graveyard_cards_binary::binary>>
  end
  
end