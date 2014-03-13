defmodule IDUtil do

  def presentation_from presentation_id do
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

  def summon_type_from summon_type_id do
    case summon_type_id do
      0->
        :normal_summon
      1->
        :special_summon
    end
  end    

  def card_type_from card_type_id do
    case card_type_id do
      0->
        :none
      1->
        :monster_card
      2->
        :magic_card
      3->
        :trap_card
    end
  end

  def attribute_from attribute_id do
    case attribute_id do
      0 ->
        :none
      1 ->
        :dark_attribute
    end
  end

  def scene_belong_from scene_belong_id do
    case scene_belong_id do
      0 ->
        :both
      1 ->
        :self
      2 ->
        :opponent
    end
  end

  def compare_from compare_id do
    case compare_id do
      0 ->
        :equal
      1 ->
        :greater
      2 ->
        :less
    end
  end
  

  def room_status_id_from room_status do
    case room_status do
      :wait ->
        1
      :battle ->
        2
    end
  end

  def ready_state_id_from ready_state do
    case ready_state do
      :ready->
        1
      :unready->
        0
    end
  end

  def phase_id_from phase do
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

  def phase_from phase_id do
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

end