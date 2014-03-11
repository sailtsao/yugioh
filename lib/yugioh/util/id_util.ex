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

end