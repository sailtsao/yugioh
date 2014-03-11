defmodule Util do

  def get_special_summon_skill [] do
    nil
  end

  def get_special_summon_skill skills do
    case Enum.filter(skills,&(&1.type == :special_summon_skill)) do
      [skill]->
        skill
      []->
        nil
    end
  end

  def get_normal_skills [] do
    []
  end

  def get_normal_skills skills do
    Enum.filter skills,&(&1.type == :normal_skill)
  end

  def get_cards_from_scene player_battle_info,:handcard_zone do
    player_battle_info.handcards
  end

  def get_cards_from_scene player_battle_info,:graveyard_zone do
    player_battle_info.graveyardcards
  end

  def get_cards_from_scene player_battle_info,:deck_zone do
    player_battle_info.deckcards
  end

  def get_cards_from_scene player_battle_info,:extra_deck_zone do
    player_battle_info.extradeckcards
  end
  
  def get_cards_from_scene player_battle_info,:banished_zone do
    player_battle_info.banishedcards
  end

  def get_cards_from_scene player_battle_info,:monster_card_zone do
    Enum.map player_battle_info.monster_card_zone,fn({_,monster})-> monster.id end
  end

  def get_cards_from_scene player_battle_info,:spell_trap_zone do
    Enum.map player_battle_info.spell_trap_zone,fn({_,card_id})-> card_id end
  end

  def get_cards_from_scene _,_ do
    []
  end

  def filter_id_index_list id_index_list,attribute,level_limit do
    Enum.filter id_index_list,fn({card_id,_})->
      card_data = Yugioh.Data.Cards.get(card_id)
      if attribute == :none do
        card_data.level >= level_limit
      else
        (card_data.level >= level_limit)&&(card_data.atrribute == attribute)
      end
    end
  end

  def get_id_index_list_from_scene player_battle_info,:handcard_zone,attribute,level_limit do
    id_index_list = Enum.with_index player_battle_info.handcards
    filter_id_index_list id_index_list,attribute,level_limit
  end

  def get_id_index_list_from_scene player_battle_info,:graveyard_zone,attribute,level_limit do
    id_index_list = Enum.with_index player_battle_info.graveyardcards
    filter_id_index_list id_index_list,attribute,level_limit
  end

  def get_id_index_list_from_scene player_battle_info,:deck_zone,attribute,level_limit do
    id_index_list = Enum.with_index player_battle_info.deckcards
    filter_id_index_list id_index_list,attribute,level_limit
  end

  def get_id_index_list_from_scene player_battle_info,:extra_deck_zone,attribute,level_limit do
    id_index_list = Enum.with_index player_battle_info.extradeckcards
    filter_id_index_list id_index_list,attribute,level_limit   
  end

  def get_id_index_list_from_scene player_battle_info,:banished_zone,attribute,level_limit do
    id_index_list = Enum.with_index player_battle_info.banishedcards
    filter_id_index_list id_index_list,attribute,level_limit   
  end

  def get_id_index_list_from_scene player_battle_info,:monster_card_zone,attribute,level_limit do
    id_index_list = Enum.map player_battle_info.monster_card_zone,fn({_,monster})-> monster.id end
    filter_id_index_list id_index_list,attribute,level_limit   
  end

  def get_id_index_list_from_scene player_battle_info,:spell_trap_zone,attribute,level_limit do
    id_index_list = Enum.map player_battle_info.spell_trap_zone,fn({_,card_id})-> card_id end
    filter_id_index_list id_index_list,attribute,level_limit   
  end  

  def get_id_index_list_from_scene _,_,_,_ do
    []
  end
end