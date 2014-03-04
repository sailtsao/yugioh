defmodule Util do

  def get_special_summon_skill [] do
    nil
  end

  def get_special_summon_skill skills do
    [skill] = Enum.filter skills,&(&1.type == :special_summon_skill)
    skill
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
  

  def get_cards_from_scene _player_battle_info,_ do
    []
  end
end