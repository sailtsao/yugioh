defmodule Yugioh.Data.Cards do

  def get(1) do
    Card.new(
    id: 1,
    card_type: :monster_card,
    race_type: :demon_race,
    atrribute: :dark_attribute,
    group: 0,
    max_count_limit: 3,
    attack: 2400,
    defense: 0,
    level: 5
    )
  end


  def get(2) do
    Card.new(
    id: 2,
    card_type: :monster_card,
    race_type: :demon_race,
    atrribute: :dark_attribute,
    group: 0,
    max_count_limit: 1,
    attack: 1000,
    defense: 600,
    level: 3
    )
  end


  def get(3) do
    Card.new(
    id: 3,
    card_type: :monster_card,
    race_type: :warrior_race,
    atrribute: :dark_attribute,
    group: 0,
    max_count_limit: 3,
    attack: 600,
    defense: 1300,
    level: 3
    )
  end


  def get(4) do
    Card.new(
    id: 4,
    card_type: :monster_card,
    race_type: :warrior_race,
    atrribute: :dark_attribute,
    group: 0,
    max_count_limit: 3,
    attack: 1400,
    defense: 1200,
    level: 4
    )
  end


  def get(5) do
    Card.new(
    id: 5,
    card_type: :monster_card,
    race_type: :demon_race,
    atrribute: :dark_attribute,
    group: 0,
    max_count_limit: 1,
    attack: 200,
    defense: 500,
    level: 4
    )
  end


  def get(6) do
    Card.new(
    id: 6,
    card_type: :monster_card,
    race_type: :plant_race,
    atrribute: :dark_attribute,
    group: 0,
    max_count_limit: 3,
    attack: 1400,
    defense: 1100,
    level: 4
    )
  end


  def get(7) do
    Card.new(
    id: 7,
    card_type: :monster_card,
    race_type: :warrior_race,
    atrribute: :dark_attribute,
    group: 0,
    max_count_limit: 3,
    attack: 1700,
    defense: 1600,
    level: 4
    )
  end


  def get(8) do
    Card.new(
    id: 8,
    card_type: :monster_card,
    race_type: :demon_race,
    atrribute: :dark_attribute,
    group: 0,
    max_count_limit: 3,
    attack: 2400,
    defense: 1000,
    level: 6
    )
  end


  def get(9) do
    Card.new(
    id: 9,
    card_type: :monster_card,
    race_type: :machine_race,
    atrribute: :dark_attribute,
    group: 0,
    max_count_limit: 3,
    attack: 2400,
    defense: 1500,
    level: 6
    )
  end


  def get(10) do
    Card.new(
    id: 10,
    card_type: :monster_card,
    race_type: :bird_race,
    atrribute: :dark_attribute,
    group: 0,
    max_count_limit: 3,
    attack: 2400,
    defense: 1600,
    level: 8
    )
  end

end
