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
  level: 5,
  skills: [
  ]
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
  level: 3,
  skills: [
  ]
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
  level: 3,
  skills: [
  ]
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
  level: 4,
  skills: [
  ]
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
  level: 4,
  skills: [
  ]
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
  level: 4,
  skills: [
  ]
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
  level: 4,
  skills: [
      Skill.new(type: :special_summon_skill,
      check_phase: [:main_phase_1,:main_phase_2],
      skill_effects: [
          SkillEffect.new(id: 2,
          params: "",
          priority: 2
          ),
          SkillEffect.new(id: 1,
          params: "1;1;7;5;1;1;3",
          priority: 1
          )
      ],
      and_conditions: [
          Condition.new(id: 1,
          params: "1;7;5;1;1;1;0"
          )
      ],
      or_conditions: [
      ]
      ),
      Skill.new(type: :normal_skill,
      check_phase: [:main_phase_1,:main_phase_2],
      skill_effects: [
          SkillEffect.new(id: 1,
          params: "1;1;7;0;1;1;3",
          priority: 1
          ),
          SkillEffect.new(id: 1,
          params: "1;1;4;0;1;1;3",
          priority: 2
          )
      ],
      and_conditions: [
          Condition.new(id: 1,
          params: "1;7;0;1;1;1;0"
          ),
          Condition.new(id: 1,
          params: "1;4;0;1;1;1;0"
          )
      ],
      or_conditions: [
      ]
      )
  ]
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
  level: 6,
  skills: [
  ]
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
  level: 6,
  skills: [
  ]
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
  level: 8,
  skills: [
  ]
  )
  end


  def get(11) do
  Card.new(
  id: 11,
  card_type: :magic_card,
  race_type: :none,
  atrribute: :none,
  group: 0,
  max_count_limit: 1,
  attack: 0,
  defense: 0,
  level: 0,
  skills: [
      Skill.new(type: :normal_skill,
      check_phase: [],
      skill_effects: [
          SkillEffect.new(id: 1,
          params: "1;0;2;0;0;0;3",
          priority: 0
          )
      ],
      and_conditions: [
          Condition.new(id: 1,
          params: "0;2;0;0;0;1;0"
          )
      ],
      or_conditions: [
      ]
      )
  ]
  )
  end


  def get(12) do
  Card.new(
  id: 12,
  card_type: :trap_card,
  race_type: :none,
  atrribute: :none,
  group: 0,
  max_count_limit: 1,
  attack: 0,
  defense: 0,
  level: 0,
  skills: [
  ]
  )
  end

end
