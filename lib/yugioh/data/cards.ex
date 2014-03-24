defmodule Data.Cards do

  def get(1) do
  Card.new(
  id: 1,
  card_type: :monster_card,
  race_type: :zombie_race,
  attribute: :dark_attribute,
  category: :none,
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
  race_type: :zombie_race,
  attribute: :dark_attribute,
  category: :none,
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
  attribute: :dark_attribute,
  category: :none,
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
  attribute: :dark_attribute,
  category: :none,
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
  race_type: :zombie_race,
  attribute: :dark_attribute,
  category: :none,
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
  race_type: :beast_warrior_race,
  attribute: :dark_attribute,
  category: :none,
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
  attribute: :dark_attribute,
  category: :none,
  group: 0,
  max_count_limit: 3,
  attack: 1700,
  defense: 1600,
  level: 4,
  skills: [
      Skill.new(type: :special_summon_skill,
      check_phase: [],
      skill_effects: [
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
      check_phase: [],
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
  race_type: :zombie_race,
  attribute: :dark_attribute,
  category: :none,
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
  race_type: :dragon_race,
  attribute: :dark_attribute,
  category: :none,
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
  race_type: :spellcaster_race,
  attribute: :dark_attribute,
  category: :none,
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
  card_type: :spell_card,
  race_type: :none,
  attribute: :none,
  category: :quickplay_spell,
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
  attribute: :none,
  category: :counter_trap,
  group: 0,
  max_count_limit: 1,
  attack: 0,
  defense: 0,
  level: 0,
  skills: [
      Skill.new(type: :normal_skill,
      check_phase: [:opponent_fire_effect_phase],
      skill_effects: [
          SkillEffect.new(id: 3,
          params: "",
          priority: 99
          ),
          SkillEffect.new(id: 4,
          params: "",
          priority: 1
          )
      ],
      and_conditions: [
      ],
      or_conditions: [
      ]
      )
  ]
  )
  end


  def get(13) do
  Card.new(
  id: 13,
  card_type: :monster_card,
  race_type: :dragon_race,
  attribute: :light_attribute,
  category: :none,
  group: 0,
  max_count_limit: 3,
  attack: 3000,
  defense: 2600,
  level: 8,
  skills: [
  ]
  )
  end


  def get(14) do
  Card.new(
  id: 14,
  card_type: :monster_card,
  race_type: :spellcaster_race,
  attribute: :dark_attribute,
  category: :none,
  group: 0,
  max_count_limit: 2,
  attack: 2300,
  defense: 2000,
  level: 6,
  skills: [
  ]
  )
  end


  def get(15) do
  Card.new(
  id: 15,
  card_type: :monster_card,
  race_type: :warrior_race,
  attribute: :light_attribute,
  category: :none,
  group: 0,
  max_count_limit: 1,
  attack: 3000,
  defense: 2500,
  level: 8,
  skills: [
  ]
  )
  end


  def get(16) do
  Card.new(
  id: 16,
  card_type: :monster_card,
  race_type: :fiend_race,
  attribute: :dark_attribute,
  category: :none,
  group: 0,
  max_count_limit: 1,
  attack: 2700,
  defense: 2500,
  level: 8,
  skills: [
  ]
  )
  end


  def get(17) do
  Card.new(
  id: 17,
  card_type: :monster_card,
  race_type: :spellcaster_race,
  attribute: :light_attribute,
  category: :none,
  group: 0,
  max_count_limit: 3,
  attack: 1000,
  defense: 1000,
  level: 3,
  skills: [
  ]
  )
  end

end
