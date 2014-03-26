defrecord Monster,id: 0,attack: 0,defense: 0,level: 0,attribute: :none,race: :none,monster_mode: :none,group: :none,category: :none,
presentation: nil,presentation_changed: false,attacked: false,effect_count: 0,skills: [],state: nil do

  def turn_reset(record) do
    record.update(presentation_changed: false,attacked: false,effect_count: 0)
  end

  def get_normal_skills monster do
    Enum.filter monster.skills,&(&1.type == :normal_skill)
  end

  def can_fire_effect? player_id,index,battle_data,monster do
    case monster.get_normal_skills do
      []->
        false
      skills->
        Enum.any?(skills,&(&1.can_fire_effect?(player_id,:monster_zone,index,battle_data)))
        and monster.effect_count == 0
    end
  end

  def get_presentation_operations(Monster[presentation_changed: false,presentation: :attack]) do
    [:change_to_defense_present_operation]
  end

  def get_presentation_operations Monster[presentation_changed: false,presentation: :defense_up] do
    [:change_to_attack_present_operation]
  end

  def get_presentation_operations(Monster[presentation_changed: false,presentation: :defense_down]) do
    [:reverse_operation]
  end

  def get_presentation_operations(_) do
    []
  end

  def get_fire_effect_operations player_id,index,battle_data,monster do
    if monster.can_fire_effect?(player_id,index,battle_data) do
      [:fire_effect_operation]
    else
      []
    end
  end

  def change_presentation monster do
    case monster.presentation do
      :defense_down->
        monster.update(presentation: :attack,presentation_changed: true)
      :defense_up->
        monster.update(presentation: :attack,presentation_changed: true)
      :attack->
        monster.update(presentation: :defense_up,presentation_changed: true)
    end
  end

end