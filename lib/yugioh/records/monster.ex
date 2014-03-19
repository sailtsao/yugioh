defrecord Monster,id: 0,attack: 0,defense: 0,level: 0,presentation: nil,effect_monster: false,presentation_changed: false,
attacked: false,effect_fired: false,skills: [] do

  def turn_reset(record) do
    record.update(effect_monster: false,presentation_changed: false,attacked: false,effect_fired: false)
  end

  def get_normal_skills monster do
    Enum.filter monster.skills,&(&1.type == :normal_skill)
  end

  def can_fire_effect? player_id,index,battle_data,monster do
    case monster.get_normal_skills do
      []->
        false
      skills->
        Enum.any?(skills,&(ConditionCore.is_skill_conditions_satisfied(player_id,:monster_zone,index,&1,battle_data,[])))
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

end