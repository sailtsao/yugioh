defrecord Monster,id: 0,attack: 0,defense: 0,level: 0,presentation: nil,effect_monster: false,presentation_changed: false,
attacked: false,effect_fired: false,skills: [] do

  def turn_reset(record) do
    record.update(effect_monster: false,presentation_changed: false,attacked: false,effect_fired: false)
  end

  def can_fire_effect? battle_data,monster do 
    case get_normal_skills(monster) do
      []->
        false
      skills->
        Enum.any?(skills,&(ConditionCore.is_skill_conditions_satisfied &1,battle_data))
    end
  end

  def get_normal_skills monster do
    Enum.filter monster.skills,&(&1.type == :normal_skill)
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

  def get_fire_effect_operations battle_data,monster do
    if monster.can_fire_effect?(battle_data) do
      [:fire_effect_operation]
    else
      []
    end
  end

  def get_operations battle_data,monster do
    presentation_operations = monster.get_presentation_operations

    fire_effect_operations = monster.get_fire_effect_operations battle_data

    presentation_operations++fire_effect_operations
  end
  
  
end