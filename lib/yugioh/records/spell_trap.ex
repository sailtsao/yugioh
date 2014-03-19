defrecord SpellTrap,id: 0,card_type: nil,state: nil,count: 0,skills: [] do

  def can_fire_effect? player_id,index,battle_data,spell_trap = SpellTrap[count: count] do
    if count > 0 do
      case spell_trap.get_normal_skills do
        []->
          false
        skills->
          Enum.any?(skills,&(ConditionCore.is_skill_conditions_satisfied(player_id,:spell_trap_zone,index,&1,battle_data,[])))
      end
    else
      false
    end
  end

  def get_normal_skills spell_trap do
    Enum.filter spell_trap.skills,&(&1.type == :normal_skill)
  end

  def get_fire_effect_operations player_id,index,battle_data,spell_trap do
    if spell_trap.can_fire_effect?(player_id,index,battle_data) do
      [:fire_effect_operation]
    else
      []
    end
  end
end