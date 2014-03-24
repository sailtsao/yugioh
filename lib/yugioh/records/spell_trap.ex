defrecord SpellTrap,id: 0,card_type: nil,category: :none,state: nil,count: 0,skills: [] do

  def can_fire_effect? player_id,index,battle_data,spell_trap = SpellTrap[count: count] do
    if count > 0 do
      case spell_trap.get_normal_skills do
        []->
          false
        skills->
          Enum.any?(skills,&(&1.is_conditions_satisfied?(player_id,:spell_trap_zone,index,battle_data)))
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

  def speed spell_trap do
    case spell_trap.category do
      x when x in [:normal_spell,:equip_spell,:field_spell,:ritual_spell,:continuous_spell]->
        1
      x when x in [:quickplay_spell,:normal_trap,:continuous_trap]->
        2
      :counter_trap->
        3
    end
  end

end