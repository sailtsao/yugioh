defrecord Skill,type: 0,check_phase: [],skill_effects: [],and_conditions: [],or_conditions: [] do
  def is_conditions_satisfied? player_id,scene_type,index,battle_data,Skill[and_conditions: and_conditions,or_conditions: or_conditions] do
    and_result = List.foldl and_conditions,true,&(&2 && ConditionCore.is_condition_satisfied(player_id,scene_type,index,&1,battle_data))
    or_result = List.foldl or_conditions,true,&(&2 || ConditionCore.is_condition_satisfied(player_id,scene_type,index,&1,battle_data))
    and_result && or_result
  end

  def can_fire_effect? player_id,scene_type,index,battle_data,skill = Skill[check_phase: []] do
    skill.is_conditions_satisfied? player_id,scene_type,index,battle_data
  end

  def can_fire_effect? player_id,scene_type,index,battle_data = BattleData[check_phase: check_phase],skill do
    (check_phase in skill.check_phase) and skill.is_conditions_satisfied?(player_id,scene_type,index,battle_data)
  end

  def can_be_chained? player_id,scene_type,index,check_phase,battle_data,skill = Skill[check_phase: []] do
    skill.is_conditions_satisfied? player_id,scene_type,index,battle_data
  end

  def can_be_chained? player_id,scene_type,index,check_phase,battle_data,skill do
    skill.is_conditions_satisfied?(player_id,scene_type,index,battle_data) && (check_phase in skill.check_phase)
  end
end