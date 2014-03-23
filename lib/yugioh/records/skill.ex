defrecord Skill,type: 0,check_phase: 0,skill_effects: [],and_conditions: [],or_conditions: [] do
  def is_conditions_satisfied? player_id,scene_type,index,battle_data,skill do
    and_result = List.foldl skill.and_conditions,true,&(&2 && ConditionCore.is_condition_satisfied(player_id,scene_type,index,&1,battle_data))
    or_result = List.foldl skill.or_conditions,true,&(&2 || ConditionCore.is_condition_satisfied(player_id,scene_type,index,&1,battle_data))
    and_result && or_result
  end

end