defmodule ChainCore do
  require Lager

  def skill_chain_available? player_id,:spell_trap_zone,index,check_phase,battle_data do
    player_battle_info = battle_data.get_player_battle_info player_id
    fire_spell_trap = Dict.get player_battle_info.spell_trap_zone,index
    opponent_player_id = battle_data.get_opponent_player_id player_id
    opponent_player_battle_info = battle_data.get_opponent_player_battle_info player_id
    count = Enum.count opponent_player_battle_info.spell_trap_zone,fn({index,spell_trap})->
      Enum.any?(spell_trap.get_normal_skills,&(&1.can_be_chained?(opponent_player_id,:spell_trap_zone,index,check_phase,battle_data)))
      and (spell_trap.state != :chained)
      and (spell_trap.speed>=fire_spell_trap.speed)
    end
    count > 0
  end

  def skill_chain_available? player_id,:monster_zone,index,check_phase,battle_data do
    # player_battle_info = battle_data.get_player_battle_info player_id
    opponent_player_id = battle_data.get_opponent_player_id player_id
    opponent_player_battle_info = battle_data.get_opponent_player_battle_info player_id
    count = Enum.count opponent_player_battle_info.spell_trap_zone,fn({_index,spell_trap})->
      Enum.any?(spell_trap.get_normal_skills,&(&1.can_be_chained?(opponent_player_id,:spell_trap_zone,index,check_phase,battle_data)))
      and (spell_trap.state != :chained)
      and (spell_trap.speed>=1)
    end
    count > 0
  end

  def execute_chain_queue battle_data = BattleData[chain_queue: []] do
    battle_data = battle_data.operator_id battle_data.turn_player_id
    battle_data = battle_data.check_phase :none
    {:ok,battle_data}
  end

  def execute_chain_queue battle_data = BattleData[chain_queue: chain_queue] do
    chain = hd chain_queue
    {player_id,scene_type,index,choose_result_list,skill} = chain
    battle_data = battle_data.chain_queue(tl(chain_queue))
    EffectCore.execute_skill_effects player_id,scene_type,index,skill,choose_result_list,battle_data,fn(scene_type,index,battle_data)->
      Lager.debug "scene_type ~p index ~p battle_data ~p",[scene_type,index,battle_data]
      case scene_type do
        :spell_trap_zone->
          player_battle_info = battle_data.get_player_battle_info player_id
          if Dict.get(player_battle_info.spell_trap_zone,index) != nil do
            battle_data = battle_data.move_cards_to_graveryard player_id,:spell_trap_zone,[index]
          end
        :monster_zone->
          player_battle_info = battle_data.get_player_battle_info player_id
          monster = Dict.get(player_battle_info.monster_zone,index)
          if monster != nil do
            monster = monster.effect_count(monster.effect_count + 1)
            monster_zone = Dict.put player_battle_info.monster_zone,index,monster
            player_battle_info = player_battle_info.monster_zone monster_zone
            player_atom = battle_data.get_player_atom player_id
            battle_data = battle_data.update [{player_atom,player_battle_info}]
          end
      end
      execute_chain_queue battle_data
    end
  end

end