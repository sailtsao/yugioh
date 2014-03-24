defmodule ChainCore do
  require Lager

  def skill_chain_available? player_id,_card_id,battle_data do
    # player_battle_info = battle_data.get_player_battle_info player_id
    opponent_player_battle_info = battle_data.get_opponent_player_battle_info player_id
    count = Enum.count opponent_player_battle_info.spell_trap_zone,fn({_index,spell_trap})->
      spell_trap.state != :chained
    end
    count > 0
  end

  def execute_chain_queue battle_data = BattleData[chain_queue: []] do
    battle_data = battle_data.operator_id battle_data.turn_player_id
    {:ok,battle_data}
  end

  def execute_chain_queue battle_data = BattleData[chain_queue: chain_queue] do
    chain = hd chain_queue
    {player_id,scene_type,index,choose_result_list,skill} = chain
    battle_data = battle_data.chain_queue(tl(chain_queue))
    EffectCore.execute_skill_effects player_id,skill,choose_result_list,battle_data,fn(battle_data)->
      if scene_type == :spell_trap_zone do
        player_battle_info = battle_data.get_player_battle_info player_id
        if Dict.get(player_battle_info.spell_trap_zone,index) != nil do
          battle_data = battle_data.move_cards_to_graveryard player_id,:spell_trap_zone,[index]
        end
      end
      execute_chain_queue battle_data
    end
  end

end