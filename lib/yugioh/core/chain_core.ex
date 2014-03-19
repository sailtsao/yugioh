defmodule ChainCore do
  def skill_chain_available? player_id,card_id,battle_data do
    player_battle_info = battle_data.get_player_battle_info player_id
    opponent_player_battle_info = battle_data.get_opponent_player_battle_info player_id
    count = Enum.count opponent_player_battle_info.spell_trap_zone,fn({index,spell_trap})->
      spell_trap.state != :chained
    end
    count > 0
  end

end