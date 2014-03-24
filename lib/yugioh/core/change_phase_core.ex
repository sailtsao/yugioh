defmodule ChangePhaseCore do

  def change_phase(player_id,:bp,battle_data = BattleData[turn_player_id: player_id,phase: :mp1]) do
    battle_data = battle_data.phase(:bp)
    message = Proto.PT12.write(:change_phase_to,[:bp])
    battle_data.send_message_to_all message
    {:ok,battle_data}
  end

  def change_phase(player_id,:mp2,battle_data = BattleData[turn_player_id: player_id,phase: old_phase])
  when old_phase in [:mp1,:bp] do
    battle_data = battle_data.phase(:mp2)
    message = Proto.PT12.write(:change_phase_to,[:mp2])
    battle_data.send_message_to_all message
    {:ok,battle_data}
  end

  def change_phase(player_id,:ep,battle_data = BattleData[turn_player_id: player_id,phase: old_phase])
  when old_phase in [:mp1,:bp,:mp2] do
    player_battle_info = battle_data.get_player_battle_info player_id
    message = Proto.PT12.write(:change_phase_to,[:ep])

    if length(player_battle_info.handcards)>6 do
      ChooseCore.drop_handcard_choose player_id,battle_data,fn([{_,:handcard_zone,index_list}],battle_data)->
        battle_data = battle_data.move_cards_to_graveryard player_id,:handcard_zone,index_list
        battle_data = battle_data.phase(:ep)
        battle_data.send_message_to_all message
        send self,:new_turn_draw_phase
        {:ok,battle_data}
      end
    else
      battle_data = battle_data.phase(:ep)
      battle_data.send_message_to_all message
      send self,:new_turn_draw_phase
      {:ok,battle_data}
    end
  end

  def change_phase player_id,_,battle_data do
    {:invalid_phase_change,battle_data}
  end

end