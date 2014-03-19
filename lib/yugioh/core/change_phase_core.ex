defmodule ChangePhaseCore do

  def change_phase :bp,battle_data = BattleData[phase: :mp1] do
    battle_data = battle_data.phase(:bp)
    message = Proto.PT12.write(:change_phase_to,[:bp])
    battle_data.send_message_to_all message
    {:ok,battle_data}
  end

  def change_phase(:mp2,battle_data = BattleData[phase: old_phase])
  when old_phase in [:mp1,:bp] do
    battle_data = battle_data.phase(:mp2)
    message = Proto.PT12.write(:change_phase_to,[:mp2])
    battle_data.send_message_to_all message
    {:ok,battle_data}
  end

  def change_phase(:ep,battle_data = BattleData[phase: old_phase])
  when old_phase in [:mp1,:bp,:mp2] do
    battle_data = battle_data.phase(:ep)
    message = Proto.PT12.write(:change_phase_to,[:ep])
    battle_data.send_message_to_all message
    send self,:new_turn_draw_phase
    {:ok,battle_data}
  end

  def change_phase _,battle_data do
    {:invalid_phase_change,battle_data}
  end

end