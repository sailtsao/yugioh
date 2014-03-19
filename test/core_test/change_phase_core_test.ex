defmodule ChangePhaseCoreTest do
  use ExUnit.Case
  test "change phase core test" do
    player_battle_info = BattlePlayerInfo[player_pid: self]
    battle_data = BattleData[turn_count: 1,operator_id: 6,phase: :mp1,player1_id: 6,player2_id: 8,
    player1_battle_info: player_battle_info,player2_battle_info: player_battle_info]

    {result,battle_data} = ChangePhaseCore.change_phase :bp,battle_data

    assert result == :ok
    assert battle_data == BattleData[turn_count: 1,operator_id: 6,phase: :bp,player1_id: 6,player2_id: 8,
    player1_battle_info: player_battle_info,player2_battle_info: player_battle_info]
  end
end