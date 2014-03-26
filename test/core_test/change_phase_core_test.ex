defmodule ChangePhaseCoreTest do
  use ExUnit.Case,async: true
  test "change phase core test" do
    player_battle_info1 = BattlePlayerInfo[id: 6,player_pid: self]
    player_battle_info2 = BattlePlayerInfo[id: 8,player_pid: self]
    battle_data = BattleData[turn_count: 1,turn_player_id: 6,operator_id: 6,phase: :mp1,player1_id: 6,player2_id: 8,
    player1_battle_info: player_battle_info1,player2_battle_info: player_battle_info2]

    {result,battle_data} = ChangePhaseCore.change_phase 6,:bp,battle_data

    assert result == :ok
    assert battle_data.phase == :bp
    receive do
      {:send,message}->
        assert message == <<0, 5, 46, 224, 4>>
    end
    receive do
      {:send,message}->
        assert message == <<0, 5, 46, 224, 4>>
    end
  end

  test "test drop handcards change phase" do
    player_battle_info = BattlePlayerInfo[id: 6,player_pid: self,handcards: [1,2,3,4,5,6,7]]
    player_battle_info2 = BattlePlayerInfo[id: 8,player_pid: self]
    battle_data = BattleData[turn_count: 1,turn_player_id: 6,operator_id: 6,phase: :mp1,player1_id: 6,player2_id: 8,
    player1_battle_info: player_battle_info,player2_battle_info: player_battle_info2]

    {result,battle_data} = ChangePhaseCore.change_phase 6,:ep,battle_data

    assert result == :ok
    assert battle_data.phase != :ep
    assert battle_data.choose_callback != nil
    receive do
      {:send,message}->
        assert message == <<0, 50, 46, 232, 4, 1, 0, 1, 0, 0, 0, 6, 7, 0, 7, 0, 0, 0, 1, 0, 0, 0, 0, 2, 1, 0, 0, 0, 3, 2, 0, 0, 0, 4, 3, 0, 0, 0, 5, 4, 0, 0, 0, 6, 5, 0, 0, 0, 7, 6>>
    end
    {result,battle_data} = battle_data.choose_callback.([{6,:handcard_zone,[0]}],battle_data)
    assert result == :ok
    assert battle_data.phase == :ep
    assert battle_data.player1_battle_info.handcards == [2,3,4,5,6,7]
    assert battle_data.player1_battle_info.graveyardcards == [1]
    assert_receive :new_turn_draw_phase
    receive do
      {:send,message}->
        assert message == <<0, 27, 46, 233, 0, 1, 0, 0, 0, 1, 0, 7, 54, 59, 49, 59, 56, 59, 48, 0, 1, 0, 0, 0, 6, 7, 0>>
    end
    receive do
      {:send,message}->
        assert message == <<0, 27, 46, 233, 0, 1, 0, 0, 0, 1, 0, 7, 54, 59, 49, 59, 56, 59, 48, 0, 1, 0, 0, 0, 6, 7, 0>>
    end
    receive do
      {:send,message}->
        assert message == <<0, 5, 46, 224, 6>>
    end
    receive do
      {:send,message}->
        assert message == <<0, 5, 46, 224, 6>>
    end
  end

  test "new turn" do
    player_battle_info = BattlePlayerInfo[id: 6,player_pid: self,handcards: [1,2,3,4,5,6],deckcards: [1,2,3,4,5]]
    player_battle_info2 = BattlePlayerInfo[id: 8,player_pid: self,handcards: [1,2,3,4,5,6],deckcards: [1,2,3,4,5]]
    battle_data = BattleData[turn_count: 1,turn_player_id: 6,operator_id: 6,phase: :mp1,player1_id: 6,player2_id: 8,
    player1_battle_info: player_battle_info,player2_battle_info: player_battle_info2]
    battle_data = battle_data.new_turn
    assert battle_data.phase == :dp
    assert battle_data.turn_player_id == 8
    assert battle_data.operator_id == 8
    assert battle_data.player2_battle_info.handcards == [1,2,3,4,5,6,1]
    assert battle_data.player2_battle_info.deckcards == [2,3,4,5]
  end
end