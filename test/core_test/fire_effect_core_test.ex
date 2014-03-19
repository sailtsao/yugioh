defmodule FireEffectCoreTest do
  use ExUnit.Case
  test "fire effect" do
    spell_trap = Data.Cards.get(12).become_spell_trap
    spell_trap = spell_trap.count 1
    player1_battle_info = BattlePlayerInfo[id: 6,hp: 3000,player_pid: self,handcards: [11,12],deckcards: [7]]
    player2_battle_info = BattlePlayerInfo[id: 8,hp: 3000,player_pid: self,spell_trap_zone: HashDict.new([{2,spell_trap}])]
    battle_data = BattleData[turn_count: 1,operator_id: 6,phase: :mp1,player1_id: 6,player2_id: 8,
    player1_battle_info: player1_battle_info,player2_battle_info: player2_battle_info]
    {result,battle_data} = FireEffectCore.fire_effect(6,:handcard_zone,0,battle_data)
    assert result == :ok
    receive do
      {:send,message}->
        assert message == <<0, 26, 46, 233, 0, 1, 0, 0, 0, 2, 0, 6, 48, 59, 49, 49, 59, 49, 0, 1, 0, 0, 0, 6, 2, 2>>
    end
    receive do
      {:send,message}->
        assert message == <<0, 26, 46, 233, 0, 1, 0, 0, 0, 2, 0, 6, 48, 59, 49, 49, 59, 49, 0, 1, 0, 0, 0, 6, 2, 2>>
    end
    receive do
      {:send,message}->
        assert message == <<0, 20, 46, 232, 3, 1, 0, 1, 0, 0, 0, 8, 2, 0, 1, 0, 0, 0, 12, 2>>
    end
    assert battle_data.choose_callback != nil
    {result,battle_data} = battle_data.choose_callback.([[{8,:spell_trap_zone,[2]}]],battle_data)
    assert result == :ok
    assert battle_data.choose_callback == nil
    receive do
      {:send,message}->
        assert message == <<0, 8, 46, 236, 0, 0, 0, 11>>
    end
    {:ok,battle_data} = OperationsCore.get_operations 8,:spell_trap_zone,2,battle_data
    # assert operations == []
    assert battle_data.answer_callback != nil
    # {result,battle_data} = battle_data.answer_callback.(:no,battle_data)
    # assert result == :ok
    # assert Dict.size(battle_data.player1_battle_info.spell_trap_zone) == 0
    # assert Dict.size(battle_data.player2_battle_info.spell_trap_zone) == 0
    # assert battle_data.player1_battle_info.graveyardcards == [11]
    # assert battle_data.player2_battle_info.graveyardcards == [12]

    {result,battle_data} = battle_data.answer_callback.(:yes,battle_data)
    assert result == :ok
    assert battle_data.answer_callback == nil
    {:ok,battle_data} = OperationsCore.get_operations 8,:spell_trap_zone,2,battle_data
    # assert operations == [:fire_effect_operation]
    {result,battle_data} = FireEffectCore.fire_effect(8,:spell_trap_zone,2,battle_data)
    assert result == :ok
    assert battle_data.player1_battle_info.handcards == [12,7]
    assert battle_data.player1_battle_info.deckcards == []
    assert battle_data.player1_battle_info.graveyardcards == [11]
    assert battle_data.player2_battle_info.graveyardcards == [12]
    # IO.inspect battle_data
  end
end