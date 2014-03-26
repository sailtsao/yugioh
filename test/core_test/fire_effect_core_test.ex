defmodule FireEffectCoreTest do
  use ExUnit.Case,async: true

  test "mosnter effect test 1" do
    monster = Data.Cards.get(7).become_monster
    monster = monster.effect_count 1
    player1_battle_info = BattlePlayerInfo[id: 6,hp: 3000,player_pid: self,deckcards: [6,7,8,9],handcards: [6,7,8,9],monster_zone: [{2,monster}]]
    player2_battle_info = BattlePlayerInfo[id: 8,hp: 3000,player_pid: self]
    battle_data = BattleData[turn_count: 2,turn_player_id: 6,operator_id: 6,phase: :mp1,player1_id: 6,player2_id: 8,
    player1_battle_info: player1_battle_info,player2_battle_info: player2_battle_info]
    {:ok,_battle_data} = OperationsCore.get_operations 6,:monster_zone,2,battle_data
    receive do
      {:send,message}->
        assert message == <<0, 6, 46, 231, 0, 0>>
    end
  end

  test "mosnter effect test 2" do
    monster = Data.Cards.get(7).become_monster
    player1_battle_info = BattlePlayerInfo[id: 6,hp: 3000,player_pid: self,deckcards: [6,7,8,9],handcards: [6,7,8,9],monster_zone: [{2,monster}]]
    player2_battle_info = BattlePlayerInfo[id: 8,hp: 3000,player_pid: self]
    battle_data = BattleData[turn_count: 2,turn_player_id: 6,operator_id: 6,phase: :mp1,player1_id: 6,player2_id: 8,
    player1_battle_info: player1_battle_info,player2_battle_info: player2_battle_info]
    {:ok,battle_data} = OperationsCore.get_operations 6,:monster_zone,2,battle_data
    receive do
      {:send,message}->
        assert message == <<0, 7, 46, 231, 0, 1, 3>>
    end
    {result,battle_data} = FireEffectCore.fire_effect(6,:monster_zone,2,battle_data)
    assert result == :ok
    receive do
      {:send,message}->
        assert message == <<0, 35, 46, 232, 3, 1, 0, 1, 0, 0, 0, 6, 7, 0, 4, 0, 0, 0, 6, 0, 0, 0, 0, 7, 1, 0, 0, 0, 8, 2, 0, 0, 0, 9, 3>>
    end
    assert battle_data.choose_callback != nil
    {result,battle_data} = battle_data.choose_callback.([{6,:handcard_zone,[0]}],battle_data)
    assert result == :ok
    assert battle_data.choose_callback != nil
    receive do
      {:send,message}->
        assert message == <<0, 35, 46, 232, 3, 1, 0, 1, 0, 0, 0, 6, 4, 0, 4, 0, 0, 0, 6, 0, 0, 0, 0, 7, 1, 0, 0, 0, 8, 2, 0, 0, 0, 9, 3>>
    end
    {result,battle_data} = battle_data.choose_callback.([{6,:deck_zone,[0]}],battle_data)
    assert result == :ok
    assert battle_data.choose_callback == nil
    assert battle_data.player1_battle_info.monster_zone[2].effect_count == 1
    assert battle_data.player1_battle_info.graveyardcards == [6,6]
    assert battle_data.player1_battle_info.handcards == [7,8,9]
    assert battle_data.player1_battle_info.deckcards == [7,8,9]
    receive do
      {:send,message}->
        assert message == <<0, 27, 46, 233, 0, 1, 0, 0, 0, 1, 0, 7, 54, 59, 54, 59, 56, 59, 48, 0, 1, 0, 0, 0, 6, 7, 0>>
    end
    receive do
      {:send,message}->
        assert message == <<0, 27, 46, 233, 0, 1, 0, 0, 0, 1, 0, 7, 54, 59, 54, 59, 56, 59, 48, 0, 1, 0, 0, 0, 6, 7, 0>>
    end
    receive do
      {:send,message}->
        assert message == <<0, 27, 46, 233, 0, 1, 0, 0, 0, 1, 0, 7, 54, 59, 54, 59, 56, 59, 48, 0, 1, 0, 0, 0, 6, 4, 0>>
    end
    receive do
      {:send,message}->
        assert message == <<0, 27, 46, 233, 0, 1, 0, 0, 0, 1, 0, 7, 54, 59, 54, 59, 56, 59, 48, 0, 1, 0, 0, 0, 6, 4, 0>>
    end
    {:ok,_battle_data} = OperationsCore.get_operations 6,:monster_zone,2,battle_data
    receive do
      {:send,message}->
        assert message == <<0, 6, 46, 231, 0, 0>>
    end
  end

  test "fire effect to self" do
    spell_trap = Data.Cards.get(11).become_spell_trap
    spell_trap = spell_trap.count 1
    player1_battle_info = BattlePlayerInfo[id: 6,hp: 3000,player_pid: self,handcards: [11],spell_trap_zone: [{1,spell_trap},{2,spell_trap}]]
    player2_battle_info = BattlePlayerInfo[id: 8,hp: 3000,player_pid: self]
    battle_data = BattleData[turn_count: 2,turn_player_id: 6,operator_id: 6,phase: :mp1,player1_id: 6,player2_id: 8,
    player1_battle_info: player1_battle_info,player2_battle_info: player2_battle_info]
    {result,battle_data} = FireEffectCore.fire_effect(6,:spell_trap_zone,2,battle_data)
    assert result == :ok
    assert_receive {:send,<<0, 24, 46, 233, 0, 1, 0, 0, 0, 4, 0, 4, 49, 49, 59, 49, 0, 1, 0, 0, 0, 6, 2, 2>>}
    assert_receive {:send,<<0, 24, 46, 233, 0, 1, 0, 0, 0, 4, 0, 4, 49, 49, 59, 49, 0, 1, 0, 0, 0, 6, 2, 2>>}
    assert_receive {:send,<<0, 20, 46, 232, 3, 1, 0, 1, 0, 0, 0, 6, 2, 0, 1, 0, 0, 0, 11, 1>>}
    assert battle_data.choose_callback != nil
    {result,battle_data} = battle_data.choose_callback.([{6,:spell_trap_zone,[1]}],battle_data)
    assert result == :ok
    assert battle_data.choose_callback == nil
    assert_receive {:send,<<0, 28, 46, 233, 0, 1, 0, 0, 0, 1, 0, 8, 54, 59, 49, 49, 59, 56, 59, 48, 0, 1, 0, 0, 0, 6, 2, 1>>}
    assert_receive {:send,<<0, 28, 46, 233, 0, 1, 0, 0, 0, 1, 0, 8, 54, 59, 49, 49, 59, 56, 59, 48, 0, 1, 0, 0, 0, 6, 2, 1>>}
    # assert_receive {:send,<<0, 28, 46, 233, 0, 1, 0, 0, 0, 1, 0, 8, 54, 59, 49, 49, 59, 56, 59, 48, 0, 1, 0, 0, 0, 6, 2, 2>>}
    # assert_receive {:send,<<0, 28, 46, 233, 0, 1, 0, 0, 0, 1, 0, 8, 54, 59, 49, 49, 59, 56, 59, 48, 0, 1, 0, 0, 0, 6, 2, 2>>}
    receive do
      {:send,message}->
        assert message == <<0, 28, 46, 233, 0, 1, 0, 0, 0, 1, 0, 8, 54, 59, 49, 49, 59, 56, 59, 48, 0, 1, 0, 0, 0, 6, 2, 2>>
    end
    receive do
      {:send,message}->
        assert message == <<0, 28, 46, 233, 0, 1, 0, 0, 0, 1, 0, 8, 54, 59, 49, 49, 59, 56, 59, 48, 0, 1, 0, 0, 0, 6, 2, 2>>
    end
  end

  test "fire effect test" do
    spell_trap = Data.Cards.get(11).become_spell_trap
    spell_trap = spell_trap.count 1
    player1_battle_info = BattlePlayerInfo[id: 6,hp: 3000,player_pid: self,handcards: [11]]
    player2_battle_info = BattlePlayerInfo[id: 8,hp: 3000,player_pid: self,spell_trap_zone: [{2,spell_trap}]]
    battle_data = BattleData[turn_count: 2,turn_player_id: 6,operator_id: 6,phase: :mp1,player1_id: 6,player2_id: 8,
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
        assert message == <<0, 20, 46, 232, 3, 1, 0, 1, 0, 0, 0, 8, 2, 0, 1, 0, 0, 0, 11, 2>>
    end
    assert battle_data.choose_callback != nil
    {result,battle_data} = battle_data.choose_callback.([{8,:spell_trap_zone,[2]}],battle_data)
    assert result == :ok
    assert battle_data.choose_callback == nil
    receive do
      {:send,message}->
        assert message == <<0, 8, 46, 236, 0, 0, 0, 11>>
    end
    {result,battle_data} = battle_data.answer_callback.(:yes,battle_data)
    assert result == :ok
    assert battle_data.answer_callback == nil

    {:ok,battle_data} = OperationsCore.get_operations 8,:spell_trap_zone,2,battle_data
    assert_receive {:send,<<0, 7, 46, 231, 0, 1, 3>>}

    {result,battle_data} = FireEffectCore.fire_effect(8,:spell_trap_zone,2,battle_data)
    assert result == :ok
    receive do
      {:send,message}->
        assert message ==  <<0, 4, 46, 237>>
    end
    receive do
      {:send,message}->
        assert message == <<0, 24, 46, 233, 0, 1, 0, 0, 0, 4, 0, 4, 49, 49, 59, 49, 0, 1, 0, 0, 0, 8, 2, 2>>
    end
    receive do
      {:send,message}->
        assert message == <<0, 24, 46, 233, 0, 1, 0, 0, 0, 4, 0, 4, 49, 49, 59, 49, 0, 1, 0, 0, 0, 8, 2, 2>>
    end
    receive do
      {:send,message}->
        assert message == <<0, 20, 46, 232, 3, 1, 0, 1, 0, 0, 0, 6, 2, 0, 1, 0, 0, 0, 11, 2>>
    end
    assert battle_data.choose_callback != nil
    {result,battle_data} = battle_data.choose_callback.([{6,:spell_trap_zone,[2]}],battle_data)
    assert result == :ok
    assert battle_data.choose_callback == nil
    assert battle_data.player1_battle_info.handcards == []
    assert battle_data.player1_battle_info.deckcards == []
    assert battle_data.player1_battle_info.graveyardcards == [11]
    assert battle_data.player2_battle_info.graveyardcards == [11]
    assert_receive {:send,<<0, 4, 46, 237>>}
    receive do
      {:send,message}->
        assert message == <<0, 28, 46, 233, 0, 1, 0, 0, 0, 1, 0, 8, 54, 59, 49, 49, 59, 56, 59, 48, 0, 1, 0, 0, 0, 6, 2, 2>>
    end
    receive do
      {:send,message}->
        assert message == <<0, 28, 46, 233, 0, 1, 0, 0, 0, 1, 0, 8, 54, 59, 49, 49, 59, 56, 59, 48, 0, 1, 0, 0, 0, 6, 2, 2>>
    end
    receive do
      {:send,message}->
        assert message == <<0, 29, 46, 233, 0, 1, 0, 0, 0, 1, 0, 9, 54, 59, 49, 49, 59, 56, 59, 49, 49, 0, 1, 0, 0, 0, 8, 2, 2>>
    end
    receive do
      {:send,message}->
        assert message == <<0, 29, 46, 233, 0, 1, 0, 0, 0, 1, 0, 9, 54, 59, 49, 49, 59, 56, 59, 49, 49, 0, 1, 0, 0, 0, 8, 2, 2>>
    end
  end

  test "fire effect answer no" do
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
    {result,battle_data} = battle_data.choose_callback.([{8,:spell_trap_zone,[2]}],battle_data)
    assert result == :ok
    assert battle_data.choose_callback == nil
    receive do
      {:send,message}->
        assert message == <<0, 8, 46, 236, 0, 0, 0, 11>>
    end

    # first pause
    assert_receive {:send,<<0, 4, 46, 237>>}

    assert battle_data.answer_callback != nil

    {result,battle_data} = battle_data.answer_callback.(:no,battle_data)
    assert result == :ok
    assert Dict.size(battle_data.player1_battle_info.spell_trap_zone) == 0
    assert Dict.size(battle_data.player2_battle_info.spell_trap_zone) == 0
    assert battle_data.player1_battle_info.graveyardcards == [11]
    assert battle_data.player2_battle_info.graveyardcards == [12]
    # pause resume
    assert_receive {:send,<<0, 4, 46, 237>>}
    assert_receive {:send,<<0, 28, 46, 233, 0, 1, 0, 0, 0, 1, 0, 8, 54, 59, 48, 59, 56, 59, 49, 50, 0, 1, 0, 0, 0, 8, 2, 2>>}
    assert_receive {:send,<<0, 28, 46, 233, 0, 1, 0, 0, 0, 1, 0, 8, 54, 59, 48, 59, 56, 59, 49, 50, 0, 1, 0, 0, 0, 8, 2, 2>>}
    assert_receive {:send,<<0, 29, 46, 233, 0, 1, 0, 0, 0, 1, 0, 9, 54, 59, 49, 49, 59, 56, 59, 49, 50, 0, 1, 0, 0, 0, 6, 2, 2>>}
    assert_receive {:send,<<0, 29, 46, 233, 0, 1, 0, 0, 0, 1, 0, 9, 54, 59, 49, 49, 59, 56, 59, 49, 50, 0, 1, 0, 0, 0, 6, 2, 2>>}
  end

  test "fire effect answer yes" do
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
    {result,battle_data} = battle_data.choose_callback.([{8,:spell_trap_zone,[2]}],battle_data)
    assert result == :ok
    assert battle_data.choose_callback == nil
    # ask for chain
    receive do
      {:send,message}->
        assert message == <<0, 8, 46, 236, 0, 0, 0, 11>>
    end

    # pause player1
    assert_receive {:send,<<0, 4, 46, 237>>}

    assert battle_data.answer_callback != nil

    {result,battle_data} = battle_data.answer_callback.(:yes,battle_data)
    assert result == :ok
    assert battle_data.answer_callback == nil

    {:ok,battle_data} = OperationsCore.get_operations 8,:spell_trap_zone,2,battle_data
    receive do
      {:send,message}->
        assert message == <<0, 7, 46, 231, 0, 1, 3>>
    end

    {result,battle_data} = FireEffectCore.fire_effect(8,:spell_trap_zone,2,battle_data)
    assert result == :ok
    assert battle_data.player1_battle_info.handcards == [12,7]
    assert battle_data.player1_battle_info.deckcards == []
    assert battle_data.player1_battle_info.graveyardcards == [11]
    assert battle_data.player2_battle_info.graveyardcards == [12]
    receive do
      {:send,message}->
        assert message == <<0, 24, 46, 233, 0, 1, 0, 0, 0, 4, 0, 4, 49, 50, 59, 49, 0, 1, 0, 0, 0, 8, 2, 2>>
    end
    receive do
      {:send,message}->
        assert message == <<0, 24, 46, 233, 0, 1, 0, 0, 0, 4, 0, 4, 49, 50, 59, 49, 0, 1, 0, 0, 0, 8, 2, 2>>
    end
    assert_receive {:send,<<0, 4, 46, 237>>}
    receive do
      {:send,message}->
        assert message == <<0, 28, 46, 233, 0, 1, 0, 0, 0, 1, 0, 8, 54, 59, 49, 49, 59, 56, 59, 48, 0, 1, 0, 0, 0, 6, 2, 2>>
    end
    receive do
      {:send,message}->
        assert message == <<0, 28, 46, 233, 0, 1, 0, 0, 0, 1, 0, 8, 54, 59, 49, 49, 59, 56, 59, 48, 0, 1, 0, 0, 0, 6, 2, 2>>
    end
    receive do
      {:send,message}->
        assert message == <<0, 17, 46, 233, 0, 1, 0, 0, 0, 5, 0, 3, 54, 59, 55, 0, 0>>
    end
    receive do
      {:send,message}->
        assert message == <<0, 17, 46, 233, 0, 1, 0, 0, 0, 5, 0, 3, 54, 59, 48, 0, 0>>
    end
    assert_receive {:send,<<0, 29, 46, 233, 0, 1, 0, 0, 0, 1, 0, 9, 54, 59, 49, 49, 59, 56, 59, 49, 50, 0, 1, 0, 0, 0, 8, 2, 2>>}
    assert_receive {:send,<<0, 29, 46, 233, 0, 1, 0, 0, 0, 1, 0, 9, 54, 59, 49, 49, 59, 56, 59, 49, 50, 0, 1, 0, 0, 0, 8, 2, 2>>}
  end
end