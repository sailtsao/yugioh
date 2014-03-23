defmodule OperationsCoreTest do
  use ExUnit.Case,async: true
  test "get handcards operations test" do
    player_battle_info = BattlePlayerInfo[id: 6,player_pid: self,handcards: [1,2,3,4,5,6,7,8,9,10,11,12]]
    battle_data = BattleData[turn_count: 1,operator_id: 6,phase: :mp1,player1_id: 6,player2_id: 8,
    player1_battle_info: player_battle_info,player2_battle_info: BattlePlayerInfo.new]
    # Enum.each 0..11,fn(i)->
    #   IO.inspect OperationsCore.get_operations(6,:handcard_zone,i,battle_data)
    # end
    {result,battle_data1} = OperationsCore.get_operations(6,:handcard_zone,0,battle_data)
    assert result == :ok
    assert battle_data == battle_data1
    receive do
      {:send,message}->
        assert message == <<0, 6, 46, 231, 0, 0>>
    end
    {result,battle_data1} = OperationsCore.get_operations(6,:handcard_zone,1,battle_data)
    assert result == :ok
    assert battle_data == battle_data1
    receive do
      {:send,message}->
        assert message == <<0, 8, 46, 231, 0, 2, 1, 2>>
    end
    {result,battle_data1} = OperationsCore.get_operations(6,:handcard_zone,2,battle_data)
    assert result == :ok
    assert battle_data == battle_data1
    receive do
      {:send,message}->
        assert message == <<0, 8, 46, 231, 0, 2, 1, 2>>
    end
    {result,battle_data1} = OperationsCore.get_operations(6,:handcard_zone,3,battle_data)
    assert result == :ok
    assert battle_data == battle_data1
    receive do
      {:send,message}->
        assert message == <<0, 8, 46, 231, 0, 2, 1, 2>>
    end
    {result,battle_data1} = OperationsCore.get_operations(6,:handcard_zone,4,battle_data)
    assert result == :ok
    assert battle_data == battle_data1
    receive do
      {:send,message}->
        assert message == <<0, 8, 46, 231, 0, 2, 1, 2>>
    end
    {result,battle_data1} = OperationsCore.get_operations(6,:handcard_zone,5,battle_data)
    assert result == :ok
    assert battle_data == battle_data1
    receive do
      {:send,message}->
        assert message == <<0, 8, 46, 231, 0, 2, 1, 2>>
    end
    {result,battle_data1} = OperationsCore.get_operations(6,:handcard_zone,6,battle_data)
    assert result == :ok
    assert battle_data == battle_data1
    receive do
      {:send,message}->
        assert message == <<0, 9, 46, 231, 0, 3, 1, 2, 8>>
    end
    {result,battle_data1} = OperationsCore.get_operations(6,:handcard_zone,7,battle_data)
    assert result == :ok
    assert battle_data == battle_data1
    receive do
      {:send,message}->
        assert message == <<0, 6, 46, 231, 0, 0>>
    end
    {result,battle_data1} = OperationsCore.get_operations(6,:handcard_zone,8,battle_data)
    assert result == :ok
    assert battle_data == battle_data1
    receive do
      {:send,message}->
        assert message == <<0, 6, 46, 231, 0, 0>>
    end
    {result,battle_data1} = OperationsCore.get_operations(6,:handcard_zone,9,battle_data)
    assert result == :ok
    assert battle_data == battle_data1
    receive do
      {:send,message}->
        assert message == <<0, 6, 46, 231, 0, 0>>
    end
    {result,battle_data1} = OperationsCore.get_operations(6,:handcard_zone,10,battle_data)
    assert result == :ok
    assert battle_data == battle_data1
    receive do
      {:send,message}->
        assert message == <<0, 7, 46, 231, 0, 1, 2>>
    end
    {result,battle_data1} = OperationsCore.get_operations(6,:handcard_zone,11,battle_data)
    assert result == :ok
    assert battle_data == battle_data1
    receive do
      {:send,message}->
        assert message == <<0, 7, 46, 231, 0, 1, 2>>
    end
  end

  test "get handcards operations of tribute summon" do
    monster = Data.Cards.get(7).become_monster
    player1_battle_info = BattlePlayerInfo[id: 6,player_pid: self,monster_zone: HashDict.new([{2,monster}]),handcards: [9]]
    battle_data = BattleData[turn_count: 1,operator_id: 6,phase: :mp1,player1_id: 6,player2_id: 8,
    player1_battle_info: player1_battle_info,player2_battle_info: nil]
    {result,_battle_data} = OperationsCore.get_operations(6,:handcard_zone,0,battle_data)
    assert result == :ok
    receive do
      {:send,message}->
        assert message == <<0, 8, 46, 231, 0, 2, 1, 2>>
    end
  end

  test "get handcards operations of spell card" do
    spell_trap = Data.Cards.get(11).become_spell_trap
    player1_battle_info = BattlePlayerInfo[id: 6,player_pid: self,handcards: [11]]
    player2_battle_info = BattlePlayerInfo[id: 8,player_pid: self,spell_trap_zone: HashDict.new([{2,spell_trap}])]
    battle_data = BattleData[turn_count: 1,operator_id: 6,phase: :mp1,player1_id: 6,player2_id: 8,
    player1_battle_info: player1_battle_info,player2_battle_info: player2_battle_info]
    {result,_battle_data} = OperationsCore.get_operations(6,:handcard_zone,0,battle_data)
    assert result == :ok
    receive do
      {:send,message}->
        assert message == <<0, 8, 46, 231, 0, 2, 3, 2>>
    end
  end

  test "get monster zone operations test mp phase" do
    monster = Data.Cards.get(7).become_monster
    monster = monster.attacked false
    monster = monster.presentation_changed true
    monster = monster.presentation :defense_down
    player_battle_info = BattlePlayerInfo[id: 6,player_pid: self,monster_zone: HashDict.new([{2,monster}]),deckcards: [1,2,3,4,5,6,7,8,9,10],handcards: [1,2,3,4,5,6,7,8,9,10]]
    battle_data = BattleData[turn_count: 1,operator_id: 6,phase: :mp1,player1_id: 6,player2_id: 8,
    player1_battle_info: player_battle_info,player2_battle_info: BattlePlayerInfo.new]
    {result,_battle_data} = OperationsCore.get_operations(6,:monster_zone,2,battle_data)
    assert result == :ok
    receive do
      {:send,message}->
        assert message == <<0, 7, 46, 231, 0, 1, 3>>
    end
  end

  test "get monster zone operations test mp phase reverse" do
    monster = Data.Cards.get(7).become_monster
    monster = monster.attacked false
    monster = monster.presentation_changed false
    monster = monster.presentation :defense_down
    player_battle_info = BattlePlayerInfo[id: 6,player_pid: self,monster_zone: HashDict.new([{2,monster}]),deckcards: [1,2,3,4,5,6,7,8,9,10],handcards: [1,2,3,4,5,6,7,8,9,10]]
    battle_data = BattleData[turn_count: 1,operator_id: 6,phase: :mp1,player1_id: 6,player2_id: 8,
    player1_battle_info: player_battle_info,player2_battle_info: BattlePlayerInfo.new]
    {result,_battle_data} = OperationsCore.get_operations(6,:monster_zone,2,battle_data)
    assert result == :ok
    receive do
      {:send,message}->
        assert message == <<0, 8, 46, 231, 0, 2, 7, 3>>
    end
  end

  test "get monster zone operations test mp phase defense up" do
    monster = Data.Cards.get(7).become_monster
    monster = monster.attacked false
    monster = monster.presentation_changed false
    monster = monster.presentation :defense_up
    player_battle_info = BattlePlayerInfo[id: 6,player_pid: self,monster_zone: HashDict.new([{2,monster}]),deckcards: [1,2,3,4,5,6,7,8,9,10],handcards: [1,2,3,4,5,6,7,8,9,10]]
    battle_data = BattleData[turn_count: 1,operator_id: 6,phase: :mp1,player1_id: 6,player2_id: 8,
    player1_battle_info: player_battle_info,player2_battle_info: BattlePlayerInfo.new]
    {result,_battle_data} = OperationsCore.get_operations(6,:monster_zone,2,battle_data)
    assert result == :ok
    receive do
      {:send,message}->
        assert message == <<0, 8, 46, 231, 0, 2, 5, 3>>
    end
  end

  test "get monster zone operations test mp phase attack" do
    monster = Data.Cards.get(7).become_monster
    monster = monster.attacked false
    monster = monster.presentation_changed false
    monster = monster.presentation :attack
    player_battle_info = BattlePlayerInfo[id: 6,player_pid: self,monster_zone: HashDict.new([{2,monster}]),deckcards: [1,2,3,4,5,6,7,8,9,10],handcards: [1,2,3,4,5,6,7,8,9,10]]
    battle_data = BattleData[turn_count: 1,operator_id: 6,phase: :mp1,player1_id: 6,player2_id: 8,
    player1_battle_info: player_battle_info,player2_battle_info: BattlePlayerInfo.new]
    {result,_battle_data} = OperationsCore.get_operations(6,:monster_zone,2,battle_data)
    assert result == :ok
    receive do
      {:send,message}->
        assert message == <<0, 8, 46, 231, 0, 2, 6, 3>>
    end
  end

  test "get monster zone operations test bp phase turn 1" do
    monster = Data.Cards.get(7).become_monster
    player_battle_info = BattlePlayerInfo[id: 6,player_pid: self,monster_zone: HashDict.new([{2,monster}]),deckcards: [1,2,3,4,5,6,7,8,9,10],handcards: [1,2,3,4,5,6,7,8,9,10]]
    battle_data = BattleData[turn_count: 1,operator_id: 6,phase: :bp,player1_id: 6,player2_id: 8,
    player1_battle_info: player_battle_info,player2_battle_info: BattlePlayerInfo.new]
    {result,_battle_data} = assert OperationsCore.get_operations(6,:monster_zone,2,battle_data)
    assert result == :ok
    receive do
      {:send,message}->
        assert message == <<0, 6, 46, 231, 0, 0>>
    end
  end

  test "get monster zone operations test bp phase turn 2" do
    monster = Data.Cards.get(7).become_monster
    monster = monster.attacked false
    monster = monster.presentation :attack
    player_battle_info = BattlePlayerInfo[id: 6,player_pid: self,monster_zone: HashDict.new([{2,monster}]),deckcards: [1,2,3,4,5,6,7,8,9,10],handcards: [1,2,3,4,5,6,7,8,9,10]]
    battle_data = BattleData[turn_count: 2,operator_id: 6,phase: :bp,player1_id: 6,player2_id: 8,
    player1_battle_info: player_battle_info,player2_battle_info: BattlePlayerInfo.new]
    {result,_battle_data} = OperationsCore.get_operations(6,:monster_zone,2,battle_data)
    assert result == :ok
    receive do
      {:send,message}->
        assert message ==<<0, 7, 46, 231, 0, 1, 4>>
    end
  end

  test "get monster zone operations test bp phase turn 2 defnese up state" do
    monster = Data.Cards.get(7).become_monster
    monster = monster.attacked false
    monster = monster.presentation :defense_up
    player_battle_info = BattlePlayerInfo[id: 6,player_pid: self,monster_zone: HashDict.new([{2,monster}]),deckcards: [1,2,3,4,5,6,7,8,9,10],handcards: [1,2,3,4,5,6,7,8,9,10]]
    battle_data = BattleData[turn_count: 2,operator_id: 6,phase: :bp,player1_id: 6,player2_id: 8,
    player1_battle_info: player_battle_info,player2_battle_info: BattlePlayerInfo.new]
    {result,_battle_data} = OperationsCore.get_operations(6,:monster_zone,2,battle_data)
    assert result == :ok
    receive do
      {:send,message}->
        assert message == <<0, 6, 46, 231, 0, 0>>
    end
  end

  test "get monster zone operations test bp phase turn 2 defnese down state" do
    monster = Data.Cards.get(7).become_monster
    monster = monster.attacked false
    monster = monster.presentation :defnese_down
    player_battle_info = BattlePlayerInfo[id: 6,player_pid: self,monster_zone: HashDict.new([{2,monster}]),deckcards: [1,2,3,4,5,6,7,8,9,10],handcards: [1,2,3,4,5,6,7,8,9,10]]
    battle_data = BattleData[turn_count: 2,operator_id: 6,phase: :bp,player1_id: 6,player2_id: 8,
    player1_battle_info: player_battle_info,player2_battle_info: BattlePlayerInfo.new]
    {result,_battle_data} = OperationsCore.get_operations(6,:monster_zone,2,battle_data)
    assert result == :ok
    receive do
      {:send,message}->
        assert message ==  <<0, 6, 46, 231, 0, 0>>
    end
  end

  test "get monster zone operations test bp phase turn 2 attacked" do
    monster = Data.Cards.get(7).become_monster
    monster = monster.attacked true
    player_battle_info = BattlePlayerInfo[id: 6,player_pid: self,monster_zone: HashDict.new([{2,monster}]),deckcards: [1,2,3,4,5,6,7,8,9,10],handcards: [1,2,3,4,5,6,7,8,9,10]]
    battle_data = BattleData[turn_count: 2,operator_id: 6,phase: :bp,player1_id: 6,player2_id: 8,
    player1_battle_info: player_battle_info,player2_battle_info: BattlePlayerInfo.new]
    {result,_battle_data} = OperationsCore.get_operations(6,:monster_zone,2,battle_data)
    assert result == :ok
    receive do
      {:send,message}->
        assert message == <<0, 6, 46, 231, 0, 0>>
    end
  end

  test "get spell trap zone operations test" do
    spell_trap = Data.Cards.get(11).become_spell_trap
    spell_trap = spell_trap.count(1)
    player1_battle_info = BattlePlayerInfo[id: 6,player_pid: self,spell_trap_zone: HashDict.new([{2,spell_trap}]),deckcards: [1,2,3,4,5,6,7,8,9,10],handcards: [1,2,3,4,5,6,7,8,9,10]]
    player2_battle_info = BattlePlayerInfo[id: 8,player_pid: self,spell_trap_zone: HashDict.new([{2,spell_trap}]),deckcards: [1,2,3,4,5,6,7,8,9,10],handcards: [1,2,3,4,5,6,7,8,9,10]]
    battle_data = BattleData[turn_count: 1,operator_id: 6,phase: :mp1,player1_id: 6,player2_id: 8,
    player1_battle_info: player1_battle_info,player2_battle_info: player2_battle_info]
    {result,_battle_data} = OperationsCore.get_operations(6,:spell_trap_zone,2,battle_data)
    assert result == :ok
    receive do
      {:send,message}->
        assert message == <<0, 7, 46, 231, 0, 1, 3>>
    end
  end

  test "get spell trap zone operations test empty" do
    spell_trap = Data.Cards.get(11).become_spell_trap
    player_battle_info = BattlePlayerInfo[id: 6,player_pid: self,spell_trap_zone: HashDict.new([{2,spell_trap}]),deckcards: [1,2,3,4,5,6,7,8,9,10],handcards: [1,2,3,4,5,6,7,8,9,10]]
    battle_data = BattleData[turn_count: 1,operator_id: 6,phase: :mp1,player1_id: 6,player2_id: 8,
    player1_battle_info: player_battle_info,player2_battle_info: BattlePlayerInfo.new]
    {result,_battle_data} = OperationsCore.get_operations(6,:spell_trap_zone,2,battle_data)
    assert result == :ok
    receive do
      {:send,message}->
        assert message == <<0, 6, 46, 231, 0, 0>>
    end
  end
end