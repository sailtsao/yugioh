defmodule SummonCoreTest do
  use ExUnit.Case
  test "normal summon test" do
    player1_battle_info = BattlePlayerInfo[id: 6,hp: 3000,player_pid: self,handcards: [7,10]]
    player2_battle_info = BattlePlayerInfo[id: 8,hp: 3000,player_pid: self]
    battle_data = BattleData[turn_count: 1,operator_id: 6,phase: :mp1,player1_id: 6,player2_id: 8,
    player1_battle_info: player1_battle_info,player2_battle_info: player2_battle_info]
    {result,battle_data} = SummonCore.summon(6,0,:attack,:normal_summon,battle_data)
    assert result == :ok
    assert battle_data.normal_summoned == true
    assert battle_data.player1_battle_info.monster_zone[2] != nil
    assert battle_data.player1_battle_info.monster_zone[2].id == 7
    assert battle_data.player1_battle_info.monster_zone[2].presentation == :attack
    assert battle_data.player1_battle_info.monster_zone[2].presentation_changed == true
  end

  test "special summon test" do
    player1_battle_info = BattlePlayerInfo[id: 6,hp: 3000,player_pid: self,handcards: [7,10],deckcards: [10]]
    player2_battle_info = BattlePlayerInfo[id: 8,hp: 3000,player_pid: self]
    battle_data = BattleData[turn_count: 1,operator_id: 6,phase: :mp1,player1_id: 6,player2_id: 8,
    player1_battle_info: player1_battle_info,player2_battle_info: player2_battle_info]
    {result,battle_data} = SummonCore.summon(6,0,:attack,:special_summon,battle_data)
    assert result == :ok
    assert battle_data.choose_callback != nil
    {result,battle_data} = battle_data.choose_callback.([{6,:handcard_zone,[1]}],battle_data)
    assert result == :ok
    # IO.inspect battle_data
    assert battle_data.choose_callback == nil
    assert battle_data.player1_battle_info.monster_zone[2] != nil
    assert battle_data.player1_battle_info.monster_zone[2].id == 7
    assert battle_data.player1_battle_info.monster_zone[2].presentation == :attack
    assert battle_data.player1_battle_info.monster_zone[2].presentation_changed == true
    assert battle_data.player1_battle_info.handcards == []
    assert battle_data.player1_battle_info.graveyardcards == [10]
    assert battle_data.normal_summoned == false
    # choose card message
    receive do
        {:send,message}->
            assert message == <<0, 20, 46, 232, 3, 1, 0, 1, 6::32, 7::8, 0, 1, 0, 0, 0, 10, 1>>
    end
    # move to grave yard message
    receive do
        {:send,message}->
            assert message == <<0, 28, 46, 233, 0, 1, 0, 0, 0, 1, 0, 8, 54, 59, 49, 48, 59, 56, 59, 48, 0, 1, 0, 0, 0, 6, 7, 1>>
    end
    receive do
        {:send,message}->
            assert message == <<0, 28, 46, 233, 0, 1, 0, 0, 0, 1, 0, 8, 54, 59, 49, 48, 59, 56, 59, 48, 0, 1, 0, 0, 0, 6, 7, 1>>
    end
    # summon message
    receive do
        {:send,message}->
            assert message == <<0, 25, 46, 233, 0, 1, 0, 0, 0, 2, 0, 5, 48, 59, 55, 59, 49, 0, 1, 0, 0, 0, 6, 1, 2>>
    end
    receive do
        {:send,message}->
            assert message == <<0, 25, 46, 233, 0, 1, 0, 0, 0, 2, 0, 5, 48, 59, 55, 59, 49, 0, 1, 0, 0, 0, 6, 1, 2>>
    end
  end

  test "tribute summon test" do
    monster = Data.Cards.get(1).become_monster
    monster = monster.presentation :attack
    player1_battle_info = BattlePlayerInfo[id: 6,hp: 3000,player_pid: self,handcards: [7,1],monster_zone: HashDict.new([{2,monster}])]
    player2_battle_info = BattlePlayerInfo[id: 8,hp: 3000,player_pid: self]
    battle_data = BattleData[turn_count: 1,operator_id: 6,phase: :mp1,player1_id: 6,player2_id: 8,
    player1_battle_info: player1_battle_info,player2_battle_info: player2_battle_info]
    {result,battle_data} = SummonCore.summon(6,1,:attack,:normal_summon,battle_data)
    assert result == :ok
    assert battle_data.choose_callback != nil
    {result,battle_data} = battle_data.choose_callback.([{6,:monster_zone,[2]}],battle_data)
    assert result == :ok
    assert battle_data.choose_callback == nil
    assert battle_data.player1_battle_info.monster_zone_size == 1
    assert battle_data.player1_battle_info.monster_zone[2] != nil
    assert battle_data.player1_battle_info.monster_zone[2].id == 1
    assert battle_data.player1_battle_info.monster_zone[2].presentation == :attack
    assert battle_data.player1_battle_info.monster_zone[2].presentation_changed == true
    assert battle_data.player1_battle_info.handcards == [7]
    assert battle_data.player1_battle_info.graveyardcards == [1]
    assert battle_data.normal_summoned == true
  end
end