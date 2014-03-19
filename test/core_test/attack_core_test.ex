defmodule AttackCoreTest do
  use ExUnit.Case
  test "attack player test" do
    # 2400 attack
    monster1 = Data.Cards.get(1).become_monster
    monster1 = monster1.presentation :attack
    # 1000 attack
    monster2 = Data.Cards.get(2).become_monster
    monster2 = monster2.presentation :attack
    player1_battle_info = BattlePlayerInfo[id: 6,hp: 3000,player_pid: self,monster_zone: HashDict.new([{2,monster1}])]
    player2_battle_info = BattlePlayerInfo[id: 8,hp: 3000,player_pid: self,monster_zone: HashDict.new([{2,monster2}])]
    battle_data = BattleData[turn_count: 2,operator_id: 6,phase: :bp,player1_id: 6,player2_id: 8,
    player1_battle_info: player1_battle_info,player2_battle_info: player2_battle_info]
    {:ok,battle_data} = AttackCore.attack 6,2,battle_data
    assert battle_data.choose_callback != nil
    {:ok,battle_data} = battle_data.choose_callback.([{8,:monster_zone,[2]}],battle_data)
    assert battle_data.player2_battle_info.hp == 1600
    assert battle_data.player2_battle_info.monster_zone[2] == nil
    assert battle_data.player2_battle_info.graveyardcards == [2]
    assert battle_data.player1_battle_info.monster_zone[2].attacked == true

  end
end