defmodule BattleSummonTest do
  use ExUnit.Case
  alias Yugioh.Data.Cards
  setup do
    battle_info = BattleInfo.new(
    player_pid: self,
    socket: self,
    maxhp: 3000,curhp: 3000,
    monster_card_zone: HashDict.new([{1,Monster.new(id: 7)}]),
    spell_trap_zone: HashDict.new,
    extradeckcards: [],
    graveyardcards: [1,2],
    deckcards: [1,2,3,4],
    banishedcards: [],
    handcards: [11,1,1,1,2,7],
    field_card: nil)
    battle_data = BattleData.new(turn_count: 2,operator_id: 1,phase: :mp1,player1_id: 1,player2_id: 2,
    player1_battle_info: battle_info,player2_battle_info: battle_info,normal_summoned: false)
    {:ok,[battle_data: battle_data]}
  end

  test "summon spell trap ok test" do
    battle_info = BattleInfo.new(
    player_pid: self,
    socket: self,
    maxhp: 3000,curhp: 3000,
    monster_card_zone: HashDict.new([{1,Monster.new(id: 7)}]),
    spell_trap_zone: HashDict.new,
    extradeckcards: [],
    graveyardcards: [1,2],
    deckcards: [1,2,3,4],
    banishedcards: [],
    handcards: [11,1,1,1,2,7],
    field_card: nil)
    battle_data = BattleData.new(turn_count: 2,operator_id: 1,phase: :mp1,player1_id: 1,player2_id: 2,
    player1_battle_info: battle_info,player2_battle_info: battle_info,normal_summoned: false)
    {result,_battle_data} = BattleCore.summon_card(Cards.get(11),0,:place,battle_data)
    assert result == :ok
    :ok
  end

  test "summon spell trap error test" do
    battle_info = BattleInfo.new(
    player_pid: self,
    socket: self,
    maxhp: 3000,curhp: 3000,
    monster_card_zone: HashDict.new([{1,Monster.new(id: 7)}]),
    spell_trap_zone: HashDict.new([{0,Monster.new(id: 7)},{1,Monster.new(id: 7)},{2,Monster.new(id: 7)},{3,Monster.new(id: 7)},{4,Monster.new(id: 7)}]),
    extradeckcards: [],
    graveyardcards: [1,2],
    deckcards: [1,2,3,4],
    banishedcards: [],
    handcards: [11,1,1,1,2,7],
    field_card: nil)
    battle_data = BattleData.new(turn_count: 2,operator_id: 1,phase: :mp1,player1_id: 1,player2_id: 2,
    player1_battle_info: battle_info,player2_battle_info: battle_info,normal_summoned: false)
    {result,_battle_data} = BattleCore.summon_card(Cards.get(11),0,:place,battle_data)
    assert result == :already_have_5_magic_trap
    :ok
  end

  test "summon normal monster test" do
    battle_info = BattleInfo.new(
    player_pid: self,
    socket: self,
    maxhp: 3000,curhp: 3000,
    monster_card_zone: HashDict.new([{1,Monster.new(id: 7)}]),
    spell_trap_zone: HashDict.new,
    extradeckcards: [],
    graveyardcards: [1,2],
    deckcards: [1,2,3,4],
    banishedcards: [],
    handcards: [2,1,1,1,2,7],
    field_card: nil)
    battle_data = BattleData.new(turn_count: 2,operator_id: 1,phase: :mp1,player1_id: 1,player2_id: 2,
    player1_battle_info: battle_info,player2_battle_info: battle_info,normal_summoned: false)
    {result,_battle_data} = BattleCore.summon_card(Cards.get(2),0,:defense_down,battle_data)
    assert result == :ok
    :ok
  end

  test "summon tribute monster test" do
    battle_info = BattleInfo.new(
    player_pid: self,
    socket: self,
    maxhp: 3000,curhp: 3000,
    monster_card_zone: HashDict.new([{1,Monster.new(id: 7)}]),
    spell_trap_zone: HashDict.new,
    extradeckcards: [],
    graveyardcards: [1,2],
    deckcards: [1,2,3,4],
    banishedcards: [],
    handcards: [1,1,1,1,2,7],
    field_card: nil)
    battle_data = BattleData.new(turn_count: 2,operator_id: 1,phase: :mp1,player1_id: 1,player2_id: 2,
    player1_battle_info: battle_info,player2_battle_info: battle_info,normal_summoned: false)
    {result,battle_data} = BattleCore.summon_card(Cards.get(1),0,:attack,battle_data)
    assert result == :ok
    assert battle_data.phase == {:choose_tribute_card_for_summon_phase, :mp1, 1, 0, :attack}
    :ok
  end
end