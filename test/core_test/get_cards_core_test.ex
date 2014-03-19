defmodule GetCardsCoreTest do
  use ExUnit.Case
  test "get cards test" do
    player_battle_info = BattlePlayerInfo[graveyardcards: [1,2,3,4,5],player_pid: self]
    battle_data = BattleData[turn_count: 1,operator_id: 6,phase: :mp1,player1_id: 6,player2_id: 8,
    player1_battle_info: player_battle_info,player2_battle_info: nil]
    {result,battle_data} = GetCardsCore.get_cards_of_scene 6,:graveyard_zone,battle_data
    assert result == :ok
    # assert result == {:ok,[1,2,3,4,5],<<0, 31, 46, 234, 0, 0, 0, 6, 3, 0, 5, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5>>}
  end
end