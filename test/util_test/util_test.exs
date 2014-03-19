defmodule UtilTest do
  test "util test" do
    player_battle_info = BattlePlayerInfo[handcards: [1,2,3,4,5,6,7,8,9,10,11,12]]
    battle_data = BattleData[turn_count: 1,operator_id: 6,phase: :mp1,player1_id: 6,player2_id: 8,
    player1_battle_info: player_battle_info,player2_battle_info: BattlePlayerInfo.new]
    IO.inspect get_id_index_list_from_scene(player_battle_info,:handcard_zone,:monster_card,:none,0,nil)
  end
end