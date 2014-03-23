defmodule ChooseCoreTest do
  use ExUnit.Case

  # test "choose callback" do
  #   ChooseCore.choose
  # end

  test "choose core test" do
    [skill] = Data.Cards.get(7).get_normal_skills
    player1_battle_info = BattlePlayerInfo[id: 6,hp: 3000,player_pid: self,handcards: [7,10],deckcards: [7,10]]
    player2_battle_info = BattlePlayerInfo[id: 8,hp: 3000,player_pid: self]
    battle_data = BattleData[turn_count: 1,operator_id: 6,phase: :mp1,player1_id: 6,player2_id: 8,
    player1_battle_info: player1_battle_info,player2_battle_info: player2_battle_info]
    {result,battle_data} = ChooseCore.choose 6,:handcard_zone,0,skill,battle_data,fn(_choose_result_list,battle_data)->
      {:ok,battle_data}
    end
    assert result == :ok
    receive do
      {:send,message}->
                                              #reason num arraylength playerid scene_type
        assert message == <<20::16, 12008::16, 3::8, 1::8, 1::16, 6::32, 7::8, 1::16, 10::32, 1::8>>
    end
    assert battle_data.choose_callback != nil
    {result,battle_data} = battle_data.choose_callback.([{6,:handcard_zone,[1]}],battle_data)
    assert result == :ok
    assert battle_data.choose_callback != nil
    receive do
      {:send,message}->
                                              #reason num arraylength playerid scene_type
        assert message == <<25::16, 12008::16, 3::8, 1::8, 1::16, 6::32, 4::8, 2::16, 7::32, 0::8 ,10::32, 1::8>>
    end
    {result,battle_data} = battle_data.choose_callback.([{6,:deck_zone,[1]}],battle_data)
    assert result == :ok
    assert battle_data.choose_callback == nil
  end
end