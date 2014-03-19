defmodule ChooseCoreTest do
  use ExUnit.Case

  test "choose core test" do
    # skill = Skill.new(type: :normal_skill,
    #   check_phase: [:main_phase_1,:main_phase_2],
    #   skill_effects: [
    #       SkillEffect.new(id: 1,
    #       params: "1;1;7;0;1;1;3",
    #       priority: 1
    #       ),
    #       SkillEffect.new(id: 1,
    #       params: "1;1;4;0;1;1;3",
    #       priority: 2
    #       )
    #   ],
    #   and_conditions: [
    #       Condition.new(id: 1,
    #       params: "1;7;0;1;1;1;0"
    #       ),
    #       Condition.new(id: 1,
    #       params: "1;4;0;1;1;1;0"
    #       )
    #   ],
    #   or_conditions: [
    #   ])
    [skill] = Data.Cards.get(7).get_normal_skills
    player1_battle_info = BattlePlayerInfo[id: 6,hp: 3000,player_pid: self,handcards: [7,10],deckcards: [7,10]]
    player2_battle_info = BattlePlayerInfo[id: 8,hp: 3000,player_pid: self]
    battle_data = BattleData[turn_count: 1,operator_id: 6,phase: :mp1,player1_id: 6,player2_id: 8,
    player1_battle_info: player1_battle_info,player2_battle_info: player2_battle_info]
    callback = fn(_choose_result_list,battle_data)->
      {:ok,battle_data}
    end
    {result,battle_data} = ChooseCore.execute_skill_choose skill,battle_data,[{:info,{6,:handcard_zone,0}},{:callback,callback}]
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