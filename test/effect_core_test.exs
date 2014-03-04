defmodule EffectCoreTest do
  use ExUnit.Case
 
  setup do
    skill = Skill.new(type: :normal_skill,
      check_phase: [:main_phase_1,:main_phase_2],
      skill_effects: [
          SkillEffect.new(id: 1,
          params: "1;7;0;1",
          priority: 1
          ),
          SkillEffect.new(id: 1,
          params: "1;4;0;1",
          priority: 2
          )
      ],
      and_conditions: [
          Condition.new(id: 1,
          params: "7;1;0;1"
          ),
          Condition.new(id: 1,
          params: "4;1;0;1"
          )
      ],
      or_conditions: [
      ]
      )
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
    {:ok,[skill: skill,battle_data: battle_data,battle_info: battle_info]}
  end 
  # test "effect core test for empty skill_effects" do
  #   battle_data = EffectCore.execute_skill_effects(Skill.new,BattleData.new,[{}])
  #   assert battle_data == BattleData.new
  # end

  test "effect core test for real skill_effects",meta do
    battle_data = EffectCore.execute_skill_effects(meta[:skill],meta[:battle_data],[{:handcards_index,5},{:presentation,:attack}])
    receive do
      {:send,message}->
        IO.inspect message
    end
    IO.inspect battle_data
    battle_data = EffectCore.resume_execute_effect_after_choose([3],battle_data)
    IO.inspect battle_data
    :ok
  end

  # test "effect core test for resume skill_effects",meta do
  #   battle_data = meta[:battle_data].phase({:choose_tribute_card_for_effect_1, :mp1, "1;7;5;1",
  #   [SkillEffect[id: 2, params: "", priority: 2]],
  #   [handcards_index: 5, presentation: :attack]})

  #   battle_data = EffectCore.resume_execute_effect_after_choose([3],battle_data)

  #   receive do
  #     {:send,message}->
  #       IO.inspect message
  #   end

  #   receive do
  #     {:send,message}->
  #       IO.inspect message
  #   end

  #   receive do
  #     {:send,message}->
  #       IO.inspect message
  #   end
  #   receive do
  #     {:send,message}->
  #       IO.inspect message
  #   end
  #   IO.inspect battle_data
  #   :ok
  # end

  # test "special summon test",meta do
  #   skill = meta[:skill]
  #   battle_info = meta[:battle_info]
  #   # IO.inspect Yugioh.Proto.PT12.write(:choose_card,[:handcard_tribute_choose,:self,:deck_zone,1,[{1,1},{2,2}]])
  # #   # IO.inspect Util.get_special_summon_skill([skill])
  # #   # IO.inspect Util.get_special_summon_skill([])
  # #   # IO.inspect ConditionCore.is_skill_conditions_satisfied skill,battle_info
  #   # IO.inspect BattleCore.is_card_can_fire_effect Yugioh.Data.Cards.get(7),battle_info
  # #   # player_battle_info = BattleCore.get_player_battle_info 1,battle_data
  # #   scene_type = :monster_card_zone
  # #   card_id = case scene_type do
  # #     :handcard_zone->
  # #       Enum.at player_battle_info.handcards,3
  # #     :monster_card_zone->
  # #       Dict.get(player_battle_info.monster_card_zone,1).id
  # #   end
  # #   card_data = Yugioh.Data.Cards.get(card_id)
  # #   [skill] = Util.get_normal_skills(card_data.skills)
  # #   result = :ok

  #   # IO.inspect ConditionCore.is_skill_conditions_satisfied(skill,player_battle_info)
  #   # battle_data = meta[:battle_data]
  #   # IO.inspect EffectCore.execute_skill_effects(skill,battle_data,[])

  # #   receive do
  # #     {:send,message}->
  # #       IO.inspect message
  # #   end
  # #   # IO.inspect BattleCore.is_card_can_be_special_summoned Yugioh.Data.Cards.get(1),battle_info
  # #   # IO.inspect BattleCore.get_handcard_special_summon_operations 7,battle_info
  # #   :ok
  # end
end