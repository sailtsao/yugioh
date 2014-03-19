defmodule FlipCardCore do
  # player order to flip card
  def flip_card(:order,card_index,battle_data = BattleData[phase: phase])
  when phase in [:mp1,:mp2] do
    player_id = battle_data.operator_id
    player_atom = battle_data.operator_atom
    player_battle_info = battle_data.operator_battle_info
    result = :ok
    monster = Dict.get(player_battle_info.monster_zone,card_index)
    if monster.presentation_changed do
      result = :already_changed_presentation_in_one_turn
    end
    if result == :ok do
      case monster.presentation do
        :defense_down->
          monster = monster.update(presentation: :attack,presentation_changed: true)
        :defense_up->
          monster = monster.update(presentation: :attack,presentation_changed: true)
        :attack->
          monster = monster.update(presentation: :defense_up,presentation_changed: true)
      end
      monster_zone = Dict.put(player_battle_info.monster_zone,card_index,monster)
      player_battle_info = player_battle_info.monster_zone monster_zone
      battle_data = battle_data.update([{player_atom,player_battle_info}])
    end
    presentation_change_effect = BattleCore.create_card_presentation_change_effect(monster.id,monster.presentation,player_id,:monster_zone,card_index)
    message = Proto.PT12.write(:effects,[presentation_change_effect])
    battle_data.send_message_to_all message
    {:ok,battle_data}
  end

  def flip_card(:order,_card_index,battle_data) do
    {:invalid_flip_card,battle_data,[]}
  end

end