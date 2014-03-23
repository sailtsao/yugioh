defmodule FlipCardCore do
  def flip_card(player_id,card_index,battle_data = BattleData[phase: phase])
  when phase in [:mp1,:mp2] do
    result = :ok

    player_atom = battle_data.get_player_atom player_id
    player_battle_info = battle_data.get_player_battle_info player_id
    monster = Dict.get(player_battle_info.monster_zone,card_index)
    if monster.presentation_changed do
      result = :already_changed_presentation
    end

    if result == :ok do
      monster = monster.change_presentation
      monster_zone = Dict.put(player_battle_info.monster_zone,card_index,monster)
      player_battle_info = player_battle_info.monster_zone monster_zone
      battle_data = battle_data.update([{player_atom,player_battle_info}])
    end
    presentation_change_effect = BattleCore.create_card_presentation_change_effect(monster.id,monster.presentation,player_id,:monster_zone,card_index)
    message = Proto.PT12.write(:effects,[presentation_change_effect])
    battle_data.send_message_to_all message
    {result,battle_data}
  end

  def flip_card(_,_,battle_data) do
    {:invalid_flip_card,battle_data,[]}
  end
end