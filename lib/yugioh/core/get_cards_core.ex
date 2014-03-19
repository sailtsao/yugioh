defmodule GetCardsCore do

  def get_cards_of_scene(player_id,target_player_id,scene_type,battle_data)
  when scene_type in [:graveyard_zone,:extra_deck_zone,:banished_zone,:deck_zone] do
    target_player_battle_info = battle_data.get_player_battle_info target_player_id
    cards = Util.get_cards_from_scene target_player_battle_info,scene_type
    message = Proto.PT12.write(:get_cards_of_scene_type,[target_player_id,scene_type,cards])
    player_battle_info = battle_data.get_player_battle_info player_id
    player_battle_info.send_message message
    {:ok,battle_data}
  end

  def get_cards_of_scene _,_,_ do
    {:invalid_get_cards_of_scene_type,[]}
  end

end