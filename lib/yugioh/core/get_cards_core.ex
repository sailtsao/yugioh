defmodule GetCardsCore do

  def get_cards_of_scene(player_id,target_player_id,scene_type,battle_data)
  when scene_type in [:graveyard_zone,:extra_deck_zone,:banished_zone,:deck_zone] do
    target_player_battle_info = battle_data.get_player_battle_info target_player_id
    cards = target_player_battle_info.get_cards_of_scene scene_type
    message = Proto.PT12.write(:get_cards_of_scene_type,[target_player_id,scene_type,cards])
    battle_data.send_message player_id,message
    {:ok,battle_data}
  end

  def get_cards_of_scene _,_,_,battle_data do
    {:invalid_get_cards_of_scene_type,battle_data}
  end

end
