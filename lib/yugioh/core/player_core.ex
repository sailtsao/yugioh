defmodule PlayerCore do
  def load_data player_state,[] do
    message = Proto.PT13.write(:load_data,[player_state])
    send self,{:send,message}
    {:ok,player_state}
  end

  def save_data player_state,[deck_id,main_deck_dict,extra_deck_dict,side_deck_dict] do
    deck = Dict.get player_state.decks,deck_id
    deck = deck.update(main_deck: main_deck_dict,extra_deck: extra_deck_dict,side_deck: side_deck_dict)
    decks = Dict.put player_state.decks,deck_id,deck
    player_state = player_state.decks decks
    role = Repo.get(Model.Role,player_state.id)
    decks_binary = Ecto.Binary[value: :erlang.term_to_binary(decks)]
    role = role.update([decks: decks_binary])
    Repo.update role
    message = Proto.PT13.write(:save_data,[1])
    send self,{:send,message}
    {:ok,player_state}
  end
end