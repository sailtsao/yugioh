defrecord PlayerState,id: 0,name: "",avatar: 0,gender: 0,hp: 0,win: 0,lose: 0,socket: nil,room_id: 0,battle_pid: nil,
  game_deck_id: 0,
  cards: HashDict.new,
  decks: HashDict.new do

  def brief_binary record do
    <<record.id::32,ProtoUtil.pack_string(record.name)::binary,record.avatar::8>>
  end

  def game_deck player_state do
    if player_state.game_deck_id == 0 do
      throw :game_deck_id_invalid
    else
      Dict.get player_state.decks,player_state.game_deck_id
    end
  end

  def game_deckcards player_state do
    Enum.flat_map player_state.game_deck.main_deck,fn({id,count})->
      List.duplicate id,count
    end
  end

end