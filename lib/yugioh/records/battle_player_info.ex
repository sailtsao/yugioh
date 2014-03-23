defrecord BattlePlayerInfo,
  id: 0,
  player_pid: nil,
  socket: nil,hp: 0,
  monster_zone: HashDict.new,
  spell_trap_zone: HashDict.new,
  extradeckcards: [],
  graveyardcards: [],
  deckcards: [],
  banishedcards: [],
  handcards: [],
  field_card: nil do

  @doc """
  binary of player battle info
  iex> r = BattlePlayerInfo[hp: 3000,handcards: [1,2,3,4,5]]
  ...> r.binary
  <<3000::16,5::16,1::32,2::32,3::32,4::32,5::32>>
  """
  def binary record do
    handcards_list_binary = ProtoUtil.pack_list record.handcards, &(<<&1::32>>)
    <<record.hp::16,handcards_list_binary::binary>>
  end

  @doc """
  get size of monster zone
  iex> r = BattlePlayerInfo[monster_zone: HashDict.new(HashDict.new([{1,1},{2,1},{3,1},{4,1},{5,1}]))]
  ...> r.monster_zone_size
  5
  """
  def monster_zone_size record do
    Dict.size record.monster_zone
  end

  @doc """
  get size of monster zone
  iex> r = BattlePlayerInfo[spell_trap_zone: HashDict.new([{1,1},{2,1},{3,1},{4,1},{5,1}])]
  ...> r.spell_trap_zone_size
  5
   """
  def spell_trap_zone_size record do
    Dict.size record.spell_trap_zone
  end

  @doc """
  send message to player
  """
  def send_message message_data,record do
    send record.player_pid,{:send,message_data}
  end

  @doc """
  hide handcards of player
  iex> r = BattlePlayerInfo[handcards: [1,2,3,4,5]]
  ...> r.hide_handcards.handcards
  [0,0,0,0,0]
  """
  def hide_handcards record do
    cards_size = length record.handcards
    record.handcards Enum.take(Stream.cycle([0]),cards_size)
  end

  @doc """
  is spell trap zone full?
  iex> r = BattlePlayerInfo[spell_trap_zone: HashDict.new([{1,1},{2,1},{3,1},{4,1},{5,1}])]
  ...> r.is_spell_trap_zone_full?
  true
  """
  def is_spell_trap_zone_full? record do
    Dict.size(record.spell_trap_zone) == 5
  end

  @doc """
  is monster zone full?
  iex> r = BattlePlayerInfo[monster_zone: HashDict.new([{1,1},{2,1},{3,1},{4,1},{5,1}])]
  ...> r.is_monster_zone_full?
  true
  """
  def is_monster_zone_full? record do
     Dict.size(record.monster_zone) == 5
  end

  @doc """
  get the available pos for new spell trap card
  iex> r = BattlePlayerInfo[]
  ...> r.get_spell_trap_available_pos
  2
  """
  def get_spell_trap_available_pos record do
    avaible_pos = :lists.subtract([2,1,3,0,4],Dict.keys(record.spell_trap_zone))
    hd avaible_pos
  end

  @doc """
  get the available pos for new monster card
  iex> r = BattlePlayerInfo[]
  ...> r.get_monster_available_pos
  2
  """
  def get_monster_available_pos record do
    avaible_pos = :lists.subtract([2,1,3,0,4],Dict.keys(record.monster_zone))
    hd avaible_pos
  end

  @doc """
  get cards of scene
  iex> r = BattlePlayerInfo[handcards: [1,2,3]]
  ...> r.get_cards_of_scene :handcard_zone
  [1,2,3]
  iex> r = BattlePlayerInfo[monster_zone: HashDict.new([{1,1},{2,1},{3,1},{4,1},{5,1}])]
  ...> r.get_cards_of_scene :monster_zone
  HashDict.new([{1,1},{2,1},{3,1},{4,1},{5,1}])
  """
  def get_cards_of_scene scene_type,record do
    scene_atom = IDUtil.get_scene_atom scene_type
    apply BattlePlayerInfo,scene_atom,[record]
  end

  @doc """
  get id index tuple list of scene
  iex> r = BattlePlayerInfo[handcards: [1,2,3]]
  ...> r.get_id_index_list_of_scene :handcard_zone
  [{1,0},{2,1},{3,2}]
  iex> r = BattlePlayerInfo[monster_zone: HashDict.new([{1,Monster[id: 1]},{2,Monster[id: 1]},{3,Monster[id: 1]},{4,Monster[id: 1]},{5,Monster[id: 1]}])]
  ...> result = r.get_id_index_list_of_scene(:monster_zone)
  ...> [{1,1},{1,2},{1,3},{1,4},{1,5}] == List.keysort result,1
  true
  """
  def get_id_index_list_of_scene(scene_type,record)
  when scene_type in [:handcard_zone,:graveyard_zone,:deck_zone,:extra_deck_zone,:banished_zone] do
    cards = record.get_cards_of_scene scene_type
    Enum.with_index cards
  end

  def get_id_index_list_of_scene(scene_type,record)
  when scene_type in [:monster_zone,:spell_trap_zone] do
    cards = record.get_cards_of_scene scene_type
    Enum.map cards,fn({index,card})-> {card.id,index} end
  end
end