defrecord BattlePlayerInfo,
  player_pid: nil,
  socket: nil,hp: 0,
  monster_card_zone: HashDict.new,
  spell_trap_zone: HashDict.new,
  extradeckcards: [],
  graveyardcards: [],
  deckcards: [],
  banishedcards: [],
  handcards: [],
  field_card: nil do

  # remove one of the hp from this structure
  def battle_player_info_binary record do
    handcards_list_binary = ProtoUtil.pack_list record.handcards, &(<<&1::32>>)
    <<record.hp::16,handcards_list_binary::binary>>
  end 

  def monster_summoned_amount record do
    Dict.size record.monster_card_zone
  end
  
  def spell_trap_summoned_amount record do
    Dict.size record.spell_trap_zone
  end
  
  def send_message message_data,record do
    send record.player_pid,{:send,message_data}
  end

  def hide_handcards record do
    cards_size = length record.handcards
    record.handcards Enum.take(Stream.cycle([0]),cards_size)
  end
  
  def is_spell_trap_zone_full? record do
    record.spell_trap_summoned_amount == 5
  end
   
  def is_monster_card_zone_full? record do
     record.monster_summoned_amount == 5
  end

  def get_spell_trap_available_pos record do
    avaible_pos = :lists.subtract([2,1,3,0,4],Dict.keys(record.spell_trap_zone))
    hd avaible_pos
  end
  
  def get_monster_available_pos record do
    avaible_pos = :lists.subtract([2,1,3,0,4],Dict.keys(record.monster_card_zone))
    hd avaible_pos
  end
  
end