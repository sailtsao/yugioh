defrecord PlayerState,id: 0,name: "",avatar: 0,gender: 0,hp: 0,win: 0,lose: 0,socket: nil,in_room_id: 0,battle_pid: nil,
  deck: [],
  extra_deck: [],
  side_deck: []

defrecord Card,id: 0,attack: 0,defense: 0,level: 0

defrecord Monster,id: 0,attack: 0,defense: 0,level: 0,presentation: nil,effect_monster: false,presentation_changed: false,attacked: false do
  def turn_reset(record) do
    record.update(effect_monster: false,presentation_changed: false,attacked: false)
  end
end

defrecord PlayerOnline,id: 0,player_pid: nil

defrecord RoomInfo,id: 0,status: nil,name: "",type: 0,owner_pid: nil,members: nil

defrecord BattleInfo,
  player_pid: nil,
  socket: nil,
  maxhp: 0,curhp: 0,
  monster_card_zone: HashDict.new,
  spell_trap_zone: HashDict.new,
  extra_deck_zone: [],
  graveyardcards: [],
  deckcards: [],
  banishedcards: [],
  handcards: [],
  field_card: nil


defrecord BattleData,turn_count: 1,operator_id: 0,phase: :dp,player1_id: 0,player2_id: 0,
  player1_battle_info: nil,player2_battle_info: nil,normal_summoned: false

defmodule RecordHelper do
  require Lager
  alias Yugioh.Proto
  alias Yugioh.Player

  
  def encode_player_brief_info(player_state) do
    <<
    player_state.id::size(32),
    Proto.pack_string(player_state.name)::binary,
    player_state.avatar::size(8)
    >>
  end

  def encode_room_info(room_info) do
    status = case room_info.status do
      :wait ->
        1
      :battle ->
        2
    end

    members_list = Dict.to_list(room_info.members)
    members_data_list = Enum.map members_list,fn({seat,{player_pid,ready_state}}) ->
      owner_pid = room_info.owner_pid
      is_owner = case player_pid do
        ^owner_pid ->
          1
        _other ->
          0
      end
      is_ready = case ready_state do
        :ready->
          1
        :unready->
          0
      end
      player_state = Player.player_state(player_pid)
      brief_info = encode_player_brief_info(player_state)
      <<seat::size(8),brief_info::binary,is_owner::size(8),is_ready::size(8)>>
    end

    members_data_binary = iolist_to_binary(members_data_list)

    <<
    room_info.id::size(32),
    status::size(16),
    Proto.pack_string(room_info.name)::binary,
    room_info.type::size(16),
    length(members_list)::size(16),
    members_data_binary::binary
    >>
  end

  def encode_battle_info(battle_info) do
    handcards_binary = iolist_to_binary(Enum.map(battle_info.handcards,fn(x)-> <<x::size(32)>> end))
    <<
    battle_info.maxhp::size(16),
    battle_info.curhp::size(16),
    length(battle_info.handcards)::size(16),
    handcards_binary::binary
    >>
  end  
end