defrecord PlayerState,id: 0,name: "",avatar: 0,gender: 0,hp: 0,win: 0,lose: 0,socket: nil,room_id: 0,battle_pid: nil,
  deck: [],
  extra_deck: [],
  side_deck: []

defrecord Card,id: 0,card_type: nil,atrribute: nil,group: nil,attack: 0,defense: 0,level: 0,skills: []

defrecord Monster,id: 0,attack: 0,defense: 0,level: 0,presentation: nil,effect_monster: false,presentation_changed: false,
attacked: false,effect_fired: false,skills: [] do
  def turn_reset(record) do
    record.update(effect_monster: false,presentation_changed: false,attacked: false,effect_fired: false)
  end
end

defrecord Target,player_id: 0,scene_type: 0,index: 0

defrecord Effect,type: 0,params: "",targets: nil

defrecord PlayerOnline,id: 0,player_pid: nil

defrecord RoomInfo,id: 0,status: nil,name: "",type: 1,owner_seat: 0,members_dict: nil

defrecord RoomPlayerInfo,id: 0,player_pid: nil,socket: nil,name: "",avatar: 0,ready_state: :unready

defrecord BattleInfo,
  player_pid: nil,
  socket: nil,
  maxhp: 0,curhp: 0,
  monster_card_zone: HashDict.new,
  spell_trap_zone: HashDict.new,
  extradeckcards: [],
  graveyardcards: [],
  deckcards: [],
  banishedcards: [],
  handcards: [],
  field_card: nil

defrecord Skill,type: 0,check_phase: 0,skill_effects: [],and_conditions: [],or_conditions: []

defrecord Condition,id: 0,params: ""

defrecord SkillEffect,id: 0,params: "",priority: 0

defrecord BattleData,turn_count: 1,operator_id: 0,phase: :dp,player1_id: 0,player2_id: 0,
  player1_battle_info: nil,player2_battle_info: nil,normal_summoned: false

defmodule RecordHelper do
  require Lager
  alias Yugioh.Proto

  def card_become_to_monster(card_data) do
    Monster[id: card_data.id,attack: card_data.attack,defense: card_data.defense,level: card_data.level,skills: card_data.skills]
  end
  
  
  def encode_player_brief_info(player_state) do
    <<
    player_state.id::size(32),
    ProtoUtil.pack_string(player_state.name)::binary,
    player_state.avatar::size(8)
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