defrecord PlayerState,id: 0,name: "",avatar: 0,gender: 0,hp: 0,win: 0,lose: 0,socket: nil,room_id: 0,battle_pid: nil,
  deck: [],
  extra_deck: [],
  side_deck: [] do
  def brief_info_binary record do
    <<record.id::32,ProtoUtil.pack_string(record.name)::binary,record.avatar::8>>
  end        
end

defrecord Card,id: 0,card_type: nil,atrribute: nil,group: nil,attack: 0,defense: 0,level: 0,skills: []

defrecord Monster,id: 0,attack: 0,defense: 0,level: 0,presentation: nil,effect_monster: false,presentation_changed: false,
attacked: false,effect_fired: false,skills: [] do
  def turn_reset(record) do
    record.update(effect_monster: false,presentation_changed: false,attacked: false,effect_fired: false)
  end
end

defrecord SpellTrap,id: 0,state: nil,skills: []

defrecord Target,player_id: 0,scene_type: 0,index: 0

defrecord Effect,type: 0,params: "",targets: nil

defrecord PlayerOnline,id: 0,player_pid: nil

defrecord RoomInfo,id: 0,status: nil,name: "",type: 1,owner_seat: 0,members_dict: nil do
  def to_binary(record) do
    room_status_id = IDUtil.room_status_id_from record.status
    members_data_binary = Enum.map_join record.members_dict,fn({seat,room_player_info}) ->
      is_owner = case record.owner_seat do
        ^seat ->
          1
        _ ->
          0
      end
      <<seat::8,room_player_info.id::32,ProtoUtil.pack_string(room_player_info.name)::binary,
        room_player_info.avatar::8,is_owner::8,IDUtil.ready_state_id_from(room_player_info.ready_state)::8>>
    end
    <<record.id::32,
    room_status_id::16,
    ProtoUtil.pack_string(record.name)::binary,
    record.type::16,
    Dict.size(record.members_dict)::16,
    members_data_binary::binary>>
  end  
end

defrecord RoomPlayerInfo,id: 0,player_pid: nil,socket: nil,name: "",avatar: 0,ready_state: :unready

defrecord BattleData,turn_count: 1,operator_id: 0,phase: :dp,player1_id: 0,player2_id: 0,
  player1_battle_info: nil,player2_battle_info: nil,normal_summoned: false,reserved: nil

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
  field_card: nil do
  def battle_info_binary record do
    handcards_binary = iolist_to_binary(Enum.map(record.handcards,fn(x)-> <<x::32>> end))
    <<
    record.maxhp::16,
    record.curhp::16,
    length(record.handcards)::16,
    handcards_binary::binary
    >>
  end 
end

defrecord Skill,type: 0,check_phase: 0,skill_effects: [],and_conditions: [],or_conditions: []

defrecord Condition,id: 0,params: ""

defrecord SkillEffect,id: 0,params: "",priority: 0



defmodule RecordHelper do
  require Lager

  def card_become_spell_trap(card_data) do
    SpellTrap[id: card_data.id, skills: card_data.skills]
  end

  def card_become_to_monster(card_data) do
    Monster[id: card_data.id,attack: card_data.attack,defense: card_data.defense,level: card_data.level,skills: card_data.skills]
  end  
end