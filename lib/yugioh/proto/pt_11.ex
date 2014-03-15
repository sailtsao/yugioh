defmodule Proto.PT11 do

  def read(11000,_) do
    {:ok,{:create_room,[]}}
  end

  def read(11001,_) do
    {:ok,{:get_rooms,[]}}
  end

  def read(11002,<<room_id::32>>) do
    {:ok,{:enter_room,[room_id]}}
  end

  def read(11004,_) do
    {:ok,{:leave_room,[]}}
  end

  def read(11005,_) do
    {:ok,{:refresh_roominfo,[]}}
  end

  def read(11006,_) do
    {:ok,{:battle_ready,[]}}
  end

  def read(11007,_) do
    {:ok,{:battle_start,[]}}
  end

  def write(:create_room,[code,room_info])do    
    data = <<code::16,
    room_info.to_binary::binary
    >>
    ProtoUtil.pack(11000,data)
  end

  def write(:get_rooms,[rooms_list])do
    data = ProtoUtil.pack_list rooms_list,fn(room_info)-> 
      <<
      room_info.id::32,
      ProtoUtil.pack_string(room_info.name)::binary,
      room_info.type::16
      >> 
    end        
    ProtoUtil.pack(11001,data)
  end

  def write(:enter_room,[code,room_info])do
    data = <<code::16,room_info.to_binary::binary>>
    ProtoUtil.pack(11002,data)
  end

  def write(:new_members,[seat,room_player_info])do
    data = <<seat::8,room_player_info.id::32,ProtoUtil.pack_string(room_player_info.name)::binary,room_player_info.avatar::8>>
    ProtoUtil.pack(11003,data)
  end

  def write(:leave_room,[code])do
    data = <<code::16>>
    ProtoUtil.pack(11004,data)
  end

  def write(:refresh_roominfo,[room_info])do
    data = room_info.to_binary
    ProtoUtil.pack(11005,data)
  end

  def write(:refresh_ready_state,[seat,ready_state]) do
    data = <<seat::8,IDUtil.ready_state_id_from(ready_state)::8>>
    ProtoUtil.pack(11006,data)
  end
  
  def write(:battle_start,[code,cur_player_id,phase,player_state1,battle_info1,player_state2,battle_info2]) do
    phase_number = case phase do
      :dp->
        1
      :sp->
        2
      :mp1->
        3
      :bp->
        4
      :mp2->
        5
    end
    data = <<code::16,cur_player_id::32,phase_number::8,
    player_state1.brief_info_binary::binary,
    battle_info1.battle_player_info_binary::binary,
    player_state2.brief_info_binary::binary,
    battle_info2.battle_player_info_binary::binary>>
    ProtoUtil.pack(11007,data)
  end
end