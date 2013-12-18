defmodule Yugioh.Proto.PT11 do

  def read(11000,bin) do
    {name,rest} = Yugioh.Proto.read_string(bin)
    <<type::size(16)>> = rest
    {:ok,{:create_room,name,type}}
  end

  def read(11001,_bin) do
    {:ok,{:get_rooms}}
  end

  def read(11002,bin) do
    <<room_id::size(32)>> = bin
    {:ok,{:enter_room,room_id}}
  end

  def read(11004,bin) do
    {:ok,{:leave_room}}
  end

  def write(11000,[code,room_info])do
    data = <<code::size(16),RecordHelper.encode_room_info(room_info)::binary>>
    Yugioh.Proto.pack(11000,data)
  end

  def write(11001,data)do
    Yugioh.Proto.pack(11001,data)
  end

  def write(11002,[code,room_info])do
    data = <<code::size(16),RecordHelper.encode_room_info(room_info)::binary>>
    Yugioh.Proto.pack(11002,data)
  end

  def write(11003,[seat,player_state])do
    data = <<seat::size(8),RecordHelper.encode_player_brief_info(player_state)::binary>>
    Yugioh.Proto.pack(11003,data)
  end

  def write(11004,code)do
    data = <<code::size(16)>>
    Yugioh.Proto.pack(11004,data)
  end

  def write(11005,room_info)do
    data = <<RecordHelper.encode_room_info(room_info)::binary>>
    Yugioh.Proto.pack(11005,data)
  end
end