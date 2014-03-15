defmodule RoomTest do
  use ExUnit.Case
  test "room test" do
    socket = TestHelper.connect
    TestHelper.enter_game_quick socket,"sail","123"

    socket1 = TestHelper.connect
    TestHelper.enter_game_quick socket1,"xqy","123"

    data = TestHelper.create_room(socket)    

    room_id = 1
    room_status_id = 1
    room_name = "Room1"
    room_type = 1
    seat = 1
    seat1 = 2
    player_id = 6
    player1_id = 8
    player_name = "sail"
    player1_name = "xqy"
    avatar = 2
    avatar1 = 1
    is_owner = 1
    is_owner1 = 0
    ready_state = 1
    ready_state1 = 0

    assert data == <<37::16, 11000::16, 1::16, room_id::32, room_status_id::16, ProtoUtil.pack_string(room_name)::binary, room_type::16,
      1::16, seat::8, player_id::32, ProtoUtil.pack_string(player_name)::binary, avatar::8, is_owner::8, ready_state::8>>


    data = TestHelper.get_rooms(socket1)
    assert data ==  <<19::16, 11001::16, 1::16, room_id::32, ProtoUtil.pack_string(room_name)::binary, room_type::16>>

    
    data = TestHelper.enter_room(socket1,room_id)
    assert data == <<50::16, 11002::16, 1::16, room_id::32, room_status_id::16, ProtoUtil.pack_string(room_name)::binary, room_type::16,
      2::16, seat1::8, player1_id::32, ProtoUtil.pack_string(player1_name)::binary, avatar1::8, is_owner1::8, ready_state1::8, 
      seat::8, player_id::32, ProtoUtil.pack_string(player_name)::binary, avatar::8, is_owner::8, ready_state::8>>

    # player receive player1 enter room new members notice message
    data = TestHelper.get_message(socket)
    assert data == <<15::16, 11003::16, seat1::8, player1_id::32, ProtoUtil.pack_string(player1_name)::binary, avatar1::8>>


    data = TestHelper.battle_ready(socket1)
    assert data == <<6::16,11006::16,seat1::8,1::8>>
    
    # player receive player1 battle ready notice message
    data = TestHelper.get_message(socket)
    assert data == <<6::16,11006::16,seat1::8,1::8>>

    ready_state1 = 1
    data = TestHelper.refresh_roominfo(socket)
    assert data == <<48::16, 11005::16, room_id::32, room_status_id::16, ProtoUtil.pack_string(room_name)::binary, room_type::16,
      2::16, seat1::8, player1_id::32, ProtoUtil.pack_string(player1_name)::binary, avatar1::8, is_owner1::8, ready_state1::8, 
      seat::8, player_id::32, ProtoUtil.pack_string(player_name)::binary, avatar::8, is_owner::8, ready_state::8>>


    data = TestHelper.battle_start(socket)
    assert <<80::size(16),11007::size(16),1::size(16),6::size(32),1::size(8),
    6::size(32),4::size(16),"sail",2::size(8),3000::size(16),5::size(16),_card_1::size(160),
    8::size(32),3::size(16),"xqy",1::size(8),3000::size(16),5::size(16),_card_2::size(160)>> = data

    # player1 receive player battle start notice message
    data = TestHelper.get_message(socket1)
    assert <<80::size(16),11007::size(16),1::size(16),6::size(32),1::size(8),
    6::size(32),4::size(16),"sail",2::size(8),3000::size(16),5::size(16),_card_1::size(160),
    8::size(32),3::size(16),"xqy",1::size(8),3000::size(16),5::size(16),_card_2::size(160)>> = data 
    
  end
  
  # test room owner battle ready
end