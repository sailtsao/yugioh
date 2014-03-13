defmodule LeaveRoomMessageTest do
  use ExUnit.Case
  test "leave room test" do
    {socket1,socket2} = TestHelper.room_ready_quick "sail","123","xqy","123"
    # :gen_tcp.recv(socket2,0)
    # :gen_tcp.recv(socket2,0)
    room_id = 1
    data = TestHelper.leave_room(socket1)
    
    data = TestHelper.enter_room(socket1,room_id)

    data = TestHelper.leave_room(socket2)

    data = TestHelper.enter_room(socket2,room_id)

    data = TestHelper.leave_room(socket1)

    data = TestHelper.enter_room(socket1,room_id)

    data = TestHelper.leave_room(socket2)

    data = TestHelper.enter_room(socket2,room_id)
    # assert data == <<6::16,11004::16,1::16>>
    # data = TestHelper.get_message(socket2)
    # assert <<34::16, 11005::16, 1::32, 1::16, _::56, 1::16, 1::16, 2::8, 8::32, _::40, 1::8, 1::8, 1::8>> = data
    receive do
      _->
    end
  end

end