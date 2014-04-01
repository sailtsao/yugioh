defmodule PlayerMessageTest do
  use ExUnit.Case
  test "save load data" do
    socket = TestHelper.connect
    TestHelper.enter_game_quick socket,"sail","123"
    message = TestHelper.load_data socket
    assert message == <<0, 157, 50, 200, 0, 1, 0, 14, 0, 0, 0, 7, 3, 0, 0, 0, 13, 3, 0, 0, 0, 2, 3, 0, 0, 0, 6, 3, 0, 0, 0, 3, 3, 0, 0, 0, 4, 3, 0, 0, 0, 1, 3, 0, 0, 0, 5, 3, 0, 0, 0, 9, 3, 0, 0, 0, 11, 3, 0, 0, 0, 10, 3, 0, 0, 0, 12, 3, 0, 0, 0, 14, 3, 0, 0, 0, 8, 3, 0, 1, 0, 1, 0, 7, 100, 101, 102, 97, 117, 108, 116, 0, 60, 55, 44, 51, 59, 49, 51, 44, 51, 59, 50, 44, 51, 59, 54, 44, 51, 59, 51, 44, 51, 59, 52, 44, 51, 59, 49, 44, 51, 59, 53, 44, 51, 59, 57, 44, 51, 59, 49, 49, 44, 51, 59, 49, 48, 44, 51, 59, 49, 50, 44, 51, 59, 49, 52, 44, 51, 59, 56, 44, 51, 0, 0, 0, 0>>
    message = TestHelper.save_data socket,<<1::16,
    ProtoUtil.pack_string("7,3;13,3;2,3;6,3;3,3;4,3;1,3;5,3;9,3;11,3;10,3;12,3;14,3;8,3")::binary,
    ProtoUtil.pack_string("")::binary,
    ProtoUtil.pack_string("")::binary>>
    assert message == <<6::16,13001::16,1::16>>
  end
end