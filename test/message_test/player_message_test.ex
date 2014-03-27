defmodule PlayerMessageTest do
  use ExUnit.Case
  test "save load data" do
    socket = TestHelper.connect
    TestHelper.enter_game_quick socket,"sail","123"
    message = TestHelper.load_data socket
    assert message == <<4::16,13000::16>>
    message = TestHelper.save_data socket
    assert message == <<4::16,13001::16>>
  end
end