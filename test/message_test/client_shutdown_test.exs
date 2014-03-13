defmodule ClientShutdownTest do
  use ExUnit.Case
  test "client shutdown test" do
    {socket1,socket2} = TestHelper.room_ready_quick "sail","123","xqy","123"
    :gen_tcp.close socket1
    :gen_tcp.close socket2
    receive do
      _->
    end
  end
end