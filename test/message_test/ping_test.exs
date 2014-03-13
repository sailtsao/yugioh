defmodule PingTest do
  use ExUnit.Case
  test "ping" do
    socket = TestHelper.connect 
    data = TestHelper.ping socket
    <<12::16,9999::16,time::64>> = data
    IO.inspect time
    receive do
      _->
    end
  end
end