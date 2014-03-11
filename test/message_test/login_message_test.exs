defmodule LoginMessageTest do
  use ExUnit.Case

  test "normal login" do
    socket = TestHelper.connect 
    data = TestHelper.normal_login socket
    assert data == ProtoUtil.pack(10000,<<1::16>>)
  end

  test "web login" do
    socket = TestHelper.connect 
    data = TestHelper.web_login socket
    assert data == ProtoUtil.pack(10000,<<1::16>>)
  end

  test "get roles" do
    socket = TestHelper.connect
    _data = TestHelper.normal_login socket
    data = TestHelper.get_roles socket
    player_id = 6
    assert data == ProtoUtil.pack(10004,<<1::16,player_id::32,ProtoUtil.pack_string("sail")::binary,2::8>>)    
  end

  test "enter game" do
    socket = TestHelper.connect
    _data = TestHelper.normal_login socket
    _data = TestHelper.get_roles socket
    data = TestHelper.enter_game socket,6
    assert <<_::16, 10005::16, 6::32 ,2::8, 4::16,"sail",3000::32,0::32,0::32,
      _rest::binary>> = data
  end

  test "enter game quick" do
    socket = TestHelper.connect
    data = TestHelper.enter_game_quick socket,"sail","123"    
    assert <<_::16, 10005::16, 6::32 ,2::8, 4::16,"sail",3000::32,0::32,0::32,
      _rest::binary>> = data
  end
  # test "create room" do
  #   socket = TestHelper.connect
  #   _data = TestHelper.normal_login socket
  #   _data = TestHelper.get_roles socket
  #   _data = TestHelper.enter_game socket,6
  # end
end