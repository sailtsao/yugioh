ExUnit.start
defmodule TestHelper do
  def connect ip_address\\'localhost',port\\1234 do
    {:ok,socket} = :gen_tcp.connect(ip_address,port,[:binary,{:packet,0},{:active,false},{:reuseaddr,true}])
    socket
  end

  def normal_login socket,user_name\\"sail",password\\"123" do
    message_data = ProtoUtil.pack(10000,<<ProtoUtil.pack_string(user_name)::binary,ProtoUtil.pack_string(password)::binary>>)
    send_and_receive(message_data,socket)
  end

  def web_login socket,user_id\\1,auth_string\\"123456" do
    message_data = ProtoUtil.pack(10007,<<user_id::32,ProtoUtil.pack_string(auth_string)::binary>>)
    send_and_receive(message_data,socket)
  end

  def get_roles socket do
    message_data = ProtoUtil.pack(10004,<<>>)
    send_and_receive(message_data,socket)
  end

  def enter_game socket,player_id do
    message_data = ProtoUtil.pack(10005,<<player_id::32>>)
    send_and_receive(message_data,socket)
  end
  
  def enter_game_quick socket,user_name,password do
    normal_login socket,user_name,password
    <<_::16,10004::16,1::16,player_id::32,_rest::binary>> = TestHelper.get_roles socket
    enter_game socket,player_id
  end
  
  def create_room socket do
    message_data = ProtoUtil.pack(11000,<<>>)
    send_and_receive(message_data,socket)
  end
  
  def get_rooms socket do
    message_data = ProtoUtil.pack(11001,<<>>)
    send_and_receive(message_data,socket)
  end

  def enter_room socket,room_id do
    message_data = ProtoUtil.pack(11002,<<room_id::32>>)
    send_and_receive(message_data,socket)
  end

  def battle_ready(socket) do
    message_data = ProtoUtil.pack(11006,<<>>)
    send_and_receive(message_data,socket)
  end

  def battle_start(socket) do
    message_data = ProtoUtil.pack(11007,<<>>)
    send_and_receive(message_data,socket)
  end    

  def get_message socket do
    {:ok,data} = :gen_tcp.recv(socket,0)
    data
  end
    
  defp send_and_receive message_data,socket do
    :gen_tcp.send(socket,message_data)
    {:ok,data}=:gen_tcp.recv(socket,0)
    data
  end
end
