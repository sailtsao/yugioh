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

  def leave_room(socket) do
    message_data = ProtoUtil.pack(11004,<<>>)
    send_and_receive(message_data,socket)
  end

  def refresh_roominfo(socket) do
    message_data = ProtoUtil.pack(11005,<<>>)
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

  def room_ready_quick(player1_username,player1_password,player2_username,player2_password) do
    socket = TestHelper.connect
    TestHelper.enter_game_quick socket,player1_username,player1_password
    socket1 = TestHelper.connect
    TestHelper.enter_game_quick socket1,player2_username,player2_password

    TestHelper.create_room(socket)    
    TestHelper.enter_room(socket1,1)
    TestHelper.get_message(socket)
    TestHelper.battle_ready(socket1)
    TestHelper.get_message(socket)
    {socket,socket1}
  end
  

  def battle_start_quick(player1_username,player1_password,player2_username,player2_password) do
    socket = TestHelper.connect
    TestHelper.enter_game_quick socket,player1_username,player1_password
    socket1 = TestHelper.connect
    TestHelper.enter_game_quick socket1,player2_username,player2_password

    TestHelper.create_room(socket)    
    TestHelper.enter_room(socket1,1)
    get_message socket
    TestHelper.battle_ready(socket1)
    get_message socket
    TestHelper.battle_start(socket)
    get_message socket1
    {socket,socket1}
  end

  def get_message socket do
    {:ok,data} = :gen_tcp.recv(socket,0)
    data
  end
    
  defp send_and_receive message_data,socket do
    :gen_tcp.send(socket,message_data)
    {:ok,data} = :gen_tcp.recv(socket,0)
    data
  end

  def battle_load_finish(socket) do
    message_data = ProtoUtil.pack(12006,<<>>)
    :gen_tcp.send(socket,message_data)
  end
  
  def get_card_operations(socket,scene_type_id,index) do
    message_data = ProtoUtil.pack(12007,<<scene_type_id::8,index::8>>)
    data = send_and_receive message_data,socket
    <<_::16,12007::16,len::16,rest::binary>> = data
    {_,operation_list} = List.foldl List.duplicate(1,len),{rest,[]},fn(_,{rest,acc})->
      <<operation_id::8,rest::binary>> =  rest
      {rest,acc++[operation_id]}
    end
    operation_list
  end

  def change_phase_to(socket,phase_number) do
    message_data = ProtoUtil.pack(12000,<<phase_number::8>>)
    send_and_receive message_data,socket
  end

  def summon(socket,handcards_index,presentation_id,summon_type_id) do
    message_data = ProtoUtil.pack(12001,<<handcards_index::8,presentation_id::8,summon_type_id::8>>)
    send_and_receive message_data,socket
  end

  def fire_effect(socket,scene_type_id,index) do
    message_data = ProtoUtil.pack(12011,<<scene_type_id::8,index::8>>)
    :timer.sleep 1000
    send_and_receive message_data,socket
  end  
  
  def choose_card socket,player_id,scene_type_id,index do
    message_data = ProtoUtil.pack(12008,<<1::16,player_id::32,scene_type_id::8,1::16,index::8>>)
    send_and_receive message_data,socket
  end
  
  def get_cards_of_scene_type socket,player_id,scene_type_id do
    message_data = ProtoUtil.pack(12010,<<player_id::32,scene_type_id::8>>)
    send_and_receive message_data,socket
  end

  def ping socket do
    message_data = ProtoUtil.pack(9999,<<>>)
    send_and_receive message_data,socket
  end  
end
