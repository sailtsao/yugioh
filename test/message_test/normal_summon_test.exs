defmodule BattleTest do
  use ExUnit.Case
  def tribute socket,<<_::16,12008::16,_::8,_::8,_::16,bin::binary>> do
    {player_id,choose_scene,choose_index} = TestHelper.get_tribute_params bin
    data = TestHelper.choose_card(socket,player_id,choose_scene,choose_index)
  end
  def tribute _,_ do
    :ok
  end
  
  
  test "battle test" do
    {socket1,socket2} = TestHelper.battle_start_quick "sail","123","xqy","123"
    TestHelper.battle_load_finish(socket1)
    TestHelper.battle_load_finish(socket2)
    :timer.sleep 100
    TestHelper.get_message(socket1)
    TestHelper.get_message(socket2)

    operations_list = TestHelper.get_card_operations(socket1,7,0)
    IO.inspect operations_list
    if 1 in operations_list do
      data = TestHelper.summon socket1,0,1,0
      tribute socket1,data
    end
    operations_list = TestHelper.get_card_operations(socket1,7,1)
    IO.inspect operations_list
    if 1 in operations_list do
      data = TestHelper.summon socket1,1,1,0
      tribute socket1,data
    end
    operations_list = TestHelper.get_card_operations(socket1,7,2)
    IO.inspect operations_list
    if 1 in operations_list do
      data = TestHelper.summon socket1,2,1,0
      tribute socket1,data
    end
    operations_list = TestHelper.get_card_operations(socket1,7,3)
    IO.inspect operations_list
    if 1 in operations_list do
      data = TestHelper.summon socket1,3,1,0
      tribute socket1,data
    end
    operations_list = TestHelper.get_card_operations(socket1,7,4)
    IO.inspect operations_list
    if 1 in operations_list do
      data = TestHelper.summon socket1,4,1,0
      tribute socket1,data
    end

    receive do
    after 
      100->
    end
  end
end 