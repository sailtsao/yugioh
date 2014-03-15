defmodule SpecialSummonTest do
  use ExUnit.Case

  def choose_card socket,<<_::16,12008::16,_::8,_::8,_::16,bin::binary>> do
    {player_id,choose_scene,choose_index} = TestHelper.get_tribute_params bin
    data = TestHelper.choose_card(socket,player_id,choose_scene,choose_index)
  end
  def choose_card _,_ do
    :ok
  end
  
  def special_summon socket,operations_list,index,socket2 do
    if 8 in operations_list do
      data = TestHelper.summon socket,index,1,1
      # <<_::16,12008::16,choose_type::8,choose_number::8,choose_scene_length::16,choose_scene::binary>> = data
      # <<player_id::32,scene_type_id::8,choose_index_length::16,choose_id::32,choose_index::8>> = choose_scene
      # data = TestHelper.choose_card(socket,6,7,2)
      choose_card socket,data
      :timer.sleep 100
      data = TestHelper.get_message(socket2)
      # data = TestHelper.get_cards_of_scene_type socket,player_id,3
      # assert <<_::16,12010::16,player_id::32,3::8,1::16,_::32>> = data 
    end
  end
  
  test "special summon battle test" do
    {socket1,socket2} = TestHelper.battle_start_quick "sail","123","xqy","123"
    TestHelper.battle_load_finish(socket1)
    TestHelper.battle_load_finish(socket2)
    :timer.sleep 100
    data = TestHelper.get_message(socket1)
    data = TestHelper.get_message(socket2)

    operations_list = TestHelper.get_card_operations(socket1,7,0)
    IO.inspect operations_list
    special_summon socket1,operations_list,0,socket2
    operations_list = TestHelper.get_card_operations(socket1,7,1)
    IO.inspect operations_list
    special_summon socket1,operations_list,1,socket2
    operations_list = TestHelper.get_card_operations(socket1,7,2)
    IO.inspect operations_list
    special_summon socket1,operations_list,2,socket2
    operations_list = TestHelper.get_card_operations(socket1,7,3)
    IO.inspect operations_list
    special_summon socket1,operations_list,3,socket2
    # operations_list = TestHelper.get_card_operations(socket1,7,4)
    # IO.inspect operations_list
    # special_summon socket1,operations_list,4

    receive do
      _->
        :ok
    end
  end
end 