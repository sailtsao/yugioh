defmodule SpecialSummonTest do
  use ExUnit.Case

  def special_summon socket,operations_list,index do
    if 8 in operations_list do
      data = TestHelper.summon socket,index,1,1
      # <<_::16,12008::16,choose_type::8,choose_number::8,choose_scene_length::16,choose_scene::binary>> = data
      # <<player_id::32,scene_type_id::8,choose_index_length::16,choose_id::32,choose_index::8>> = choose_scene
      data = TestHelper.choose_card(socket,6,7,2)
      
      # data = TestHelper.get_cards_of_scene_type socket,player_id,3
      # assert <<_::16,12010::16,player_id::32,3::8,1::16,_::32>> = data 
    end
  end
  
  test "special summon battle test" do
    {socket1,socket2} = TestHelper.battle_start_quick "sail","123","xqy","123"
    TestHelper.battle_load_finish(socket1)
    TestHelper.battle_load_finish(socket2)
    data = TestHelper.get_message(socket1)
    data = TestHelper.get_message(socket2)

    operations_list = TestHelper.get_card_operations(socket1,7,4)
    IO.inspect operations_list
    special_summon socket1,operations_list,4
    # operations_list = TestHelper.get_card_operations(socket1,7,1)
    # IO.inspect operations_list
    # special_summon socket1,operations_list,1
    # operations_list = TestHelper.get_card_operations(socket1,7,2)
    # IO.inspect operations_list
    # special_summon socket1,operations_list,2
    # operations_list = TestHelper.get_card_operations(socket1,7,3)
    # IO.inspect operations_list
    # special_summon socket1,operations_list,3
    # operations_list = TestHelper.get_card_operations(socket1,7,4)
    # IO.inspect operations_list
    # special_summon socket1,operations_list,4

    receive do
      _->
        :ok
    end
  end
end 