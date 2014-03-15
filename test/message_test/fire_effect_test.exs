defmodule FireEffectTest do
  use ExUnit.Case
  def fire_effect socket,operations_list,index do
    if 3 in operations_list do
      data = TestHelper.fire_effect socket,7,index
      # <<0,26,46,233,0,1,0,0,0,2,0,6,48,59,49,49,59,49,0,1,0,0,0,6,2,2,_::16,12008::16,choose_type::8,choose_number::8,choose_scene_length::16,choose_scene::binary>> = data
      # # <<>> = data
      # <<player_id::32,scene_type_id::8,choose_index_length::16,choose_id::32,choose_index::8>> = choose_scene
      data = TestHelper.choose_card(socket,8,2,2)
    end
  end
  
  test "special summon battle test" do
    {socket1,socket2} = TestHelper.battle_start_quick "sail","123","xqy","123"
    TestHelper.battle_load_finish(socket1)
    TestHelper.battle_load_finish(socket2)
    data = TestHelper.get_message(socket1)
    data = TestHelper.get_message(socket2)

    operations_list = TestHelper.get_card_operations(socket1,7,0)
    IO.inspect operations_list
    fire_effect socket1,operations_list,0

    # operations_list = TestHelper.get_card_operations(socket1,7,0)
    # IO.inspect operations_list
    # fire_effect socket1,operations_list,0
    # operations_list = TestHelper.get_card_operations(socket1,7,1)
    # IO.inspect operations_list
    # fire_effect socket1,operations_list,1
    # operations_list = TestHelper.get_card_operations(socket1,7,2)
    # IO.inspect operations_list
    # fire_effect socket1,operations_list,2

    receive do
      _->
        :ok
    end
  end

end