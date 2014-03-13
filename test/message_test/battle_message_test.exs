defmodule BattleMessageTest do
  use ExUnit.Case
  test "battle test" do
    {socket1,socket2} = TestHelper.battle_start_quick "sail","123","xqy","123"
    TestHelper.battle_load_finish(socket1)
    TestHelper.battle_load_finish(socket2)
    data = TestHelper.get_message(socket1)
    data = TestHelper.get_message(socket2)

    operations_list = TestHelper.get_card_operations(socket1,7,0)
    IO.inspect operations_list
    if 1 in operations_list do
      data = TestHelper.summon socket1,0,1,0
    end
    operations_list = TestHelper.get_card_operations(socket1,7,1)
    IO.inspect operations_list
    if 1 in operations_list do
      data = TestHelper.summon socket1,1,1,0
    end
    operations_list = TestHelper.get_card_operations(socket1,7,2)
    IO.inspect operations_list
    if 1 in operations_list do
      data = TestHelper.summon socket1,2,1,0
    end
    operations_list = TestHelper.get_card_operations(socket1,7,3)
    IO.inspect operations_list
    if 1 in operations_list do
      data = TestHelper.summon socket1,3,1,0
    end
    operations_list = TestHelper.get_card_operations(socket1,7,4)
    IO.inspect operations_list
    if 1 in operations_list do
      data = TestHelper.summon socket1,4,1,0
    end

    receive do
      _->
        :ok
    end
  end
end 