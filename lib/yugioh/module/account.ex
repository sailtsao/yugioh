defmodule Yugioh.Module.Account do
  def login([acc,pwd],socket) do
    # todo:get account id from database
    :gen_tcp.send(socket,Yugioh.Proto.PT10.write(10000,1))
    {:ok,1}
    # {:fail}
  end
end