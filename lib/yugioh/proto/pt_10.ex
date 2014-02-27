defmodule Yugioh.Proto.PT10 do
  def read(10000,bin) do
    {account,rest} = Yugioh.Proto.read_string(bin)
    {password,_} = Yugioh.Proto.read_string(rest)
    {:ok,:login,[account,password]}
  end

  def read(10001,bin) do
    {name,_} = Yugioh.Proto.read_string(bin)
    {:ok,:check_role_name,name}
  end

  def read(10002,bin) do
    {name,rest} = Yugioh.Proto.read_string(bin)
    <<avatar::size(8),card_type::size(8)>> = rest
    {:ok,:create_role,[name,avatar,card_type]}
  end

  def read(10003,bin) do
    {name,_} = Yugioh.Proto.read_string(bin)
    {:ok,:delete_role,name}
  end

  def read(10004,_bin) do
    {:ok,:get_roles,[]}
  end

  def read(10005,bin) do
    <<role_id::size(32)>> = bin
    {:ok,:enter_game,role_id}
  end

  def read(10007,bin) do
    <<user_id::size(32),rest::binary>> = bin
    {auth_string,_} = Yugioh.Proto.read_string(rest)
    {:ok,:web_login,[user_id,auth_string]}
  end
  

  def write(10001,true) do
    data = <<1::size(8)>>
    Yugioh.Proto.pack(10001,data)
  end

  def write(10001,false) do
    data = <<0::size(8)>>
    Yugioh.Proto.pack(10001,data)
  end

  def write(10000,code) do
    data = <<code::size(16)>>
    Yugioh.Proto.pack(10000,data)
  end

  def write(10002,code) do
    data = <<code::size(16)>>
    Yugioh.Proto.pack(10002,data)
  end

  def write(10003,code) do
    data = <<code::size(16)>>
    Yugioh.Proto.pack(10003,data)
  end

  def write(10004,data)do
    Yugioh.Proto.pack(10004,data)
  end

  def write(10005,data)do
    Yugioh.Proto.pack(10005,data)
  end

  def write(:tips,message) do
    message_binary = Yugioh.Proto.pack_string(message)
    Yugioh.Proto.pack(10006,message_binary)
  end
end