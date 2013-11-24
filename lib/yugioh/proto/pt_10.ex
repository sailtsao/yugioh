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
    <<gender::size(8)>> = rest
    {:ok,:create_role,[name,gender]}
  end

  def read(10003,bin) do
    {name,_} = Yugioh.Proto.read_string(bin)
    {:ok,:delete_role,name}
  end

  def read(10004,bin) do
    {:ok,:get_roles}
  end

  def write(10000,code) do
    data = <<code::size(16)>>
    Yugioh.Proto.pack(10000,data)
  end

  def write(10001,true) do
    data = <<1::size(8)>>
    Yugioh.Proto.pack(10001,data)
  end

  def write(10001,false) do
    data = <<0::size(8)>>
    Yugioh.Proto.pack(10001,data)
  end

  def write(10002,code) do
    data = <<code::size(8)>>
    Yugioh.Proto.pack(10002,data)
  end

  def write(10003,code) do
    data = <<code::size(8)>>
    Yugioh.Proto.pack(10003,data)
  end

  def write(10004,data)do
    Yugioh.Proto.pack(10004,data)
  end
end