defmodule Yugioh.Proto.PT10 do
  def read(10000,bin) do
    {account,rest} = Yugioh.Proto.read_string(bin)
    {password,_} = Yugioh.Proto.read_string(rest)
    {:ok,:login,[account,password]}
  end

  def write(10000,code) do
    data = <<code::size(16)>>
    Yugioh.Proto.pack(10000,data)
  end

end