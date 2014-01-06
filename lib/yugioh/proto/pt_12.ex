defmodule Yugioh.Proto.PT12 do
  def read(12000,bin) do
    <<index::size(8),type::size(8)>> = bin
    {:ok,{:summon,index,type}}
  end

  def read(12001,bin) do
    <<attacker_pos::size(8),defender_pos::size(8)>> = bin
    {:ok,{:attack,attacker_pos,defender_pos}}
  end
  
  def read(12002,bin) do
    <<card_pos::size(8)>> = bin
    {:ok,{:flip_card,card_pos}}
  end
  

  def write(12000,[code,data]) do
    Yugioh.Proto.pack(12000,data)
  end  
  
  def write(12001,data) do
    Yugioh.Proto.pack(12001,data)
  end  

  def write(12002,data) do
    Yugioh.Proto.pack(12002,data)
  end  
end