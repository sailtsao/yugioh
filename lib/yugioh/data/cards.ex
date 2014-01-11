defmodule Yugioh.Data.Cards do

  def get(1) do
    Card.new(
      id: 1,
      attack: 24000,
      defend: 1600,
      star: 8
      )
  end
  
  def get(2) do
    Card.new(
      id: 2,
      attack: 16000,
      defend: 240,
      star: 8
      )
  end

  def get(3) do
    Card.new(
      id: 3,
      attack: 10000,
      defend: 160,
      star: 8
      )
  end

  def get(4) do
    Card.new(
      id: 4,
      attack: 5000,
      defend: 160,
      star: 8
      )
  end

end
