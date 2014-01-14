defmodule Yugioh.Data.Cards do

  def get(1) do
    Card.new(
      id: 1,
      attack: 10000,
      defend: 10000,
      star: 8
      )
  end
  
  def get(2) do
    Card.new(
      id: 2,
      attack: 200,
      defend: 200,
      star: 8
      )
  end

  def get(3) do
    Card.new(
      id: 3,
      attack: 300,
      defend: 300,
      star: 8
      )
  end

  def get(4) do
    Card.new(
      id: 4,
      attack: 400,
      defend: 400,
      star: 8
      )
  end

  def get(5) do
    Card.new(
      id: 5,
      attack: 500,
      defend: 500,
      star: 8
      )
  end
end
