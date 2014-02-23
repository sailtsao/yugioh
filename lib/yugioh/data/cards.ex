defmodule Yugioh.Data.Cards do

  def get(1) do
    Card.new(
      id: 1,
      attack: 1000,
      defense: 1000,
      level: 7
      )
  end
  
  def get(2) do
    Card.new(
      id: 2,
      attack: 200,
      defense: 200,
      level: 2
      )
  end

  def get(3) do
    Card.new(
      id: 3,
      attack: 300,
      defense: 300,
      level: 3
      )
  end

  def get(4) do
    Card.new(
      id: 4,
      attack: 400,
      defense: 400,
      level: 4
      )
  end

  def get(5) do
    Card.new(
      id: 5,
      attack: 500,
      defense: 500,
      level: 5
      )
  end
end
