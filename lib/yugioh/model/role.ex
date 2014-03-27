defmodule Model.Role do
  use Ecto.Model

  queryable "roles" do
    field :name, :string
    field :gender, :integer
    field :avatar, :integer
    field :hp, :integer
    field :win, :integer
    field :lose, :integer
    field :cards, :binary
    field :game_deck_id, :integer
    field :decks, :binary
    belongs_to :user,Model.User
  end
end