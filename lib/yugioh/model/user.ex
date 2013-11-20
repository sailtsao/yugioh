defmodule Model.User do
  use Ecto.Model

  queryable "users" do
    field :name, :string
    field :password, :string
  end
end