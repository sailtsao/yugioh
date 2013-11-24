defmodule Model.Role do
  use Ecto.Model

  queryable "roles" do
    field :name, :string
    field :gender, :integer
    belongs_to :user,Model.User
  end
end