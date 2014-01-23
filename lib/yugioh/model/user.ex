defmodule Model.User do
  use Ecto.Model

  queryable "users" do
    field :name, :string
    field :password, :string
    field :login_at, :datetime
    field :auth_string, :string
    has_many :roles,Model.Role
  end
end