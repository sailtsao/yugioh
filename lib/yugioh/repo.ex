defmodule Yugioh.Repo do
  use Ecto.Repo, adapter: Ecto.Adapters.Postgres

  def url do
    "ecto://postgres:sailtsao@localhost/yugioh"
  end
end
