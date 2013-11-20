defmodule Yugioh.Repo do
  use Ecto.Repo, adapter: Ecto.Adapters.Postgres

  def url do
    "ecto://sail@localhost/yugioh_development"
  end
end
