defmodule Yugioh.Supervisor do
  use Supervisor.Behaviour

  def init([]) do
    children = [
      worker(Yugioh.Core, []),
      worker(Yugioh.Repo, []),
      supervisor(Yugioh.Acceptor.Supervisor, [])
    ]

    supervise(children, strategy: :one_for_all)
  end
end
