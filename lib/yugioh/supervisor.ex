defmodule Yugioh.Supervisor do
  use Supervisor.Behaviour

  def init([]) do
    children = [
      # Define workers and child supervisors to be supervised
      worker(Yugioh.Listener, []),
      supervisor(Yugioh.Acceptor.Supervisor, []),
      worker(Yugioh.Repo, [])
    ]

    # See http://elixir-lang.org/docs/stable/Supervisor.Behaviour.html
    # for other strategies and supported options
    supervise(children, strategy: :one_for_all)
  end
end
