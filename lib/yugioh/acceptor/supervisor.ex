defmodule Yugioh.Acceptor.Supervisor do
  use Supervisor.Behaviour  
  
  def start_link do
    :supervisor.start_link({:local,__MODULE__},__MODULE__,[])
  end

  def init([]) do
   children = [
     # Define workers and child supervisors to be supervised
     worker(Yugioh.Acceptor.Worker, [], restart: :temporary)
   ]

   # See http://elixir-lang.org/docs/stable/Supervisor.Behaviour.html
   # for other strategies and supported options
   supervise(children, strategy: :simple_one_for_one)
  end
end