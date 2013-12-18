defmodule Yugioh.Acceptor.Supervisor do
  use Supervisor.Behaviour  
  
  def start_link do
    :supervisor.start_link({:local,__MODULE__},__MODULE__,[])
  end

  def init([]) do
   children = [
     worker(Yugioh.Acceptor.Acceptor, [], restart: :temporary)
   ]

   supervise(children, strategy: :simple_one_for_one)
  end
end