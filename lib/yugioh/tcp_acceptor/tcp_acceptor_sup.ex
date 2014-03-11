defmodule TcpAcceptorSup do
  use Supervisor.Behaviour  
  
  def start_link do
    :supervisor.start_link({:local,__MODULE__},__MODULE__,[])
  end

  def init([]) do
   children = [
     worker(TcpAcceptor, [])
   ]

   supervise(children, strategy: :simple_one_for_one)
  end
end