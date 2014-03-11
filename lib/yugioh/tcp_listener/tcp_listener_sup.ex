defmodule TcpListenerSup do
  use Supervisor.Behaviour    

  def start_link [acceptor_count,port] do
    :supervisor.start_link {:local,__MODULE__},__MODULE__,[acceptor_count,port]
  end  

  def init [acceptor_count,port] do
    children = [
      supervisor(TcpAcceptorSup, []),
      worker(TcpListener, [[acceptor_count,port]])     
   ]

   supervise(children, strategy: :one_for_all)
  end
end