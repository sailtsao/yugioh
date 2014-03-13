defmodule YugiohAppSup do
  use Supervisor.Behaviour
  
  def start_link [acceptor_count,port] do
    :supervisor.start_link __MODULE__,[acceptor_count,port]
  end  

  def init [acceptor_count,port] do
    children = [
      worker(Repo, []),
      worker(System.Room,[]),
      supervisor(TcpListenerSup, [[acceptor_count,port]]),
      supervisor(ClientSup,[])
    ]

    supervise(children, strategy: :one_for_all)
  end
end
