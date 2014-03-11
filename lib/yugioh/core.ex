defmodule Yugioh.Core do
  require Lager
  use GenServer.Behaviour

  def start_link() do
    :gen_server.start_link(__MODULE__,[],[])
  end

  def init(_args) do
    :gen_server.cast(self(),:init)
    {:ok,:booting}
  end

  def loop(lSock) do
    # TODO: improve performance here with pool
    case :gen_tcp.accept(lSock) do
      {:ok,sock}->
        :supervisor.start_child(Yugioh.Acceptor.Supervisor,[sock])
        loop(lSock)
      other->
        Lager.notice "tcp socket accept loop process ~p stopped by reason ~p:",[self,other]
        :gen_tcp.close(lSock)
    end
  end

  def handle_call(_msg, _from, state) do
    reply = :ok
    {:reply, reply, state}
  end

  def handle_cast(:init, state) do

    if  Mix.env == :dev do
      Lager.set_loglevel(:lager_file_backend,'log/console.log',:debug)
      Lager.set_loglevel(:lager_console_backend,:debug)
    end

    case :gen_tcp.listen(1234,[:binary,{:packet,0},{:active,false},{:reuseaddr,true},{:recbuf,8192}]) do      
      {:ok,lSock}->
        Lager.info "yugioh server listened at port 1234 successed"
        pid = spawn_link(fn()-> loop(lSock) end)
        {:noreply,{:booted,pid,lSock}}
      {:error,reason}->
        {:stop,reason,state}
    end
    
    # :ets.new(:online,[{:keypos,PlayerOnline.__record__(:index,:id)+1},:named_table,:set,:public])    
    
    Yugioh.System.Room.start

    {:noreply, state}
  end

  def handle_cast(:stop,state) do
    {:stop, :normal, state}
  end

  def handle_cast(_msg, state) do
    {:noreply, state}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end
  
  def terminate(reason,state) do    
    Lager.notice "system exit with state: ~p, reason: ~p",[state,reason]
    :ok
  end
  
  def code_change(_oldVsn, state, _extra) do
    {:ok, state}
  end
end