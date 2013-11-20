defmodule Yugioh.Listener do
  use GenServer.Behaviour

  def start_link() do
    :gen_server.start_link(__MODULE__,[],[])
  end

  def init(_args) do
    :gen_server.cast(self(),:init)            
    case :gen_tcp.listen(1234,[:binary,{:packet,0},{:active,false},{:reuseaddr,true},{:recbuf,8192}]) do
      {:ok,lSock}->
        spawn_link(fn()-> loop(lSock) end)
        System.at_exit fn(status) ->
          IO.puts "system exit #{status}"
          :gen_tcp.close(lSock)
        end
        {:ok,lSock}
      {:error,reason}->
        IO.inspect reason        
    end
  end

  def loop(lSock) do
    case :gen_tcp.accept(lSock) do
      {:ok,sock}->
        :supervisor.start_child(Yugioh.Acceptor.Supervisor,[sock])
        loop(lSock)
      other->
        other
    end
  end

  def handle_call(_msg, _from, state) do
    reply = :ok
    {:reply, reply, state}
  end

  def handle_cast(:init, state) do
    {:noreply, state}
  end

  def handle_cast(_msg, state) do
    {:noreply, state}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end
  
  def terminate(_reason,lSock) do
    :gen_tcp.close(lSock)
    IO.puts "listener terminate"
    :ok
  end
  
  def code_change(_oldVsn, state, _extra) do
    {:ok, state}
  end
end