
defmodule Yugioh.Acceptor.Worker do
  use GenServer.Behaviour
  
  defrecordp :rClient,[accid: 0,login: false]

  def start_link(socket) do
    :gen_server.start_link(__MODULE__,[socket],[])
    # start loop to receive message
    # spawn_link(fn->parse_packet(socket,rClient())end)    
  end

  def init(socket) do
    :gen_server.cast(self,:accept)
    {:ok,socket}
  end

  def handle_call(_msg, _from, state) do
    reply = :ok
    {:reply, reply, state}
  end

  def handle_cast(:accept,[socket]) do
    parse_packet(socket,rClient())
    IO.puts "stop"
    {:stop,:normal,socket}
  end 

  def handle_cast(_msg, state) do
    {:noreply, state}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end
  
  def terminate(_reason,state) do
    IO.puts "terminate"
    :ok
  end
  
  def code_change(_oldVsn, state, _extra) do
    {:ok, state}
  end  

  def decode_message(cmd,bin) do
    [h1,h2,_,_,_] = integer_to_list(cmd)
    module = list_to_atom('Elixir.Yugioh.Proto.PT'++[h1,h2])
    module.read(cmd,bin)
  end
  
  def check_error(socket,reason,c) do
    case reason do
      {:error,:timeout} ->
        parse_packet(socket,c)
      other ->
        :gen_tcp.close(socket)
    end
  end
  
  # first 
  def parse_packet(socket,c) do    
    case :gen_tcp.recv(socket,4,2000) do
      {:ok,<<msgLength::size(16),msgID::size(16)>>} ->
        case :gen_tcp.recv(socket,msgLength-4,2000) do
          {:ok,binaryData} ->
            case decode_message(msgID,binaryData) do
              {:ok,:login,loginData} ->
                case Yugioh.Module.Account.login(loginData,socket) do
                  {:ok,accid}->
                    _=rClient(c,accid: accid)
                    _=rClient(c,login: true)
                    parse_packet(socket,c)
                  {:fail,reason}->
                    check_error(socket,reason,c)
                end
              {:ok,:create_role,_data} ->
                parse_packet(socket,c)
              {:ok,:enter_game,_data} ->
                parse_packet(socket,c)              
              other-> # out of pre-enter-game area message
                check_error(socket,other,c)
            end
          other ->
            check_error(socket,other,c)
        end        
      other ->
        check_error(socket,other,c)
    end
  end
  
end