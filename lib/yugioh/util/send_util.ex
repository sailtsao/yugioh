defmodule SendUtil do

  def send_message player_pid,message_data do
    send player_pid,{:send,message_data}
  end
  
end