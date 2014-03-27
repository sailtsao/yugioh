defmodule PlayerCore do
  def load_data player_state,[] do
    message = Proto.PT13.write(:load_data,[])
    send self,{:send,message}
    {:ok,player_state}
  end

  def save_data player_state,[] do
    message = Proto.PT13.write(:save_data,[])
    send self,{:send,message}
    {:ok,player_state}
  end
end