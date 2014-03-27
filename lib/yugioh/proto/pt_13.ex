defmodule Proto.PT13 do

  def read(13000,_) do
    {:ok,{:load_data,[]}}
  end

  def read(13001,bin) do
    {:ok,{:save_data,[]}}
  end


  def write(:load_data,[]) do
    ProtoUtil.pack(13000,<<>>)
  end

  def write(:save_data,[]) do
    ProtoUtil.pack(13001,<<>>)
  end

end