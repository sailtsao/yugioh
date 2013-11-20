defmodule Yugioh.Proto do
  def read_string(bin) do
    case bin do
      <<len::size(16),data::binary>>->
        # true = is_integer(len)
        # 11 = len
        case data do
          <<str::[size(len),unit(8),binary],rest::binary>>->
            {str,rest}
          _r->
            {[],<<>>}
        end        
      _r->
        {[],<<>>}
    end
  end  
  def pack(cmd,data) do
    l = byte_size(data)+4
    <<l::size(16),cmd::size(16),data::binary>>
  end
  
end