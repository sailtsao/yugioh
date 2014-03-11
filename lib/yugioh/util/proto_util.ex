defmodule ProtoUtil do
  @moduledoc """
  Util function relating to Protocol operation
  """

  @doc """
  read string from binary

  iex> ProtoUtil.read_string(<<10::16,"1234567890">>)
  {"1234567890",<<>>}
  """
  def read_string(bin) do
    <<len::size(16),binary_data::binary>> = bin
    <<str::[size(len),unit(8),binary],rest::binary>> = binary_data
    {str,rest}
  end  

  @doc """
  pack string to binary

  iex> ProtoUtil.pack_string("1234567890")
  <<10::16,"1234567890">>  
  """
  def pack_string(str) do
    nl = byte_size(str)
    <<nl::size(16),str::bitstring>>
  end
  

  @doc """
  pack list to binary
  iex> ProtoUtil.pack_list([1,2,3,4,5],&(<<&1::8>>))
  <<5::16,1::8,2::8,3::8,4::8,5::8>>
  """
  def pack_list(list,fun) do
    <<length(list)::16,List.foldl(list,<<>>,&(&2<>fun.(&1)))::binary>>
  end
  

  @doc """
  pack message binary_data with message_length & message_id in protocol format

  iex> ProtoUtil.pack(10000,<<10::16,"1234567890">>)
  <<0, 16, 39, 16, 0, 10, 49, 50, 51, 52, 53, 54, 55, 56, 57, 48>>
  """
  def pack(message_id,binary_data) do
    message_length = byte_size(binary_data)+4
    <<message_length::size(16),message_id::size(16),binary_data::binary>>
  end

  @doc """
  decode the binary message
  iex> ProtoUtil.decode_message(10000,<<4::16,"sail",4::16,"1234">>)
  {:ok,:login,["sail","1234"]}
  """
  def decode_message(message_id,binary_data) do
    [h1,h2,_,_,_] = integer_to_list(message_id)
    module = list_to_atom('Elixir.Proto.PT'++[h1,h2])
    module.read(message_id,binary_data)
  end
end