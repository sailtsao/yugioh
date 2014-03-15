defmodule Proto.PT10 do
  @moduledoc """
  protocols of login
  """

  @doc """
  decode login protocol

  iex> Proto.PT10.read(10000,<<4::16,"sail",4::16,"1234">>)
  {:ok,:login,["sail","1234"]}
  iex> Proto.PT10.read(10001,<<4::16,"sail">>)
  {:ok,:check_role_name,["sail"]}
  iex> Proto.PT10.read(10002,<<4::16,"sail",1::8,1::8>>)
  {:ok,:create_role,["sail",1,1]}
  iex> Proto.PT10.read(10003,<<4::16,"sail">>)
  {:ok,:delete_role,["sail"]}
  iex> Proto.PT10.read(10004,<<>>)
  {:ok,:get_roles,[]}
  iex> Proto.PT10.read(10005,<<1::32>>)
  {:ok,:enter_game,[1]}
  iex> Proto.PT10.read(10007,<<1::32,8::16,"12345678">>)
  {:ok,:web_login,[1,"12345678"]}
  """
  def read(10000,bin) do
    {account,rest} = ProtoUtil.read_string(bin)
    {password,_} = ProtoUtil.read_string(rest)
    {:ok,:login,[account,password]}
  end


  def read(10001,bin) do
    {name,_} = ProtoUtil.read_string(bin)
    {:ok,:check_role_name,[name]}
  end

  def read(10002,bin) do
    {name,rest} = ProtoUtil.read_string(bin)
    <<avatar::size(8),card_type::size(8)>> = rest
    {:ok,:create_role,[name,avatar,card_type]}
  end

  def read(10003,bin) do
    {name,_} = ProtoUtil.read_string(bin)
    {:ok,:delete_role,[name]}
  end

  def read(10004,_bin) do
    {:ok,:get_roles,[]}
  end

  def read(10005,bin) do
    <<role_id::32>> = bin
    {:ok,:enter_game,[role_id]}
  end
  
  def read(10007,bin) do
    <<user_id::size(32),rest::binary>> = bin
    {auth_string,_} = ProtoUtil.read_string(rest)
    {:ok,:web_login,[user_id,auth_string]}
  end
  
  @doc """
    encode login message data to protocol binary

    iex> Proto.PT10.write(:login,1)
    <<6::16,10000::16,1::16>>
    iex> Proto.PT10.write(:check_role_name,true)
    <<5::16,10001::16,1::8>>
    iex> Proto.PT10.write(:check_role_name,false)
    <<5::16,10001::16,0::8>>
    iex> Proto.PT10.write(:create_role,1)
    <<6::16,10002::16,1::16>>
    iex> Proto.PT10.write(:delete_role,1)
    <<6::16,10003::16,1::16>>
    iex> Proto.PT10.write(:get_roles,[{1,"sail",1}])
    <<17::16,10004::16,1::16,1::32,4::16,"sail",1::8>>
    iex> Proto.PT10.write(:enter_game,[Model.Role.Entity[model: Model.Role, id: 8, name: "xqy", avatar: 1,hp: 3000, win: 0, lose: 0],[1,2,3,4,5]])
    <<0, 48, 39, 21, 0, 0, 0, 8, 1, 0, 3, 120, 113, 121, 0, 0, 11, 184, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5>>
    iex> Proto.PT10.write(:tips,"12345678")
    <<14::16,10006::16,8::16,"12345678">>
    """  

  def write(:login,code) do
    ProtoUtil.pack(10000,<<code::size(16)>>)
  end

  def write(:check_role_name,true) do
    ProtoUtil.pack(10001,<<1::size(8)>>)
  end

  def write(:check_role_name,false) do
    ProtoUtil.pack(10001,<<0::size(8)>>)
  end

  def write(:create_role,code) do
    ProtoUtil.pack(10002,<<code::size(16)>>)
  end

  def write(:delete_role,code) do
    ProtoUtil.pack(10003,<<code::size(16)>>)
  end

  def write(:get_roles,roles)do
    fun = fn(role)->
      {id,name,avatar} = role
      <<id::size(32), ProtoUtil.pack_string(name)::binary ,avatar::size(8)>>
    end
    ProtoUtil.pack(10004,ProtoUtil.pack_list(roles,fun))
  end

  def write(:enter_game,[role,cards_list])do
    message_binary = <<role.id::32, role.avatar::8, ProtoUtil.pack_string(role.name)::binary,role.hp::32,role.win::32,role.lose::32,
      ProtoUtil.pack_list(cards_list,&(<<&1::32>>))::binary>>
    ProtoUtil.pack(10005,message_binary)
  end

  def write(:tips,message) do
    message_binary = ProtoUtil.pack_string(message)
    ProtoUtil.pack(10006,message_binary)
  end
  
end