defrecord PlayerStatus,id: 0,name: "",avatar: 0,gender: 0,hp: 0,win: 0,lose: 0,cards: []

defmodule Yugioh.Module.Player do
  import Ecto.Query
  def enter_game(role_id,socket) do    
    role = Yugioh.Repo.get(Model.Role,role_id)
    nl = byte_size(role.name)
    cards_list=binary_to_term(role.cards.value)
    cards_binary = iolist_to_binary(Enum.map(cards_list,fn(x)-> <<x::size(32)>> end))
    :gen_tcp.send(socket,Yugioh.Proto.PT10.write(10005,<<role.id::size(32), role.avatar::size(8), nl::size(16), role.name::bitstring,
                        role.hp::size(32),role.win::size(32),role.lose::size(32),length(cards_list)::size(8),cards_binary::binary>>))
    {:ok,PlayerStatus.new(id: role.id,name: role.name,gender: role.gender,avatar: role.avatar,
                          hp: role.hp,win: role.win,lose: role.lose,cards: cards_list)}
  end

end