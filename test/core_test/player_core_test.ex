defmodule PlayerCoreTest do
  use ExUnit.Case
  import Ecto.Query
  test "player have cards column" do
    # role = Repo.get(Model.Role,1230)
    # state = :erlang.binary_to_term role.cards
    # IO.inspect state
    # query = from(r in Model.Role,select: r)
    # roles = Repo.all(query)
    # Enum.each roles,fn(role)->
    #   cards = HashDict.new([{1, 3}, {2, 3}, {3, 3}, {4, 3}, {5, 3}, {6, 3}, {7, 3}, {8, 3}, {9, 3},{10, 3}, {11, 3}, {12, 3},{13, 3},{14, 3}])
    #   cards_binary = Ecto.Binary[value: :erlang.term_to_binary(cards)]
    #   decks = HashDict.new([{1,Deck[main_deck: cards]}])
    #   decks_binary = Ecto.Binary[value: :erlang.term_to_binary(decks)]
    #   role = role.update([cards: cards_binary,decks: decks_binary,game_deck_id: 1])
    #   Repo.update role
    # end
    # Enum.each roles,fn(role)->
    #   cards = :erlang.binary_to_term(role.cards)
    #   decks = :erlang.binary_to_term(role.decks)
    #   IO.inspect cards
    #   IO.inspect decks
    # end
    # IO.inspect length(roles)

    # case YugiohDb.Repo.all(query) do
    #   [user]->
    #     cards = Enum.take Stream.cycle([1,2,3,4,5]),40
    #     r = user.roles.new(name: acc,avatar: 1,gender: 1,cards: Ecto.Binary[value: term_to_binary(cards)],hp: 3000,win: 0,lose: 0)
    #     YugiohDb.Repo.create(r)
    #   []->
    #     conn.resp 500,"no this account"<>"#{acc}"
    # end
    # Repo.update

    # state = PlayerState[deck: [1,2,3,4]]
    # role = role.cards Ecto.Binary[value: :erlang.term_to_binary(state)]
    # Repo.update role
  end
end