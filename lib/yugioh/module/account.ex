defmodule Yugioh.Module.Account do
  import Ecto.Query

  def login([acc,pwd],socket) do
      query = from(u in Model.User,where: u.name == ^acc,select: u)
      case Yugioh.Repo.all(query) do
        [user]->
          case user.password==pwd do
            true->
              :gen_tcp.send(socket,Yugioh.Proto.PT10.write(10000,1))
              {:ok,user.id}
            false->
              :gen_tcp.send(socket,Yugioh.Proto.PT10.write(10000,0))
              {:fail,:wrong_password}
          end
        []->
          {:fail,:no_user_find}
      end
  end

  def check_role_exist(name,socket) do
    :gen_tcp.send(socket,Yugioh.Proto.PT10.write(10001,role_exist?(name)))    
    :ok
  end

  def create_role([user_id,name,avatar,card_type],socket) do 
    case role_exist?(name) do
      true->
        :gen_tcp.send(socket,Yugioh.Proto.PT10.write(10002,0))
        {:fail,:role_name_exist}
      false->
        user = Yugioh.Repo.get(Model.User,user_id)
        # if avatar_id is even then the gender is female(which is 1 in integer),else is male(whihc is 0 in integer)
        gender = case Integer.even? avatar do
          true->
            1
          false->
            0
        end

        # TODO: replace this 1,2,3,4 with real cards

        r = user.roles.new(name: name,avatar: avatar,gender: gender,cards: Ecto.Binary[value: term_to_binary([1,2,3,4])],hp: 3000,win: 0,lose: 0)
        Yugioh.Repo.create(r)
        :gen_tcp.send(socket,Yugioh.Proto.PT10.write(10002,1))
        :ok
    end
  end

  def delete_role(name,socket) do
    case Yugioh.Repo.all(from(r in Model.Role,where: r.name == ^name,select: r)) do
      nil->
        :gen_tcp.send(socket,Yugioh.Proto.PT10.write(10003,0))
        {:fail,:role_didnt_exist}
      [role]->
        Yugioh.Repo.delete(role)
        :gen_tcp.send(socket,Yugioh.Proto.PT10.write(10003,1))
        :ok
    end
  end

  def get_roles(userID,socket) do
    roles = Yugioh.Repo.all(from(r in Model.Role,where: r.user_id == ^userID,select: {r.id,r.name,r.avatar}))
    n = length(roles)
    l = lc r inlist roles do
      {id,name,avatar} = r
      nl = byte_size(name)
      <<id::size(32), nl::size(16), name::bitstring ,avatar::size(8)>>
    end
    bData =<<n::size(16),iolist_to_binary(l)::binary>>
    :gen_tcp.send(socket,Yugioh.Proto.PT10.write(10004,bData))
    :ok    
  end
  
# help function
  defp role_exist?(name) do
    query = from(r in Model.Role,where: r.name == ^name,select: count(r.id))
    [count]=Yugioh.Repo.all(query)
    count > 0
  end


end