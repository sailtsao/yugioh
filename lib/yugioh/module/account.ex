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

  def create_role([userID,name,gender],socket) do 
    case role_exist?(name) do
      true->
        :gen_tcp.send(socket,Yugioh.Proto.PT10.write(10002,0))
        {:fail,:role_name_exist}
      false->
        user = Yugioh.Repo.get(Model.User,userID)
        r = user.roles.new(name: name,gender: gender)
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
    [user] = Yugioh.Repo.all(from(u in Model.User,where: u.id == ^userID,preload: :roles))
    rl = user.roles.to_list
    n = length(rl)
    l = lc r inlist rl do
      nl = byte_size(r.name)
      <<r.id::size(32), nl::size(16), r.name::bitstring ,r.gender::size(8)>>
    end
    bData =<<n::size(16),iolist_to_binary(l)::binary>>
    :gen_tcp.send(socket,Yugioh.Proto.PT10.write(10004,bData))
    :ok    
  end

  def enter_game(role_id,socket) do
    :gen_tcp.send(socket,Yugioh.Proto.PT10.write(10005,1))
    {:ok,[]}
  end
  
# help function
  defp role_exist?(name) do
    query = from(r in Model.Role,where: r.name == ^name,select: count(r.id))
    [count]=Yugioh.Repo.all(query)
    count > 0
  end


end