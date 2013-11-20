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
end