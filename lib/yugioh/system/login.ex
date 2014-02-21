defmodule Yugioh.System.Login do
  import Ecto.Query
  require Lager
  require Integer

  alias Yugioh.Library.Online

  def login([acc,pwd],socket) do
      query = from(u in Model.User,where: u.name == ^acc,select: u)
      case Yugioh.Repo.all(query) do
        [user]->
          case user.password==pwd do
            true->
              :gen_tcp.send(socket,Yugioh.Proto.PT10.write(10000,1))
              Lager.debug "login_at [~p]",[user.login_at]
              {:ok,user.id}
            false->
              :gen_tcp.send(socket,Yugioh.Proto.PT10.write(10000,0))
              Lager.debug "wrong_password: account~p, password ~p",[acc,pwd]
              {:fail,:wrong_password}
          end
        []->
          {:error,:no_user_find}
      end
  end

  def web_login([user_id,auth_string],socket) do
    query = from(u in Model.User,where: u.id == ^user_id,select: u)
    case Yugioh.Repo.all(query) do
      [user]->
        if user.auth_string==auth_string do
          login_at = user.login_at
          Lager.debug "web login time [~p]",[login_at]
          login_at_dt = { { login_at.year, login_at.month, login_at.day }, { login_at.hour, login_at.min, login_at.sec } }
          login_at_date = Date.from(login_at_dt,:local)
          invalid_date = Date.shift(login_at_date, min: 30)
          Lager.debug "invalid_date [~p],now date [~p]",[invalid_date,Date.now]
          # if Date.now<invalid_date do
            :gen_tcp.send(socket,Yugioh.Proto.PT10.write(10000,1))
            {:ok,user.id}
          # else
          #   :gen_tcp.send(socket,Yugioh.Proto.PT10.write(10000,0))
          #   {:fail,:session_timeout_need_relogin}
          # end
        else
          :gen_tcp.send(socket,Yugioh.Proto.PT10.write(10000,0))
          Lager.debug "web login invalid auth_string: [~p]",[auth_string]
          {:fail,:invalid_auth_string}
        end
      []->
        {:error,:no_user_find}
    end
  end

  def check_role_exist(name,socket) do
    :gen_tcp.send(socket,Yugioh.Proto.PT10.write(10001,role_exist?(name)))    
    :ok
  end

  def create_role([user_id,name,avatar,_card_type],socket) do 
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
        cards = Enum.take Stream.cycle([1,2,3,4,5]),40
        r = user.roles.new(name: name,avatar: avatar,gender: gender,cards: Ecto.Binary[value: :erlang.term_to_binary(cards)],hp: 3000,win: 0,lose: 0)
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

  def enter_game(role_id,socket) do    
    # TODO:check the role_id is belonged to the user
    if(Online.is_player_online(role_id)) do
      message = Yugioh.Proto.PT10.write(:tips,Yugioh.Data.Strings.get(:login_again_string))
      player_pid = Online.get_online_player(role_id).player_pid
      send(player_pid,{:send,message})
      Yugioh.Player.stop_cast player_pid,:login_again
    end

    # fetch role data from database
    role = Yugioh.Repo.get(Model.Role,role_id)
    Lager.debug "role: ~p",[role]
    
    # caculate role name length
    nl = byte_size(role.name)
    
    # make cards binary
    cards_list=:erlang.binary_to_term(role.cards)
    cards_binary = iolist_to_binary(Enum.map(cards_list,fn(x)-> <<x::size(32)>> end))
    # send player data to client
    :gen_tcp.send(socket,Yugioh.Proto.PT10.write(10005,<<role.id::size(32), role.avatar::size(8), nl::size(16), role.name::bitstring,
                        role.hp::size(32),role.win::size(32),role.lose::size(32),length(cards_list)::size(16),cards_binary::binary>>))

    # create player status record to initialize player process
    player_state=PlayerState.new(id: role.id,name: role.name,gender: role.gender,avatar: role.avatar,
                          hp: role.hp,win: role.win,lose: role.lose,deck: cards_list)

    # create player process
    Yugioh.Player.start({player_state,socket})
  end

# help function
  defp role_exist?(name) do
    query = from(r in Model.Role,where: r.name == ^name,select: count(r.id))
    case Yugioh.Repo.all(query) do
      [0] ->
        false
      _ ->
        true
    end
  end
end