defmodule System.Login do
  @moduledoc """
  callback funtion must use this format
  message_atom([params],socket,client)
  """

  import Ecto.Query
  require Lager
  require Integer

  def socket_event message_id,binary_data,socket,client do
    {:ok,func_atom,params} = ProtoUtil.decode_message(message_id,binary_data)
    apply(__MODULE__,func_atom,[params,socket,client])
  end


  @doc """
  login with account&password
  """
  def login([acc,pwd],socket,client) do
      query = from(u in Model.User,where: u.name == ^acc,select: u)
      case Repo.all(query) do
        [user]->
          case user.password==pwd do
            true->
              :gen_tcp.send(socket,Proto.PT10.write(:login,1))
              client = client.user_id(user.id)
              {:ok,client}
            false->
              :gen_tcp.send(socket,Proto.PT10.write(:login,0))
              {{:fail,:wrong_password},client}
          end
        []->
          {{:error,:no_user_find},client}
      end
  end

  @doc """
  login with user_id & auth_string
  """
  def web_login([user_id,auth_string],socket,client) do
    query = from(u in Model.User,where: u.id == ^user_id,select: u)
    case Repo.all(query) do
      [user]->
        if user.auth_string==auth_string do
          # login_at = user.login_at
          # Lager.debug "web login time [~p]",[login_at]
          # login_at_dt = { { login_at.year, login_at.month, login_at.day }, { login_at.hour, login_at.min, login_at.sec } }
          # login_at_date = Date.from(login_at_dt,:local)
          # _invalid_date = Date.shift(login_at_date, min: 30)
          # Lager.debug "invalid_date [~p],now date [~p]",[invalid_date,Date.now]
          # if Date.now<invalid_date do
            :gen_tcp.send(socket,Proto.PT10.write(:login,1))
            client = client.user_id(user_id)
            {:ok,client}
          # else
          #   :gen_tcp.send(socket,Proto.PT10.write(10000,0))
          #   {:fail,:session_timeout_need_relogin}
          # end
        else
          :gen_tcp.send(socket,Proto.PT10.write(10000,0))
          Lager.debug "web login invalid auth_string: [~p]",[auth_string]
          {{:fail,:invalid_auth_string},client}
        end
      []->
        {{:error,:no_user_find},client}
    end
  end

  def check_role_name([name],socket,client) do
    :gen_tcp.send(socket,Proto.PT10.write(10001,DBUtil.role_exist?(name)))
    {:ok,client}
  end

  def create_role([user_id,name,avatar,_card_type],socket,client) do
    case DBUtil.role_exist?(name) do
      true->
        :gen_tcp.send(socket,Proto.PT10.write(10002,0))
        {:fail,:role_name_exist}
      false->
        user = Repo.get(Model.User,user_id)
        # if avatar_id is even then the gender is female(which is 1 in integer),else is male(whihc is 0 in integer)
        gender = case Integer.even? avatar do
          true->
            1
          false->
            0
        end
        # cards = Enum.take Stream.cycle([1,2,3,4,5,6,7,8,9,10,11,12]),40
        cards = HashDict.new([{1, 3}, {2, 3}, {3, 3}, {4, 3}, {5, 3}, {6, 3}, {7, 3}, {8, 3}, {9, 3},{10, 3}, {11, 3}, {12, 3},{13, 3},{14, 3}])
        cards_binary = Ecto.Binary[value: :erlang.term_to_binary(cards)]
        decks = HashDict.new([{1,Deck[id: 1,name: "Default",main_deck: cards]}])
        decks_binary = Ecto.Binary[value: :erlang.term_to_binary(decks)]

        r = user.roles.new(name: name,avatar: avatar,gender: gender,cards: cards_binary,game_deck_id: 1,decks: decks_binary,hp: 3000,win: 0,lose: 0)
        Repo.create(r)
        :gen_tcp.send(socket,Proto.PT10.write(10002,1))
        {:ok,client}
    end
  end

  def delete_role([name],socket,client) do
    case Repo.all(from(r in Model.Role,where: r.name == ^name,select: r)) do
      nil->
        :gen_tcp.send(socket,Proto.PT10.write(10003,0))
        {{:fail,:role_didnt_exist},client}
      [role]->
        Repo.delete(role)
        :gen_tcp.send(socket,Proto.PT10.write(10003,1))
        {:ok,client}
    end
  end

  def get_roles([],socket,client) do
    roles = DBUtil.get_roles_list(client.user_id)
    :gen_tcp.send(socket,Proto.PT10.write(:get_roles,roles))
    {:ok,client}
  end

  def enter_game([role_id],socket,client) do
    # TODO:check the role_id is belonged to the user

    # if(Online.is_player_online(role_id)) do
      # message_data = Proto.PT10.write(:tips,Data.Strings.get(:login_again_string))
      # player_pid = Online.get_online_player(role_id).player_pid
      # send(player_pid,{:send,message_data})
      # Player.stop_cast player_pid,:login_again
    # end

    # fetch role data from database
    role = DBUtil.get_role_data(role_id)

    cards = :erlang.binary_to_term(role.cards)
    decks = :erlang.binary_to_term(role.decks)
    # cards_list = :erlang.binary_to_term(role.cards)

    # !!!!!!!!!! just for test !!!!!!!!!!!
    # cards_list = Enum.take Stream.cycle([1,3,7,7,8,11,12]),40
    # cards_list = Enum.take Stream.cycle([11,12]),40
    # !!!!!!!!!! just for test !!!!!!!!!!!
    cards_list = []

    message_data = Proto.PT10.write(:enter_game,[role,cards_list])
    :gen_tcp.send(socket,message_data)

    player_state=PlayerState.new(id: role.id,name: role.name,gender: role.gender,avatar: role.avatar,hp: role.hp,
      win: role.win,lose: role.lose,cards: cards,game_deck_id: role.game_deck_id,decks: decks)
    {:ok,player_pid} = System.Player.start({player_state,socket})

    client=client.player_pid player_pid
    Lager.debug "new player enter game,player_state: [~p]",[player_state]
    {:ok,client}
  end

  def logout player_pid do
    System.Player.stop_cast player_pid
  end
end