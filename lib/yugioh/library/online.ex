defmodule Yugioh.Library.Online do
  require Lager

  def add_onine_player(role_id,player_pid) do
    :ets.insert(:online, PlayerOnline.new(id: role_id,player_pid: player_pid))
    Lager.debug "online count ~p",[get_online_player_count]
  end

  def remove_online_player(role_id) do
    :ets.delete(:online, role_id)
    Lager.debug "online count ~p",[get_online_player_count]
  end

  def get_online_player_count do
    :ets.info(:online,:size)
  end

  def get_online_player(role_id) do
    case :ets.lookup :online,role_id do
      []->
        nil
      [online_player]->
        online_player
    end
  end

  def is_player_online(role_id) do
    case :ets.lookup :online,role_id do
      []->
        false
      _other->
        true
    end
  end
  
end