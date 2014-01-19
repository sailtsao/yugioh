defmodule Yugioh.System.Battle do
  alias Yugioh.Proto.PT12

  def handle({:summon,handcards_index,summon_type},player_state) do
    case is_pid(player_state.battle_pid) do
      true ->
        case Yugioh.Battle.summon(player_state.battle_pid,player_state.id,handcards_index,summon_type) do
          :ok->
            {:ok,player_state}
          reason->
            {:error,reason}
        end
      false ->
        {:error,:invalid_battle_pid}
    end
  end

  def handle({:change_phase_to,phase},player_state) do
    case is_pid(player_state.battle_pid) do
      true ->
        case Yugioh.Battle.change_phase_to(player_state.battle_pid,player_state.id,phase) do
          :ok->
            {:ok,player_state}
          reason->
            {:error,reason}
        end
      false ->
        {:error,:invalid_battle_pid}
    end
  end

  def handle({:attack,summon_card_index,target_card_index},player_state) do
    case is_pid(player_state.battle_pid) do
      true ->
        case Yugioh.Battle.attack(player_state.battle_pid,player_state.id,summon_card_index,target_card_index) do
          :ok->
            {:ok,player_state}
          reason->
            {:error,reason}
        end
      false ->
        {:error,:invalid_battle_pid}
    end
  end

  def handle(:battle_load_finish,player_state) do
    case is_pid(player_state.battle_pid) do
      true ->
        case Yugioh.Battle.battle_load_finish(player_state.battle_pid,player_state.id) do
          :ok->
            {:ok,player_state}
          reason->
            {:error,reason}
        end
      false ->
        {:error,:invalid_battle_pid}
    end
  end
  
end