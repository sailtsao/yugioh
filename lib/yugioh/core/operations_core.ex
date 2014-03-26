defmodule OperationsCore do
  @doc """
  get operations
  """
  require Lager
  alias Data.Cards

  # get handcard zone operations
  def get_operations(player_id,:handcard_zone,index,battle_data = BattleData[phase: phase,operator_id: player_id])
  when phase in [:mp1,:mp2] do
    player_battle_info = battle_data.get_player_battle_info player_id
    card_id = Enum.at(player_battle_info.handcards,index)
    card_data = Cards.get(card_id)
    operations = get_handcard_operations player_id,index,battle_data,card_data
    message = Proto.PT12.write(:get_card_operations,operations)
    player_battle_info.send_message message
    {:ok,battle_data}
  end

  # handcard operations when battle phase
  def get_operations(player_id,:handcard_zone,_index,battle_data = BattleData[phase: :bp]) do
    player_battle_info = battle_data.get_player_battle_info player_id
    message = Proto.PT12.write(:get_card_operations,[])
    player_battle_info.send_message message
    {:ok,battle_data}
  end

  def get_operations(player_id,:monster_zone,index,battle_data = BattleData[phase: phase,operator_id: player_id])
  when phase in [:mp1,:mp2] do
    player_battle_info = battle_data.get_player_battle_info player_id
    monster = player_battle_info.monster_zone[index]

    presentation_operations = monster.get_presentation_operations
    fire_effect_operations = monster.get_fire_effect_operations player_id,index,battle_data
    operations = presentation_operations++fire_effect_operations
    message = Proto.PT12.write(:get_card_operations,operations)
    player_battle_info.send_message message
    {:ok,battle_data}
  end

  def get_operations(player_id,:monster_zone,index,battle_data = BattleData[phase: :bp,turn_count: turn_count,operator_id: player_id]) do
    player_battle_info = battle_data.get_player_battle_info player_id
    monster = player_battle_info.monster_zone[index]
    operations = []
    if monster.attacked == false and turn_count > 1 and monster.presentation == :attack do
      operations = [:attack_operation]
    end
    message = Proto.PT12.write(:get_card_operations,operations)
    player_battle_info.send_message message
    {:ok,battle_data}
  end

  def get_operations(player_id,:spell_trap_zone,index,battle_data = BattleData[phase: phase,operator_id: player_id])
  when phase in [:mp1,:mp2] do
    player_battle_info = battle_data.get_player_battle_info player_id
    spell_trap = player_battle_info.spell_trap_zone[index]
    operations = spell_trap.get_fire_effect_operations player_id,index,battle_data
    message = Proto.PT12.write(:get_card_operations,operations)
    player_battle_info.send_message message
    {:ok,battle_data}
  end

  def get_operations player_id,_,_,battle_data do
    player_battle_info = battle_data.get_player_battle_info player_id
    message = Proto.PT12.write(:get_card_operations,[])
    player_battle_info.send_message message
    {:ok,battle_data}
  end


  # private functions
  defp get_handcard_operations(player_id,index,battle_data,card_data = Card[card_type: :monster_card]) do
    normal_summon_operations = card_data.get_normal_summon_operations player_id,battle_data
    special_summon_operations = card_data.get_special_summon_operations player_id,index,battle_data
    normal_summon_operations ++ special_summon_operations
  end

  defp get_handcard_operations(player_id,index,battle_data,card_data = Card[card_type: card_type])
  when card_type in [:spell_card,:trap_card] do
    fire_effect_operations = card_data.get_fire_effect_operations player_id,index,battle_data
    place_operations = get_handcard_spell_trap_place_operation player_id,battle_data
    fire_effect_operations++place_operations
  end

  defp get_handcard_spell_trap_place_operation player_id,battle_data do
    player_battle_info = battle_data.get_player_battle_info player_id
    if player_battle_info.is_spell_trap_zone_full? do
      []
    else
      [:place_operation]
    end
  end

end