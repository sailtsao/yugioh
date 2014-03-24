defrecord BattleData,turn_count: 1,turn_player_id: 0,operator_id: 0,phase: :dp,player1_id: 0,player2_id: 0,
  player1_battle_info: nil,player2_battle_info: nil,normal_summoned: false,choose_callback: nil,answer_callback: nil,
  chain_queue: [] do
  @moduledoc """
  battle data record
  """

  @doc """
  get player battle info

  iex> battle_data = BattleData[player1_id: 6,player2_id: 8,player1_battle_info: :a,player2_battle_info: :b]
  ...> battle_data.get_player_battle_info 6
  :a
  iex> battle_data = BattleData[player1_id: 6,player2_id: 8,player1_battle_info: :a,player2_battle_info: :b]
  ...> battle_data.get_player_battle_info 8
  :b
  """
  def get_player_battle_info player_id,BattleData[player1_id: player_id,player1_battle_info: player1_battle_info] do
    player1_battle_info
  end

  def get_player_battle_info player_id,BattleData[player2_id: player_id,player2_battle_info: player2_battle_info] do
    player2_battle_info
  end

  @doc """
  get player atom

  iex> battle_data = BattleData[player1_id: 6,player2_id: 8]
  ...> battle_data.get_player_atom 6
  :player1_battle_info
  iex> battle_data = BattleData[player1_id: 6,player2_id: 8]
  ...> battle_data.get_player_atom 8
  :player2_battle_info
  """
  def get_player_atom player_id,BattleData[player1_id: player_id] do
    :player1_battle_info
  end

  def get_player_atom player_id,BattleData[player2_id: player_id] do
    :player2_battle_info
  end

  @doc """
  get player opponent player id
  """
  def get_opponent_player_id player_id,BattleData[player1_id: player_id,player2_id: player2_id] do
    player2_id
  end

  def get_opponent_player_id player_id,BattleData[player1_id: player1_id,player2_id: player_id] do
    player1_id
  end

  @doc """
  get player oppoen
  """
  def get_opponent_player_battle_info player_id,BattleData[player1_id: player_id,player2_battle_info: player2_battle_info] do
    player2_battle_info
  end

  def get_opponent_player_battle_info player_id,BattleData[player2_id: player_id,player1_battle_info: player1_battle_info] do
    player1_battle_info
  end

  @doc """
  get player atom
  """
  def get_opponent_player_atom player_id,BattleData[player1_id: player_id] do
    :player2_battle_info
  end

  def get_opponent_player_atom player_id,BattleData[player2_id: player_id] do
    :player1_battle_info
  end

  @doc """
  get graveyard params string
  """
  def get_graveyard_params_string battle_data do
    if(Enum.empty?(battle_data.player1_battle_info.graveyardcards)) do
      player1_graveyard_card_id = 0
    else
      player1_graveyard_card_id = hd(battle_data.player1_battle_info.graveyardcards)
    end

    if(Enum.empty?(battle_data.player2_battle_info.graveyardcards)) do
      player2_graveyard_card_id = 0
    else
      player2_graveyard_card_id = hd(battle_data.player2_battle_info.graveyardcards)
    end

    "#{battle_data.player1_id};#{player1_graveyard_card_id};#{battle_data.player2_id};#{player2_graveyard_card_id}"
  end
  @doc """
  new turn operator id

  iex> battle_data = BattleData[turn_player_id: 6,player1_id: 6,player2_id: 8,turn_count: 0]
  ...> battle_data.new_turn_operator_id
  6
  iex> battle_data = BattleData[turn_player_id: 6,player1_id: 6,player2_id: 8,turn_count: 1]
  ...> battle_data.new_turn_operator_id
  8
  iex> battle_data = BattleData[turn_player_id: 8,player1_id: 6,player2_id: 8,turn_count: 2]
  ...> battle_data.new_turn_operator_id
  6
  """
  def new_turn_operator_id(BattleData[turn_player_id: turn_player_id,turn_count: 0]) do
    turn_player_id
  end

  def new_turn_operator_id(battle_data) do
    battle_data.get_opponent_player_id battle_data.turn_player_id
  end

  @doc """
  send message to player
  """
  def send_message player_id,message_data,BattleData[player1_battle_info: BattlePlayerInfo[id: player_id,player_pid: pid]] do
    send pid,{:send,message_data}
  end

  def send_message player_id,message_data,BattleData[player2_battle_info: BattlePlayerInfo[id: player_id,player_pid: pid]] do
    send pid,{:send,message_data}
  end

  @doc """
  send message to all
  """
  def send_message_to_all message_data,BattleData[player1_battle_info: player1_battle_info,player2_battle_info: player2_battle_info] do
    send player1_battle_info.player_pid,{:send,message_data}
    send player2_battle_info.player_pid,{:send,message_data}
  end

  @doc """
  send message to all with mask
  """
  def send_message_to_all_with_mask player_id,message_data,message_data_masked,
  BattleData[player1_id: player_id,player1_battle_info: player1_battle_info,player2_battle_info: player2_battle_info] do
    send player1_battle_info.player_pid,{:send,message_data}
    send player2_battle_info.player_pid,{:send,message_data_masked}
  end

  def send_message_to_all_with_mask player_id,message_data,message_data_masked,
  BattleData[player2_id: player_id,player1_battle_info: player1_battle_info,player2_battle_info: player2_battle_info] do
    send player1_battle_info.player_pid,{:send,message_data_masked}
    send player2_battle_info.player_pid,{:send,message_data}
  end

  def move_cards_to_graveryard player_id,:handcard_zone,index_list,battle_data do
    player_atom = battle_data.get_player_atom player_id
    player_battle_info = battle_data.get_player_battle_info player_id
    handcards = player_battle_info.handcards
    graveyardcards = player_battle_info.graveyardcards
    graveyardcards = List.foldl index_list,graveyardcards,&([Enum.at(handcards,&1)|&2])
    handcards = Enum.filter_map Enum.with_index(handcards),fn({_,index})-> !Enum.member?(index_list,index) end,fn({id,_})-> id end
    player_battle_info = player_battle_info.update(handcards: handcards,graveyardcards: graveyardcards)
    battle_data = battle_data.update [{player_atom,player_battle_info}]
    targets = BattleCore.create_effect_targets player_id,:handcard_zone,index_list
    move_to_graveyard_effect = BattleCore.create_move_to_graveyard_effect targets,battle_data
    message_data = Proto.PT12.write(:effects,[move_to_graveyard_effect])
    battle_data.send_message_to_all message_data
    battle_data
  end

  def move_cards_to_graveryard player_id,:monster_zone,index_list,battle_data do
    player_atom = battle_data.get_player_atom player_id
    player_battle_info = battle_data.get_player_battle_info player_id
    monster_zone = player_battle_info.monster_zone
    graveyardcards = player_battle_info.graveyardcards
    graveyardcards = List.foldl index_list,graveyardcards,&([Dict.get(monster_zone,&1).id|&2])
    monster_zone = Dict.drop(monster_zone,index_list)
    player_battle_info = player_battle_info.update(monster_zone: monster_zone,graveyardcards: graveyardcards)
    battle_data = battle_data.update [{player_atom,player_battle_info}]
    targets = BattleCore.create_effect_targets player_id,:monster_zone,index_list
    move_to_graveyard_effect = BattleCore.create_move_to_graveyard_effect targets,battle_data
    message_data = Proto.PT12.write(:effects,[move_to_graveyard_effect])
    battle_data.send_message_to_all message_data
    battle_data
  end

  def move_cards_to_graveryard player_id,:spell_trap_zone,index_list,battle_data do
    player_atom = battle_data.get_player_atom player_id
    player_battle_info = battle_data.get_player_battle_info player_id
    spell_trap_zone = player_battle_info.spell_trap_zone
    graveyardcards = player_battle_info.graveyardcards
    graveyardcards = List.foldl index_list,graveyardcards,&([Dict.get(spell_trap_zone,&1).id|&2])
    spell_trap_zone = Dict.drop(spell_trap_zone,index_list)
    player_battle_info = player_battle_info.update(spell_trap_zone: spell_trap_zone,graveyardcards: graveyardcards)
    battle_data = battle_data.update [{player_atom,player_battle_info}]
    targets = BattleCore.create_effect_targets player_id,:spell_trap_zone,index_list
    move_to_graveyard_effect = BattleCore.create_move_to_graveyard_effect targets,battle_data
    message_data = Proto.PT12.write(:effects,[move_to_graveyard_effect])
    battle_data.send_message_to_all message_data
    battle_data
  end

  def summon_handcard_monster player_id,handcards_index,presentation,summon_type,battle_data do
    player_atom = battle_data.get_player_atom player_id
    player_battle_info = battle_data.get_player_battle_info player_id
    card_id = Enum.at(player_battle_info.handcards,handcards_index)
    card_data = Data.Cards.get(card_id)
    monster = card_data.become_monster
    monster = monster.update(presentation: presentation,presentation_changed: true)
    handcards = List.delete_at(player_battle_info.handcards,handcards_index)
    avaible_pos = :lists.subtract([2,1,3,0,4],Dict.keys(player_battle_info.monster_zone))
    pos = hd avaible_pos
    monster_zone = Dict.put(player_battle_info.monster_zone,pos,monster)
    player_battle_info = player_battle_info.update(handcards: handcards,monster_zone: monster_zone)
    if summon_type == :normal_summon do
      battle_data = battle_data.update [{player_atom,player_battle_info},{:normal_summoned,true}]
    else
      battle_data = battle_data.update [{player_atom,player_battle_info}]
    end
    targets = BattleCore.create_effect_targets player_id,:monster_zone,[pos]
    summon_effect = BattleCore.create_summon_effect handcards_index,card_id,presentation,targets
    message_data = Proto.PT12.write(:effects,[summon_effect])
    if presentation == :defense_down do
      summon_effect_masked = BattleCore.create_summon_effect handcards_index,0,presentation,targets
      message_data_masked = Proto.PT12.write(:effects,[summon_effect_masked])
      battle_data.send_message_to_all_with_mask player_id,message_data,message_data_masked
    else
      message_data = Proto.PT12.write(:effects,[summon_effect])
      battle_data.send_message_to_all message_data
    end
    battle_data
  end

  def place_handcard_spell_trap player_id,handcards_index,battle_data do
    player_atom = battle_data.get_player_atom player_id
    player_battle_info = battle_data.get_player_battle_info player_id
    handcards = List.delete_at(player_battle_info.handcards,handcards_index)
    pos = player_battle_info.get_spell_trap_available_pos
    card_id = Enum.at(player_battle_info.handcards,handcards_index)
    card_data = Data.Cards.get(card_id)
    spell_trap = card_data.become_spell_trap
    spell_trap_zone = Dict.put(player_battle_info.spell_trap_zone,pos,spell_trap)
    player_battle_info = player_battle_info.update(handcards: handcards,spell_trap_zone: spell_trap_zone)
    battle_data = battle_data.update [{player_atom,player_battle_info}]
    targets = BattleCore.create_effect_targets player_id,:spell_trap_zone,[pos]
    summon_effect = BattleCore.create_summon_effect handcards_index,card_data.id,:place,targets
    message_data = Proto.PT12.write :effects,[summon_effect]
    summon_effect_masked = BattleCore.create_summon_effect handcards_index,0,:place,targets
    message_data_masked = Proto.PT12.write :effects,[summon_effect_masked]
    battle_data.send_message_to_all_with_mask player_id,message_data,message_data_masked
    battle_data
  end

end