defrecord BattleData,turn_count: 1,operator_id: 0,phase: :dp,player1_id: 0,player2_id: 0,
  player1_battle_info: nil,player2_battle_info: nil,normal_summoned: false,choose_callback: nil,effect_callback: nil do
  @moduledoc """
  battle data record
  """

  @doc """
  get operator player battle info

  iex> battle_data = BattleData[operator_id: 6,player1_id: 6,player2_id: 8,player1_battle_info: :a,player2_battle_info: :b]
  ...> battle_data.operator_battle_info
  :a
  iex> battle_data = BattleData[operator_id: 8,player1_id: 6,player2_id: 8,player1_battle_info: :a,player2_battle_info: :b]
  ...> battle_data.operator_battle_info
  :b
  """
  def operator_battle_info BattleData[operator_id: operator_id,player1_id: operator_id,player1_battle_info: player1_battle_info] do
    player1_battle_info      
  end

  def operator_battle_info BattleData[operator_id: operator_id,player2_id: operator_id,player2_battle_info: player2_battle_info] do
    player2_battle_info
  end

  @doc """
  get operator player atom

  iex> battle_data = BattleData[operator_id: 6,player1_id: 6,player2_id: 8]
  ...> battle_data.operator_atom
  :player1_battle_info
  iex> battle_data = BattleData[operator_id: 8,player1_id: 6,player2_id: 8]
  ...> battle_data.operator_atom
  :player2_battle_info
  """
  def operator_atom BattleData[operator_id: operator_id,player1_id: operator_id] do
    :player1_battle_info
  end

  def operator_atom BattleData[operator_id: operator_id,player2_id: operator_id] do
    :player2_battle_info
  end

  @doc """
  get opponent player id

  iex> battle_data = BattleData[operator_id: 6,player1_id: 6,player2_id: 8]
  ...> battle_data.opponent_player_id
  8
  iex> battle_data = BattleData[operator_id: 8,player1_id: 6,player2_id: 8]
  ...> battle_data.opponent_player_id
  6
  """
  def opponent_player_id BattleData[operator_id: operator_id,player1_id: player1_id,player2_id: operator_id] do
    player1_id
  end

  def opponent_player_id BattleData[operator_id: operator_id,player1_id: operator_id,player2_id: player2_id] do
    player2_id
  end

  @doc """
  get opponent player atom

  iex> battle_data = BattleData[operator_id: 6,player1_id: 6,player2_id: 8]
  ...> battle_data.opponent_player_atom
  :player2_battle_info
  iex> battle_data = BattleData[operator_id: 8,player1_id: 6,player2_id: 8]
  ...> battle_data.opponent_player_atom
  :player1_battle_info
  """
  def opponent_player_atom BattleData[operator_id: operator_id,player1_id: operator_id] do    
    :player2_battle_info
  end

  def opponent_player_atom BattleData[operator_id: operator_id,player2_id: operator_id] do
    :player1_battle_info
  end

  @doc """
  get opponent player battle info

  iex> battle_data = BattleData[operator_id: 6,player1_id: 6,player2_id: 8,player1_battle_info: :a,player2_battle_info: :b]
  ...> battle_data.opponent_player_battle_info
  :b
  iex> battle_data = BattleData[operator_id: 8,player1_id: 6,player2_id: 8,player1_battle_info: :a,player2_battle_info: :b]
  ...> battle_data.opponent_player_battle_info
  :a
  """
  def opponent_player_battle_info BattleData[operator_id: operator_id,player1_id: operator_id,player2_battle_info: player2_battle_info] do
    player2_battle_info      
  end

  def opponent_player_battle_info BattleData[operator_id: operator_id,player2_id: operator_id,player1_battle_info: player1_battle_info] do
    player1_battle_info
  end

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

  iex> battle_data = BattleData[operator_id: 6,player1_id: 6,player2_id: 8,turn_count: 0]
  ...> battle_data.new_turn_operator_id
  6
  iex> battle_data = BattleData[operator_id: 6,player1_id: 6,player2_id: 8]
  ...> battle_data.new_turn_operator_id
  8
  iex> battle_data = BattleData[operator_id: 8,player1_id: 6,player2_id: 8]
  ...> battle_data.new_turn_operator_id
  6
  """
  def new_turn_operator_id(BattleData[operator_id: operator_id,turn_count: 0]) do
    operator_id
  end

  def new_turn_operator_id(battle_data) do
    battle_data.opponent_player_id
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
end