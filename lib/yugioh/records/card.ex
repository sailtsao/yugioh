defrecord Card,id: 0,card_type: nil,atrribute: nil,group: nil,attack: 0,defense: 0,level: 0,skills: [] do
  @doc """
  become monster card
  """
  def become_monster card_data do
    Monster[id: card_data.id,attack: card_data.attack,defense: card_data.defense,level: card_data.level,skills: card_data.skills]
  end
  
  @doc """
  become spell trap card
  """
  def become_spell_trap card_data do
    SpellTrap[id: card_data.id, skills: card_data.skills]
  end  

  @doc """
  get operations
  """
  def get_operations(battle_data,card_data = Card[card_type: :monster_card]) do
    normal_summon_operations = card_data.get_normal_summon_operations battle_data
    special_summon_operations = card_data.get_special_summon_operations battle_data
    normal_summon_operations ++ special_summon_operations
  end
  
  def get_operations(battle_data,card_data = Card[card_type: card_type])
  when card_type in [:magic_card,:trap_card] do
    fire_effect_operations = card_data.get_fire_effect_operations battle_data
    place_operations = card_data.get_place_operations battle_data
    fire_effect_operations++place_operations
  end

  @doc """
  get normal summon operations
  """
  def get_normal_summon_operations(battle_data = BattleData[normal_summoned: false],Card[level: level])
  when level < 5 do
    player_battle_info = battle_data.operator_battle_info
    if player_battle_info.is_monster_card_zone_full? do
      []
    else
      [:summon_operation,:place_operation]
    end    
  end

  def get_normal_summon_operations(BattleData[normal_summoned: true],_) do
    []
  end
  
  def get_normal_summon_operations(battle_data,card_data = Card[level: level])
  when level > 4 do
    player_battle_info = battle_data.operator_battle_info
    summoned_count = player_battle_info.monster_summoned_amount
    case card_data.can_be_normal_summoned? summoned_count do
      true->
        [:summon_operation,:place_operation]
      false->
        []
    end
  end  
    
  @doc """
  get special summon skill
  """
  def get_special_summon_skill card_data do
    case Enum.filter(card_data.skills,&(&1.type == :special_summon_skill)) do
      [skill]->
        skill
      []->
        nil
    end
  end
  
  @doc """
  get normal skills
  """
  def get_normal_skills card_data do
    Enum.filter card_data.skills,&(&1.type == :normal_skill)
  end

  @doc """
  can be special summoned?
  """
  def can_be_special_summoned? battle_data,card_data do
    case card_data.get_special_summon_skill do
      nil->
        false
      skill->
        ConditionCore.is_skill_conditions_satisfied skill,battle_data
    end
  end
  
  @doc """
  get special summon operations
  """
  def get_special_summon_operations battle_data,card_data do
    if card_data.can_be_special_summoned?(battle_data) do
      [:special_summon_operation]
    else
      []
    end
  end

  @doc """
  get place operations
  """
  def get_place_operations battle_data,_card_data do
    player_battle_info = battle_data.operator_battle_info
    if player_battle_info.is_spell_trap_zone_full? do
      []
    else
      [:place_operation]
    end
  end  

  @doc """
  can fire effect
  """
  def can_fire_effect? battle_data,card_data do    
    case card_data.get_normal_skills do
      []->
        false
      skills->
        Enum.any?(skills,&(ConditionCore.is_skill_conditions_satisfied(&1,battle_data)))
    end
  end
  
  @doc """
  get fire effect operations
  """
  def get_fire_effect_operations battle_data,card_data do
    player_battle_info = battle_data.operator_battle_info
    if player_battle_info.is_spell_trap_zone_full? do
      []
    else
      if card_data.can_fire_effect?(battle_data) do
        [:fire_effect_operation]
      else
        []
      end
    end
  end

  @doc """
  get normal summon tribute amount
  """
  def get_normal_summon_tribute_amount(Card[level: level])
  when level<5 do
    0
  end

  def get_normal_summon_tribute_amount(Card[level: level])
  when level in [5,6] do
    1
  end

  def get_normal_summon_tribute_amount(Card[level: level])
  when level in [7,8] do
    2
  end

  def get_normal_summon_tribute_amount(Card[level: level])
  when level in [9,10] do
    3
  end

  @doc """
  can be normal summoned?
  """
  def can_be_normal_summoned? summoned_count,card_data do
    card_data.get_normal_summon_tribute_amount<=summoned_count
  end
end