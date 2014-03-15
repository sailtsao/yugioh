defmodule AttackCore do
  require Lager

  # player directly
  def attack source_card_index,battle_data do
    opponent_player_battle_info = battle_data.opponent_player_battle_info
    opponent_monster_amount = opponent_player_battle_info.monster_summoned_amount
    if opponent_monster_amount == 0 do
      attack_player source_card_index,battle_data
    else
      attack_card source_card_index,battle_data
    end
  end
  
  def attack_player source_card_index,battle_data do
    opponent_player_battle_info = battle_data.opponent_player_battle_info
    
    result = :ok
    
    if opponent_player_battle_info.monster_summoned_amount != 0 do
      result = :attack_player_invalid
    end      

    if result == :ok do      
      player_battle_info = battle_data.operator_battle_info
      attack_monster = Dict.get player_battle_info.monster_card_zone,source_card_index    

      hp_damage = attack_monster.attack
      if hp_damage>opponent_player_battle_info.hp do
        hp_damage = opponent_player_battle_info.hp
      end

      monster_card_zone = Dict.put(player_battle_info.monster_card_zone,source_card_index,attack_monster.attacked(true))
      player_battle_info = player_battle_info.monster_card_zone(monster_card_zone)
      opponent_player_battle_info = opponent_player_battle_info.hp(opponent_player_battle_info.hp - hp_damage)
       
      battle_data = battle_data.update([{battle_data.opponent_player_atom,opponent_player_battle_info},
        {battle_data.operator_atom,player_battle_info}])
      
      if opponent_player_battle_info.hp <= 0 do
        send self,:battle_end
      end

      attack_effect = BattleCore.create_attack_player_effect battle_data.operator_id,source_card_index,battle_data.opponent_player_id,hp_damage

      Proto.PT12.write(:effects,[attack_effect])|> battle_data.send_message_to_all
    end    
    {result,battle_data}
  end
  
  def attack_card source_card_index,battle_data do
    player_battle_info = battle_data.operator_battle_info
    opponent_player_id = battle_data.opponent_player_id 
    opponent_player_battle_info = battle_data.opponent_player_battle_info
    # attack_opponent battle_data,source_card_index
    id_index_list = Enum.map opponent_player_battle_info.monster_card_zone,fn({index,monster})->
      {monster.id,index}
    end
    message_data = Proto.PT12.write(:choose_card,[:attack_choose,1,[{opponent_player_id,:monster_card_zone,id_index_list}]])
    player_battle_info.send_message message_data

    choose_callback = fn([{_,:monster_card_zone,[target_card_index]}],battle_data)->
      battle_data = battle_data.choose_callback nil
      attack_card_after_choose source_card_index,target_card_index,battle_data
    end

    battle_data = battle_data.choose_callback choose_callback
    
    {:ok,battle_data}    
  end  
  
  def attack_card_after_choose source_card_index,target_card_index,battle_data do
    player = {_,_,player_battle_info,_} = {battle_data.operator_id,battle_data.operator_atom,
    battle_data.operator_battle_info,source_card_index}

    opponent_player = {_,_,opponent_player_battle_info,_} = {battle_data.opponent_player_id,battle_data.opponent_player_atom,
    battle_data.opponent_player_battle_info,target_card_index}
    
    attack_monster = Dict.get player_battle_info.monster_card_zone,source_card_index
    opponent_monster = Dict.get opponent_player_battle_info.monster_card_zone,target_card_index

    attack_card_caculation player,opponent_player,attack_monster,opponent_monster,battle_data    
  end

  # already_attacked
  def attack_card_caculation(_player,_opponent_player,Monster[attacked: true],_opponent_monster,battle_data) do
    {:already_attacked,battle_data}
  end

  # defense_card_cant_attack
  def attack_card_caculation(_player,_opponent_player,Monster[presentation: presentation],_defense_monster,battle_data)
  when presentation != :attack do
    {:defense_card_cant_attack,battle_data}
  end  

  # attack a > b
  def attack_card_caculation({player_id,player_atom,player_battle_info,source_card_index},
  {opponent_player_id,opponent_player_atom,opponent_player_battle_info,target_card_index},
  attack_monster = Monster[presentation: :attack,attack: attack_monster_attack],
  opponent_monster = Monster[presentation: :attack,attack: opponent_monster_attack],battle_data)
  when attack_monster_attack > opponent_monster_attack do
    destroy_targets = BattleCore.create_effect_targets opponent_player_id,:monster_card_zone,[target_card_index]
    opponent_graveyardcards = [opponent_monster.id|opponent_player_battle_info.graveyardcards]
    opponent_monster_card_zone = Dict.delete opponent_player_battle_info.monster_card_zone,target_card_index
    damage_player_id = opponent_player_id

    hp_damage = attack_monster_attack - opponent_monster_attack
    if hp_damage>opponent_player_battle_info.hp do
      hp_damage = opponent_player_battle_info.hp
    end

    opponent_curhp = opponent_player_battle_info.hp - hp_damage
    opponent_player_battle_info = opponent_player_battle_info.update(curhp: opponent_curhp,
      monster_card_zone: opponent_monster_card_zone,graveyardcards: opponent_graveyardcards)
    
    monster_card_zone = Dict.put(player_battle_info.monster_card_zone,source_card_index,attack_monster.attacked(true)) 
    player_battle_info = player_battle_info.monster_card_zone(monster_card_zone)
    
    battle_data = battle_data.update([{opponent_player_atom,opponent_player_battle_info},{player_atom,player_battle_info}])

    if opponent_curhp <= 0 do
      send self,:battle_end
    end

    attack_card_effect = BattleCore.create_attack_card_effect(player_id,source_card_index,opponent_player_id,target_card_index,
      damage_player_id,hp_damage)
    move_to_graveyard_effect = BattleCore.create_move_to_graveyard_effect(destroy_targets,battle_data)
    battle_data.send_message_to_all Proto.PT12.write(:effects,[attack_card_effect,move_to_graveyard_effect])
    {:ok,battle_data}
  end

  # attack a < b
  def attack_card_caculation({player_id,player_atom,player_battle_info,source_card_index},
  {opponent_player_id,_opponent_player_atom,_opponent_player_battle_info,target_card_index},
  attack_monster = Monster[presentation: :attack,attack: attack_monster_attack],
  opponent_monster = Monster[presentation: :attack,attack: opponent_monster_attack],battle_data) 
  when attack_monster_attack < opponent_monster_attack do
    destroy_targets = BattleCore.create_effect_targets player_id,:monster_card_zone,[source_card_index]
    graveyardcards = [attack_monster.id|player_battle_info.graveyardcards]
    monster_card_zone = Dict.delete player_battle_info.monster_card_zone,source_card_index
    damage_player_id = player_id
    hp_damage = opponent_monster.attack - attack_monster.attack
    if hp_damage>player_battle_info.hp do
      hp_damage = player_battle_info.hp
    end
    curhp = player_battle_info.hp - hp_damage    
    if curhp <= 0 do
      send self,:battle_end
    end
    player_battle_info = player_battle_info.update(curhp: curhp,monster_card_zone: monster_card_zone,graveyardcards: graveyardcards)
    battle_data = battle_data.update([{player_atom,player_battle_info}])
    attack_card_effect = BattleCore.create_attack_card_effect(player_id,source_card_index,opponent_player_id,target_card_index,damage_player_id,hp_damage)
    move_to_graveyard_effect = BattleCore.create_move_to_graveyard_effect(destroy_targets,battle_data)
    battle_data.send_message_to_all Proto.PT12.write(:effects,[attack_card_effect,move_to_graveyard_effect])
    {:ok,battle_data}
  end

  # attack a == b
  def attack_card_caculation({player_id,player_atom,player_battle_info,source_card_index},
  {opponent_player_id,opponent_player_atom,opponent_player_battle_info,target_card_index},
  attack_monster = Monster[presentation: :attack,attack: attack_monster_attack],
  opponent_monster = Monster[presentation: :attack,attack: opponent_monster_attack],battle_data)
  when attack_monster_attack == opponent_monster_attack do
    self_destroy_targets = BattleCore.create_effect_targets player_id,:monster_card_zone,[source_card_index]
    opponent_destroy_targets = BattleCore.create_effect_targets opponent_player_id,:monster_card_zone,[target_card_index]
    destroy_targets = self_destroy_targets++opponent_destroy_targets
    
    graveyardcards = [attack_monster.id|player_battle_info.graveyardcards]
    monster_card_zone = Dict.delete player_battle_info.monster_card_zone,source_card_index
    player_battle_info = player_battle_info.update(monster_card_zone: monster_card_zone,graveyardcards: graveyardcards)

    opponent_graveyardcards = [opponent_monster.id|opponent_player_battle_info.graveyardcards]
    opponent_monster_card_zone = Dict.delete opponent_player_battle_info.monster_card_zone,target_card_index
    opponent_player_battle_info = opponent_player_battle_info.update(monster_card_zone: opponent_monster_card_zone,graveyardcards: opponent_graveyardcards)

    battle_data = battle_data.update([{opponent_player_atom,opponent_player_battle_info},{player_atom,player_battle_info}])
    attack_card_effect = BattleCore.create_attack_card_effect(player_id,source_card_index,opponent_player_id,target_card_index,0,0)
    move_to_graveyard_effect = BattleCore.create_move_to_graveyard_effect(destroy_targets,battle_data)
    battle_data.send_message_to_all Proto.PT12.write(:effects,[attack_card_effect,move_to_graveyard_effect])
    {:ok,battle_data}
  end

  # defense a > b
  def attack_card_caculation({player_id,player_atom,player_battle_info,source_card_index},
  {opponent_player_id,opponent_player_atom,opponent_player_battle_info,target_card_index},
  attack_monster = Monster[presentation: :attack,attack: attack_monster_attack],
  opponent_monster = Monster[presentation: defense_state,defense: opponent_monster_defense],battle_data) 
  when attack_monster_attack > opponent_monster_defense do
    destroy_targets = BattleCore.create_effect_targets opponent_player_id,:monster_card_zone,[target_card_index]

    opponent_graveyardcards = opponent_player_battle_info.graveyardcards++[opponent_monster.id]
    opponent_monster_card_zone = Dict.delete opponent_player_battle_info.monster_card_zone,target_card_index
    opponent_player_battle_info = opponent_player_battle_info.update(
      monster_card_zone: opponent_monster_card_zone,graveyardcards: opponent_graveyardcards)

    monster_card_zone = Dict.put player_battle_info.monster_card_zone,source_card_index,attack_monster.attacked(true)
    player_battle_info = player_battle_info.monster_card_zone monster_card_zone

    battle_data = battle_data.update([{opponent_player_atom,opponent_player_battle_info},{player_atom,player_battle_info}])

    attack_card_effect = BattleCore.create_attack_card_effect(player_id,source_card_index,opponent_player_id,target_card_index,
      0,0)
    move_to_graveyard_effect = BattleCore.create_move_to_graveyard_effect(destroy_targets,battle_data)

    if defense_state == :defense_down do
      card_presentation_change_effect =  BattleCore.create_card_presentation_change_effect(opponent_monster.id,:defense_up,opponent_player_id,:monster_card_zone,target_card_index)
      effects = [card_presentation_change_effect,attack_card_effect,move_to_graveyard_effect]
    else
      effects = [attack_card_effect,move_to_graveyard_effect]
    end
    battle_data.send_message_to_all Proto.PT12.write(:effects,effects)
    {:ok,battle_data}
  end

  # defense a < b
  def attack_card_caculation({player_id,player_atom,player_battle_info,source_card_index},
  {opponent_player_id,opponent_player_atom,opponent_player_battle_info,target_card_index},
  attack_monster = Monster[presentation: :attack,attack: attack_monster_attack],
  opponent_monster = Monster[presentation: defense_state,defense: opponent_monster_defense],battle_data)
  when attack_monster_attack < opponent_monster_defense do
    hp_damage = opponent_monster_defense - attack_monster_attack
    if hp_damage>player_battle_info.hp do
      hp_damage = player_battle_info.hp
    end
    damage_player_id = player_id
    curhp = player_battle_info.hp - hp_damage
    monster_card_zone = Dict.put player_battle_info.monster_card_zone,source_card_index,attack_monster.attacked(true)
    player_battle_info = player_battle_info.update(monster_card_zone: monster_card_zone,curhp: curhp)

    if defense_state == :defense_down do
      opponent_monster_card_zone = Dict.put opponent_player_battle_info.monster_card_zone,target_card_index,opponent_monster.presentation(:defense_up)
      opponent_player_battle_info = opponent_player_battle_info.monster_card_zone opponent_monster_card_zone
      battle_data = battle_data.update([{player_atom,player_battle_info},{opponent_player_atom,opponent_player_battle_info}])      
    else
      battle_data = battle_data.update([{player_atom,player_battle_info}])
    end

    if curhp <= 0 do
      send self,:battle_end
    end    

    attack_card_effect = BattleCore.create_attack_card_effect(player_id,source_card_index,opponent_player_id,target_card_index,
      damage_player_id,hp_damage)
    move_to_graveyard_effect = BattleCore.create_move_to_graveyard_effect([],battle_data)

    if defense_state == :defense_down do
      card_presentation_change_effect =  BattleCore.create_card_presentation_change_effect(opponent_monster.id,:defense_up,opponent_player_id,:monster_card_zone,target_card_index)
      effects = [card_presentation_change_effect,attack_card_effect,move_to_graveyard_effect]
    else
      effects = [attack_card_effect,move_to_graveyard_effect]
    end
    battle_data.send_message_to_all Proto.PT12.write(:effects,effects)
    {:ok,battle_data}
  end

  # defense a == b
  def attack_card_caculation({player_id,player_atom,player_battle_info,source_card_index},
  {opponent_player_id,opponent_player_atom,opponent_player_battle_info,target_card_index},
  attack_monster = Monster[presentation: :attack,attack: attack_monster_attack],
  opponent_monster = Monster[presentation: defense_state,defense: opponent_monster_defense],battle_data) 
  when attack_monster_attack == opponent_monster_defense do
    monster_card_zone = Dict.put player_battle_info.monster_card_zone,source_card_index,attack_monster.attacked(true)
    player_battle_info = player_battle_info.update(monster_card_zone: monster_card_zone)

    if defense_state == :defense_down do
      opponent_monster_card_zone = Dict.put opponent_player_battle_info.monster_card_zone,target_card_index,opponent_monster.presentation(:defense_up)
      opponent_player_battle_info = opponent_player_battle_info.monster_card_zone opponent_monster_card_zone
      battle_data = battle_data.update([{opponent_player_atom,opponent_player_battle_info},{player_atom,player_battle_info}])
    else
      battle_data = battle_data.update([{player_atom,player_battle_info}])
    end

    Lager.debug "battle_data [~p]",battle_data
    attack_card_effect = BattleCore.create_attack_card_effect(player_id,source_card_index,opponent_player_id,target_card_index,0,0)
    move_to_graveyard_effect = BattleCore.create_move_to_graveyard_effect([],battle_data)

    if defense_state == :defense_down do
      card_presentation_change_effect =  BattleCore.create_card_presentation_change_effect(opponent_monster.id,:defense_up,opponent_player_id,:monster_card_zone,target_card_index)
      effects = [card_presentation_change_effect,attack_card_effect,move_to_graveyard_effect]
    else
      effects = [attack_card_effect,move_to_graveyard_effect]
    end
    battle_data.send_message_to_all Proto.PT12.write(:effects,effects)
    {:ok,battle_data}
  end
end