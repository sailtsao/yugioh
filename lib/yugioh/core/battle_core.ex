defmodule BattleCore do
  require Lager

  def destroy_card(battle_data,player_id,scene_type,pos) do
    Lager.debug "destory card [~p] [~p] [~p] [~p]",[battle_data,player_id,scene_type,pos]
    player_battle_info = get_player_battle_info player_id,battle_data

    case scene_type do
      :spell_trap_zone->
        player_battle_info = [Dict.get(player_battle_info.spell_trap_zone,pos).id|player_battle_info.graveyardcards] 
          |> player_battle_info.graveyardcards
        player_battle_info = Dict.drop(player_battle_info.spell_trap_zone,[pos])|>player_battle_info.spell_trap_zone
    end

    player_atom = get_player_atom player_id,battle_data
    battle_data = battle_data.update([{player_atom,player_battle_info}])

    targets = create_effect_targets player_id,scene_type,[pos]
    move_to_graveyard_effect = create_move_to_graveyard_effect(targets,battle_data)
    send_message_to_all battle_data,:effects,[move_to_graveyard_effect]
    battle_data
   end
    
  def send_choose_message player_pid,choose_type,choose_number,choose_scene_list do
    message_data = Proto.PT12.write(:choose_card,[choose_type,choose_number,choose_scene_list])
    send player_pid,{:send,message_data}
  end

  def send_message player_pid,message_atom,params do
    message_data = Proto.PT12.write(message_atom,params)
    send player_pid,{:send,message_data}
  end  

  def send_message_to_all battle_data,message_atom,params do
    message_data = Proto.PT12.write(message_atom,params)
    send battle_data.player1_battle_info.player_pid,{:send,message_data}
    send battle_data.player2_battle_info.player_pid,{:send,message_data}
  end  

  def hide_handcards battle_info do
    cards_size = length battle_info.handcards    
    battle_info.handcards Enum.take(Stream.cycle([0]),cards_size)
  end

  def is_operator? player_id,BattleData[operator_id: operator_id] do
    operator_id == player_id
  end

  def get_scene_atom scene_type do
    case scene_type do
      :spell_trap_zone->
        :spell_trap_zone
      :monster_card_zone->
        :monster_card_zone
      :handcard_zone ->
        :handcards
      :deck_zone ->
        :deckcards
      :graveyard_zone->
        :graveyardcards
      :banished_zone->
        :banishedcards
      :extra_deck_zone->
        :extradeckcards
      :field_card_zone->
        :field_card
    end
  end  

  def get_operator_battle_info BattleData[operator_id: operator_id,player1_id: player1_id,player2_id: player2_id,
                                player1_battle_info: player1_battle_info,player2_battle_info: player2_battle_info] do
    case operator_id do
      ^player1_id ->
        player1_battle_info
      ^player2_id ->
        player2_battle_info
    end
  end

  def get_operator_atom BattleData[operator_id: operator_id,player1_id: player1_id,player2_id: player2_id] do
    case operator_id do
      ^player1_id ->
        :player1_battle_info
      ^player2_id ->
        :player2_battle_info
    end
  end

  def get_player_battle_info player_id,BattleData[player1_id: player1_id,player2_id: player2_id,
                                player1_battle_info: player1_battle_info,player2_battle_info: player2_battle_info] do
    case player_id do
      ^player1_id ->
        player1_battle_info
      ^player2_id ->
        player2_battle_info
    end
  end

  def get_player_atom player_id,BattleData[player1_id: player1_id,player2_id: player2_id] do
    case player_id do
      ^player1_id ->
        :player1_battle_info
      ^player2_id ->
        :player2_battle_info
    end
  end


  def get_opponent_player_id BattleData[operator_id: operator_id,player1_id: player1_id,player2_id: player2_id] do
    case operator_id do
      ^player1_id->
        player2_id
      ^player2_id->
        player1_id
    end
  end

  def get_opponent_player_atom BattleData[operator_id: operator_id,player1_id: player1_id,player2_id: player2_id] do
    case operator_id do
      ^player1_id ->
        :player2_battle_info
      ^player2_id ->
        :player1_battle_info
    end
  end

  def get_opponent_player_battle_info BattleData[operator_id: operator_id,player1_id: player1_id,player2_id: player2_id,
  player1_battle_info: player1_battle_info,player2_battle_info: player2_battle_info] do
    case operator_id do
      ^player1_id ->
        player2_battle_info
      ^player2_id ->
        player1_battle_info
    end
  end
  
  
  # 0 mean that we have not start our battle,we start the new turn after we collected two battle load finish message
  def get_new_turn_operator_id(BattleData[operator_id: operator_id,turn_count: turn_count])
  when turn_count == 0 do
    operator_id
  end

  def get_new_turn_operator_id(battle_data) do
    get_opponent_player_id battle_data
  end  

  def get_monster_zone_presentation_operations(Monster[presentation_changed: false,presentation: presentation]) do
    case presentation do
      :attack ->
        [:change_to_defense_present_operation]
      :defense_down ->
        [:reverse_operation]
      :defense_up ->
        [:change_to_attack_present_operation]
    end
  end

  def get_monster_zone_presentation_operations(_) do
    []
  end  

  def is_monster_zone_can_fire_effect monster,battle_data do
    case Util.get_normal_skills(monster.skills) do
      []->
        false
      skills->
        Enum.any?(skills,&(ConditionCore.is_skill_conditions_satisfied &1,battle_data))
    end
  end
  
  def get_monster_zone_fire_effect_operations(monster,battle_data) do
    case is_monster_zone_can_fire_effect(monster,battle_data) do
      true->
        [:fire_effect_operation]
      false->
        []
    end
  end
  
  def get_monster_zone_operations monster,battle_data do
    presentation_operations = get_monster_zone_presentation_operations(monster)

    fire_effect_operations = get_monster_zone_fire_effect_operations(monster,battle_data)

    presentation_operations++fire_effect_operations
  end

  def get_handcard_operations(card_data = Card[card_type: card_type],battle_data)
  when card_type == :monster_card do
    player_battle_info = get_operator_battle_info battle_data
    summoned_count = Dict.size(player_battle_info.monster_card_zone)

    normal_operations = get_handcard_monster_normal_summon_operations card_data,summoned_count,battle_data.normal_summoned
    special_summon_operations = get_handcard_monster_special_summon_operation card_data,battle_data

    normal_operations ++ special_summon_operations
  end
  
  def get_handcard_operations(card_data = Card[card_type: card_type],battle_data)
  when card_type == :magic_card or card_type == :trap_card do        
    fire_effect_operation = get_handcard_spell_trap_fire_effect_operation card_data,battle_data
    place_operation = get_handcard_spell_trap_place_operation battle_data
    fire_effect_operation++place_operation
  end  

  def get_handcard_spell_trap_place_operation battle_data do
    player_battle_info = get_operator_battle_info battle_data
    case Enum.count(player_battle_info.spell_trap_zone) do
      5->
        []
      _->
        [:place_operation]
    end
  end

  def is_handcard_spell_trap_can_fire_effect(card_data,battle_data) do
    case Util.get_normal_skills(card_data.skills) do
      []->
        false
      skills->
        Enum.any?(skills,&(ConditionCore.is_skill_conditions_satisfied(&1,battle_data)))
    end
  end

  def get_handcard_spell_trap_fire_effect_operation card_data,battle_data do
    player_battle_info = get_operator_battle_info battle_data
    if Dict.size(player_battle_info.spell_trap_zone) == 5 do
      []
    else
      case is_handcard_spell_trap_can_fire_effect(card_data,battle_data) do
        true->
          [:fire_effect_operation]
        false->
          []
      end
    end
  end  

  def is_handcard_monster_can_be_special_summoned card_data,battle_data do
    case Util.get_special_summon_skill(card_data.skills) do
      nil->
        false
      skill->
        ConditionCore.is_skill_conditions_satisfied skill,battle_data
    end
  end

  def get_handcard_monster_special_summon_operation card_data,battle_data do
    case is_handcard_monster_can_be_special_summoned(card_data,battle_data) do
      true ->
        [:special_summon_operation]
      false ->
        []
    end
  end

  def get_handcard_monster_normal_summon_tribute_number(Card[level: level])
  when level<5 do
    0
  end
  
  def get_handcard_monster_normal_summon_tribute_number(Card[level: level])
  when level == 5 or level == 6 do
    1
  end

  def get_handcard_monster_normal_summon_tribute_number(Card[level: level])
  when level == 7 or level == 8 do
    2
  end

  def get_handcard_monster_normal_summon_tribute_number(Card[level: level])
  when level == 9 or level == 10 do
    3
  end

  def get_handcard_monster_normal_summon_operations _,_,true do
    []
  end

  def get_handcard_monster_normal_summon_operations(Card[level: level],summoned_count,_)
  when level < 5 and summoned_count == 5 do
    []
  end

  def get_handcard_monster_normal_summon_operations(card_data = Card[level: level],summoned_count,_)
  when level > 4 and summoned_count >=1 do
    case is_handcard_monster_can_be_normal_summoned(card_data,summoned_count) do
      true->
        [:summon_operation,:place_operation]
      false->
        []
    end
  end

  def get_handcard_monster_normal_summon_operations(card_data = Card[level: level],0,_)
  when level > 4 do
    []
  end

  def get_handcard_monster_normal_summon_operations _,_,_ do
    [:summon_operation,:place_operation]
  end

  def is_handcard_monster_can_be_normal_summoned(card_data,summoned_count) do
    get_handcard_monster_normal_summon_tribute_number(card_data)<=summoned_count
  end

  def summon_spell_card_for_fire(card_data,handcards_index,battle_data)do
    Lager.debug "battle before summon state [~p]",[battle_data]

    player_id = battle_data.operator_id
    player_atom = BattleCore.get_operator_atom battle_data
    player_battle_info = BattleCore.get_operator_battle_info battle_data
    result = :ok
    if Dict.size(player_battle_info.spell_trap_zone)==5 do
      result = :already_have_5_magic_trap
    end
    pos = 0
    if result == :ok do
      handcards = List.delete_at(player_battle_info.handcards,handcards_index)

      avaible_pos = :lists.subtract([2,1,3,0,4],Dict.keys(player_battle_info.spell_trap_zone))

      pos = hd avaible_pos
      spell_trap = RecordHelper.card_become_spell_trap card_data
      spell_trap_zone = Dict.put(player_battle_info.spell_trap_zone,pos,spell_trap)

      player_battle_info = player_battle_info.update(handcards: handcards,spell_trap_zone: spell_trap_zone)

      battle_data = battle_data.update [{player_atom,player_battle_info}]

      presentation_id = IDUtil.presentation_id_from :attack
      targets = BattleCore.create_effect_targets player_id,:spell_trap_zone,[pos]
      summon_effect = Effect.new(type: :summon_effect,params: "#{handcards_index};#{card_data.id};#{presentation_id}",targets: targets)
      send_message_to_all battle_data,:effects,[summon_effect]

      Lager.debug "battle after summon state [~p]",[battle_data]
    end
    {result,battle_data,pos}
  end

  def summon_card(card_data = Card[card_type: card_type],handcards_index,_presentation,battle_data)
  when card_type == :magic_card or card_type == :trap_card do
    Lager.debug "battle before summon state [~p]",[battle_data]
    player_id = battle_data.operator_id
    player_atom = BattleCore.get_operator_atom battle_data
    player_battle_info = BattleCore.get_operator_battle_info battle_data
    result = :ok
    if Dict.size(player_battle_info.spell_trap_zone)==5 do
      result = :already_have_5_magic_trap
    end
    if result == :ok do
      handcards = List.delete_at(player_battle_info.handcards,handcards_index)

      avaible_pos = :lists.subtract([2,1,3,0,4],Dict.keys(player_battle_info.spell_trap_zone))

      pos = hd avaible_pos
      spell_trap = RecordHelper.card_become_spell_trap card_data
      spell_trap_zone = Dict.put(player_battle_info.spell_trap_zone,pos,spell_trap)

      player_battle_info = player_battle_info.update(handcards: handcards,spell_trap_zone: spell_trap_zone)

      battle_data = battle_data.update [{player_atom,player_battle_info}]

      presentation_id = IDUtil.presentation_id_from :place
      targets = BattleCore.create_effect_targets player_id,:spell_trap_zone,[pos]
      summon_effect = Effect.new(type: :summon_effect,params: "#{handcards_index};#{card_data.id};#{presentation_id}",targets: targets)
      summon_effect_masked = Effect.new(type: :summon_effect,params: "#{handcards_index};0;#{presentation_id}",targets: targets)    
      send_message_to_all_with_mask battle_data,player_atom,:effects,[summon_effect],[summon_effect_masked]
      Lager.debug "battle after summon state [~p]",[battle_data]
    end
    {result,battle_data}
  end

  def summon_card(card_data = Card[card_type: card_type,level: level],handcards_index,presentation,battle_data)
  when level>=5 and card_type == :monster_card do
    Lager.debug "battle before summon state [~p]",[battle_data]
    player_battle_info = BattleCore.get_operator_battle_info battle_data
    result = :ok
    id_index_list = Enum.map player_battle_info.monster_card_zone,fn({index,monster})->
      {monster.id,index}
    end
    summoned_count = Enum.count(player_battle_info.monster_card_zone)
    if(!is_handcard_monster_can_be_normal_summoned(card_data,summoned_count)) do
      result = :not_enough_tribute_monster_for_normal_summon
    end
    if result == :ok do
      tribute_number = get_handcard_monster_normal_summon_tribute_number card_data
      send_choose_message player_battle_info.player_pid,:tribute_choose,tribute_number,[{battle_data.operator_id,:monster_card_zone,id_index_list}]
      battle_data = battle_data.update(phase: {:choose_tribute_card_for_summon_phase,battle_data.phase,tribute_number,handcards_index,presentation})
      Lager.debug "battle after summon state [~p]",[battle_data]
    end    
    {result,battle_data}
  end

  def summon_card(card_data = Card[card_type: card_type,level: level],handcards_index,presentation,battle_data)
  when level<5 and card_type == :monster_card do
    Lager.debug "battle before summon state [~p]",[battle_data]
    player_id = battle_data.operator_id
    player_atom = BattleCore.get_operator_atom battle_data
    player_battle_info = BattleCore.get_operator_battle_info battle_data
    result = :ok
    if Dict.size(player_battle_info.monster_card_zone)==5 do
      result = :already_have_5_monsters
    end
    monster = RecordHelper.card_become_to_monster(card_data)
    monster = monster.update(presentation: presentation,presentation_changed: true)
    if result == :ok do
      handcards = List.delete_at(player_battle_info.handcards,handcards_index)

      avaible_pos = :lists.subtract([2,1,3,0,4],Dict.keys(player_battle_info.monster_card_zone))
      [pos|_] = avaible_pos
      
      monster_card_zone = Dict.put(player_battle_info.monster_card_zone,pos,monster)
      player_battle_info = player_battle_info.update(handcards: handcards,monster_card_zone: monster_card_zone)
      battle_data = battle_data.update [{player_atom,player_battle_info},{:normal_summoned,true}]

      
      targets = create_effect_targets(player_id,:monster_card_zone,[pos])
      summon_effect = create_summon_effect(handcards_index,monster.id,presentation,targets)
      summon_effect_masked = create_summon_effect(handcards_index,0,presentation,targets)
  
      if presentation == :defense_down do
        send_message_to_all_with_mask battle_data,player_atom,:effects,[summon_effect],[summon_effect_masked]
      else
        send_message_to_all battle_data,:effects,[summon_effect]
      end

      Lager.debug "battle after summon state [~p]",[battle_data]
    end
    {result,battle_data}
  end  

  def send_message_to_all_with_mask(battle_data,:player1_battle_info,message_atom,message_params,masked_message_params) do
    message_data = Proto.PT12.write(message_atom,message_params)
    send battle_data.player1_battle_info.player_pid,{:send,message_data}
    message_data = Proto.PT12.write(message_atom,masked_message_params)
    send battle_data.player2_battle_info.player_pid,{:send,message_data}
  end

  def send_message_to_all_with_mask(battle_data,:player2_battle_info,message_atom,message_params,masked_message_params) do
    message_data = Proto.PT12.write(message_atom,masked_message_params)
    send battle_data.player1_battle_info.player_pid,{:send,message_data}
    message_data = Proto.PT12.write(message_atom,message_params)
    send battle_data.player2_battle_info.player_pid,{:send,message_data}
  end

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

  def create_card_presentation_change_effect card_id,new_presentation,player_id,scene_type,index do
    Effect.new(type: :card_presentation_change_effect,
      params: "#{card_id};#{IDUtil.presentation_id_from(new_presentation)}",
      targets: [Target[player_id: player_id,scene_type: scene_type,index: index]])
  end
  
  def create_attack_card_effect attack_player_id,attack_card_index,defense_player_id,defense_card_index,damage_player_id,hp_damage do
    attack_target = Target[player_id: attack_player_id,scene_type: :monster_card_zone,index: attack_card_index]
    defense_target = Target[player_id: defense_player_id,scene_type: :monster_card_zone,index: defense_card_index]
    Effect.new(type: :attack_effect,
      params: "#{attack_player_id};#{defense_player_id};#{damage_player_id};#{hp_damage}",
      targets: [attack_target,defense_target])        
  end

  def create_attack_player_effect attack_player_id,attack_card_index,defense_player_id,hp_damage do
    attack_target = Target[player_id: attack_player_id,scene_type: :monster_card_zone,index: attack_card_index]
    defense_target = Target[player_id: defense_player_id,scene_type: :player_zone,index: 0]
    Effect.new(type: :attack_effect,
      params: "#{attack_player_id};#{defense_player_id};#{defense_player_id};#{hp_damage}",
      targets: [attack_target,defense_target])
  end

  def create_summon_effect handcards_index,card_id,presentation,targets do
    presentation_id = IDUtil.presentation_id_from(presentation)
    Effect.new(type: :summon_effect,
      params: "#{handcards_index};#{card_id};#{presentation_id}",
      targets: targets)
  end

  def create_effect_targets player_id,scene_type,index_list do
    Enum.map index_list,&(Target[player_id: player_id,scene_type: scene_type,index: &1])      
  end

  def create_move_to_graveyard_effect targets,battle_data do    
    Effect.new(type: :move_to_graveyard_effect,
        params: get_graveyard_params_string(battle_data),
        targets: targets)
  end
  
  # already_attacked
  def attack_card_caculation(_player,_opponent_player,Monster[attacked: attacked],_opponent_monster,battle_data) 
  when attacked == true do
    {:already_attacked,battle_data,[]}
  end

  # defense_card_cant_attack
  def attack_card_caculation(_player,_opponent_player,Monster[presentation: presentation],_defense_monster,battle_data)
  when presentation != :attack do
    {:defense_card_cant_attack,battle_data,[]}
  end  

  # attack a > b
  def attack_card_caculation({player_id,player_atom,player_battle_info,source_card_index},
  {opponent_player_id,opponent_player_atom,opponent_player_battle_info,target_card_index},
  attack_monster = Monster[presentation: :attack,attack: attack_monster_attack],
  opponent_monster = Monster[presentation: :attack,attack: opponent_monster_attack],battle_data)
  when attack_monster_attack > opponent_monster_attack do
    destroy_targets = create_effect_targets opponent_player_id,:monster_card_zone,[target_card_index]
    opponent_graveyardcards = [opponent_monster.id|opponent_player_battle_info.graveyardcards]
    opponent_monster_card_zone = Dict.delete opponent_player_battle_info.monster_card_zone,target_card_index
    damage_player_id = opponent_player_id

    hp_damage = attack_monster_attack - opponent_monster_attack
    if hp_damage>opponent_player_battle_info.curhp do
      hp_damage = opponent_player_battle_info.curhp
    end

    opponent_curhp = opponent_player_battle_info.curhp - hp_damage
    opponent_player_battle_info = opponent_player_battle_info.update(curhp: opponent_curhp,
      monster_card_zone: opponent_monster_card_zone,graveyardcards: opponent_graveyardcards)
    
    monster_card_zone = Dict.put(player_battle_info.monster_card_zone,source_card_index,attack_monster.attacked(true)) 
    player_battle_info = player_battle_info.monster_card_zone(monster_card_zone)
    
    battle_data = battle_data.update([{opponent_player_atom,opponent_player_battle_info},{player_atom,player_battle_info}])

    if opponent_curhp <= 0 do
      send self,:battle_end
    end

    attack_card_effect = create_attack_card_effect(player_id,source_card_index,opponent_player_id,target_card_index,
      damage_player_id,hp_damage)
    move_to_graveyard_effect = create_move_to_graveyard_effect(destroy_targets,battle_data)
    {:ok,battle_data,[attack_card_effect,move_to_graveyard_effect]}
  end

  # attack a < b
  def attack_card_caculation({player_id,player_atom,player_battle_info,source_card_index},
  {opponent_player_id,_opponent_player_atom,_opponent_player_battle_info,target_card_index},
  attack_monster = Monster[presentation: :attack,attack: attack_monster_attack],
  opponent_monster = Monster[presentation: :attack,attack: opponent_monster_attack],battle_data) 
  when attack_monster_attack < opponent_monster_attack do
    destroy_targets = create_effect_targets player_id,:monster_card_zone,[source_card_index]
    graveyardcards = [attack_monster.id|player_battle_info.graveyardcards]
    monster_card_zone = Dict.delete player_battle_info.monster_card_zone,source_card_index
    damage_player_id = player_id
    hp_damage = opponent_monster.attack - attack_monster.attack
    if hp_damage>player_battle_info.curhp do
      hp_damage = player_battle_info.curhp
    end
    curhp = player_battle_info.curhp - hp_damage    
    if curhp <= 0 do
      send self,:battle_end
    end
    player_battle_info = player_battle_info.update(curhp: curhp,monster_card_zone: monster_card_zone,graveyardcards: graveyardcards)
    battle_data = battle_data.update([{player_atom,player_battle_info}])
    attack_card_effect = create_attack_card_effect(player_id,source_card_index,opponent_player_id,target_card_index,damage_player_id,hp_damage)
    move_to_graveyard_effect = create_move_to_graveyard_effect(destroy_targets,battle_data)
    {:ok,battle_data,[attack_card_effect,move_to_graveyard_effect]}
  end

  # attack a == b
  def attack_card_caculation({player_id,player_atom,player_battle_info,source_card_index},
  {opponent_player_id,opponent_player_atom,opponent_player_battle_info,target_card_index},
  attack_monster = Monster[presentation: :attack,attack: attack_monster_attack],
  opponent_monster = Monster[presentation: :attack,attack: opponent_monster_attack],battle_data)
  when attack_monster_attack == opponent_monster_attack do
    self_destroy_targets = create_effect_targets player_id,:monster_card_zone,[source_card_index]
    opponent_destroy_targets = create_effect_targets opponent_player_id,:monster_card_zone,[target_card_index]
    destroy_targets = self_destroy_targets++opponent_destroy_targets
    
    graveyardcards = [attack_monster.id|player_battle_info.graveyardcards]
    monster_card_zone = Dict.delete player_battle_info.monster_card_zone,source_card_index
    player_battle_info = player_battle_info.update(monster_card_zone: monster_card_zone,graveyardcards: graveyardcards)

    opponent_graveyardcards = [opponent_monster.id|opponent_player_battle_info.graveyardcards]
    opponent_monster_card_zone = Dict.delete opponent_player_battle_info.monster_card_zone,target_card_index
    opponent_player_battle_info = opponent_player_battle_info.update(monster_card_zone: opponent_monster_card_zone,graveyardcards: opponent_graveyardcards)

    battle_data = battle_data.update([{opponent_player_atom,opponent_player_battle_info},{player_atom,player_battle_info}])
    attack_card_effect = create_attack_card_effect(player_id,source_card_index,opponent_player_id,target_card_index,0,0)
    move_to_graveyard_effect = create_move_to_graveyard_effect(destroy_targets,battle_data)
    {:ok,battle_data,[attack_card_effect,move_to_graveyard_effect]}
  end

  # defense a > b
  def attack_card_caculation({player_id,player_atom,player_battle_info,source_card_index},
  {opponent_player_id,opponent_player_atom,opponent_player_battle_info,target_card_index},
  attack_monster = Monster[presentation: :attack,attack: attack_monster_attack],
  opponent_monster = Monster[presentation: defense_state,defense: opponent_monster_defense],battle_data) 
  when attack_monster_attack > opponent_monster_defense do
    destroy_targets = create_effect_targets opponent_player_id,:monster_card_zone,[target_card_index]

    opponent_graveyardcards = opponent_player_battle_info.graveyardcards++[opponent_monster.id]
    opponent_monster_card_zone = Dict.delete opponent_player_battle_info.monster_card_zone,target_card_index
    opponent_player_battle_info = opponent_player_battle_info.update(
      monster_card_zone: opponent_monster_card_zone,graveyardcards: opponent_graveyardcards)

    monster_card_zone = Dict.put player_battle_info.monster_card_zone,source_card_index,attack_monster.attacked(true)
    player_battle_info = player_battle_info.monster_card_zone monster_card_zone

    battle_data = battle_data.update([{opponent_player_atom,opponent_player_battle_info},{player_atom,player_battle_info}])

    attack_card_effect = create_attack_card_effect(player_id,source_card_index,opponent_player_id,target_card_index,
      0,0)
    move_to_graveyard_effect = create_move_to_graveyard_effect(destroy_targets,battle_data)

    if defense_state == :defense_down do
      card_presentation_change_effect =  create_card_presentation_change_effect(opponent_monster.id,:defense_up,opponent_player_id,:monster_card_zone,target_card_index)
      effects = [card_presentation_change_effect,attack_card_effect,move_to_graveyard_effect]
    else
      effects = [attack_card_effect,move_to_graveyard_effect]
    end
    {:ok,battle_data,effects}
  end

  # defense a < b
  def attack_card_caculation({player_id,player_atom,player_battle_info,source_card_index},
  {opponent_player_id,opponent_player_atom,opponent_player_battle_info,target_card_index},
  attack_monster = Monster[presentation: :attack,attack: attack_monster_attack],
  opponent_monster = Monster[presentation: defense_state,defense: opponent_monster_defense],battle_data)
  when attack_monster_attack < opponent_monster_defense do
    hp_damage = opponent_monster_defense - attack_monster_attack
    if hp_damage>player_battle_info.curhp do
      hp_damage = player_battle_info.curhp
    end
    damage_player_id = player_id
    curhp = player_battle_info.curhp - hp_damage
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

    attack_card_effect = create_attack_card_effect(player_id,source_card_index,opponent_player_id,target_card_index,
      damage_player_id,hp_damage)
    move_to_graveyard_effect = create_move_to_graveyard_effect([],battle_data)

    if defense_state == :defense_down do
      card_presentation_change_effect =  create_card_presentation_change_effect(opponent_monster.id,:defense_up,opponent_player_id,:monster_card_zone,target_card_index)
      effects = [card_presentation_change_effect,attack_card_effect,move_to_graveyard_effect]
    else
      effects = [attack_card_effect,move_to_graveyard_effect]
    end
    {:ok,battle_data,effects}
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
    attack_card_effect = create_attack_card_effect(player_id,source_card_index,opponent_player_id,target_card_index,0,0)
    move_to_graveyard_effect = create_move_to_graveyard_effect([],battle_data)

    if defense_state == :defense_down do
      card_presentation_change_effect =  create_card_presentation_change_effect(opponent_monster.id,:defense_up,opponent_player_id,:monster_card_zone,target_card_index)
      effects = [card_presentation_change_effect,attack_card_effect,move_to_graveyard_effect]
    else
      effects = [attack_card_effect,move_to_graveyard_effect]
    end
    {:ok,battle_data,effects}
  end

  
  def attack_card source_card_index,opponent_card_index,battle_data do

    player = {_,_,player_battle_info,_} = {battle_data.operator_id,get_operator_atom(battle_data),
    get_operator_battle_info(battle_data),source_card_index}

    opponent_player = {_,_,opponent_player_battle_info,_} = {get_opponent_player_id(battle_data),get_opponent_player_atom(battle_data),
    get_opponent_player_battle_info(battle_data),opponent_card_index}
    
    attack_monster = Dict.get player_battle_info.monster_card_zone,source_card_index
    opponent_monster = Dict.get opponent_player_battle_info.monster_card_zone,opponent_card_index

    {result,battle_data,effects} =attack_card_caculation player,opponent_player,attack_monster,opponent_monster,battle_data
    
    send_message battle_data.player1_battle_info.player_pid,:effects,effects
    send_message battle_data.player2_battle_info.player_pid,:effects,effects

    {result,battle_data}
  end

  def attack_player source_card_index,battle_data do

    {player_id,player_atom,player_battle_info} = {battle_data.operator_id,get_operator_atom(battle_data),
    get_operator_battle_info(battle_data)}

    {opponent_player_id,opponent_player_atom,opponent_player_battle_info} = {get_opponent_player_id(battle_data),get_opponent_player_atom(battle_data),
    get_opponent_player_battle_info(battle_data)}
    
    attack_monster = Dict.get player_battle_info.monster_card_zone,source_card_index    
    result = :ok

    if Dict.size(opponent_player_battle_info.monster_card_zone)!=0 do
      result = :attack_player_invalid
    end      

    if result == :ok do
      hp_damage = attack_monster.attack
      if hp_damage>opponent_player_battle_info.curhp do
        hp_damage = opponent_player_battle_info.curhp
      end

      opponent_player_battle_info = opponent_player_battle_info.curhp(opponent_player_battle_info.curhp - hp_damage)

      player_battle_info = player_battle_info.monster_card_zone(Dict.put(player_battle_info.monster_card_zone,source_card_index,attack_monster.attacked(true)))
       
      battle_data = battle_data.update([{opponent_player_atom,opponent_player_battle_info},{player_atom,player_battle_info}])
      
      if opponent_player_battle_info.curhp <= 0 do
        send self,:battle_end
      end

      attack_effect = create_attack_player_effect player_id,source_card_index,opponent_player_id,hp_damage

      send_message battle_data.player1_battle_info.player_pid,:effects,[attack_effect]
      send_message battle_data.player2_battle_info.player_pid,:effects,[attack_effect]
    end    
    {result,battle_data}
  end  
end