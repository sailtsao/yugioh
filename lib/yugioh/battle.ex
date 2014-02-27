defmodule Yugioh.Battle do
  require Lager
  use ExActor.GenServer
  alias Yugioh.Data.Cards
  alias Yugioh.Core.BattleCore

  # init
  definit {player1_pid,player2_pid} do
    Lager.debug "battle process [~p] for player [~p] created",[self,{player1_pid,player2_pid}]
    init_cast self,player1_pid,player2_pid
    initial_state {}
  end

  #############
  # call
  defcall get_cards_of_scene_type(player_id,:graveyard_zone),from: {pid,_},state: battle_data do
    player_battle_info = BattleCore.get_player_battle_info player_id,battle_data
    BattleCore.send_message pid,:get_cards_of_scene_type,[player_id,:graveyard_zone,player_battle_info.graveyardcards]
    set_and_reply battle_data,:ok
  end

  defcall get_cards_of_scene_type(player_id,:extra_deck_zone),from: {pid,_},state: battle_data do
    player_battle_info = BattleCore.get_player_battle_info player_id,battle_data
    BattleCore.send_message pid,:get_cards_of_scene_type,[player_id,:extra_deck_zone,player_battle_info.extradeckcards]
    set_and_reply battle_data,:ok
  end

  defcall get_cards_of_scene_type(player_id,:banished_zone),from: {pid,_},state: battle_data do
    player_battle_info = BattleCore.get_player_battle_info player_id,battle_data
    BattleCore.send_message pid,:get_cards_of_scene_type,[player_id,:banished_zone,player_battle_info.banishedcards]
    set_and_reply battle_data,:ok
  end

  defcall get_cards_of_scene_type(_player_id,_scene_type) do    
    reply :get_cards_of_invalid_scene_type
  end
  # get card operations  
  # already summoned monster in this turn
  defcall get_card_operations(_player_id,:handcard_zone,_index),
  from: {pid,_},
  state: BattleData[normal_summoned: normal_summoned],
  when: normal_summoned==true do
    message_data = Yugioh.Proto.PT12.write(:get_card_operations,[])
    send pid,{:send,message_data}
    reply :ok
  end

  # handcard operations in phase mp1 or mp2 without summoned
  defcall get_card_operations(_player_id,:handcard_zone,index),
  from: {pid,_},
  state: battle_data = BattleData[phase: phase],
  when: phase == :mp1 or phase == :mp2 do    
    player_battle_info = BattleCore.get_operator_battle_info battle_data
    card_id = Enum.at(player_battle_info.handcards,index)
    operations = BattleCore.get_handcard_operations card_id,Dict.size(player_battle_info.monster_card_zone)
    BattleCore.send_message pid,:get_card_operations,operations
    set_and_reply battle_data,:ok
  end

  # handcard operations except mp1 or mp2 phase
  defcall get_card_operations(_player_id,:handcard_zone,_index),from: {pid,_} do
    BattleCore.send_message pid,:get_card_operations,[]
    reply :ok
  end

  # graveyard operations
  defcall get_card_operations(_player_id,:graveyard_zone,_index),from: {pid,_} do
    BattleCore.send_message pid,:get_card_operations,[]
    reply :ok
  end

  # monster operations in bp phase
  defcall get_card_operations(_player_id,:monster_card_zone,index),from: {pid,_},
    state: battle_data = BattleData[phase: phase,turn_count: turn_count],
  when: phase == :bp do

    player_battle_info = BattleCore.get_operator_battle_info battle_data

    monster = Dict.get player_battle_info.monster_card_zone,index
    if monster.attacked == false and turn_count > 1 and monster.presentation == :attack do
      operations = :attack_operation
    end

    BattleCore.send_message pid,:get_card_operations,operations
    set_and_reply battle_data,:ok
  end

  # monster operations in mp1 or mp2
  defcall get_card_operations(_player_id,:monster_card_zone,index),from: {pid,_},
    state: battle_data = BattleData[phase: phase],
  when: phase == :mp1 or phase == :mp2 do

    player_battle_info = BattleCore.get_operator_battle_info battle_data

    operations = []
    monster = Dict.get player_battle_info.monster_card_zone,index 

    if monster.presentation_changed == false do
      operations = BattleCore.get_presentation_operations(monster.presentation)
    end

    BattleCore.send_message pid,:get_card_operations,operations
    set_and_reply battle_data,:ok
  end

  # monster operations in dp sp ep phase
  defcall get_card_operations(_player_id,:monster_card_zone,_index),from: {pid,_} do
    BattleCore.send_message pid,:get_card_operations,[]
    reply :ok
  end

  # magic trap operations
  defcall get_card_operations(_player_id,:magic_trap_zone,_index),from: {pid,_} do
    BattleCore.send_message pid,:get_card_operations,[:fire_effect_operation]
    reply :ok
  end

  defcall get_card_operations(player_id,_,_),state: BattleData[operator_id: operator_id],
  when: operator_id != player_id do
    reply :cant_get_card_operation_when_not_your_turn
  end
# battle_load_finish
  defcall battle_load_finish,state: battle_data=BattleData[phase: phase], when: phase ==:wait_load_finish_1 or phase == :wait_load_finish_2 do
    # 0->1->first dp
    # phase is atom,0 and 1 is used to count the ready message    
    case phase do
      # get one ready message
      :wait_load_finish_1->
        set_and_reply battle_data.phase(:wait_load_finish_2),:ok
      # get two ready message,start the battle
      :wait_load_finish_2->
        send self, :new_turn_draw_phase
        set_and_reply battle_data.phase(:dp),:ok
    end
  end

# normal summon when already summmoned
  defcall summon(_,_,_),
    state: BattleData[normal_summoned: normal_summoned],
    when: normal_summoned == true do
    
    reply :cant_normal_summon_twice_in_one_turn
  end

# normal summon in mp1 mp2 phase
  defcall summon(player_id,handcards_index,presentation),
    state: battle_data = BattleData[phase: phase,player1_id: player1_id,player2_id: player2_id],
    when: phase == :mp1 or phase == :mp2 do

    Lager.debug "battle before summon battle data [~p]",[battle_data]

    {player_atom,player_battle_info} = case player_id do
      ^player1_id->
        {:player1_battle_info,battle_data.player1_battle_info}
      ^player2_id->
        {:player2_battle_info,battle_data.player2_battle_info}
    end
    result = :ok
    if Dict.size(player_battle_info.monster_card_zone)==5 do
      result = :already_have_5_monsters
    end
    
    summon_card_id = Enum.at(player_battle_info.handcards,handcards_index)
    card = Cards.get(summon_card_id)
    monster = Monster[id: card.id,attack: card.attack,defense: card.defense,level: card.level,presentation: presentation,presentation_changed: true]
    tribute_number = 0
    case monster.level do
      x when x==5 or x==6 ->
        monster_size = Dict.size player_battle_info.monster_card_zone
        if monster_size >=1 do
          tribute_number = 1
        else
          result = :not_enough_tribute_monster_for_summon
        end
      x when x==7 or x==8 ->
        monster_size = Dict.size player_battle_info.monster_card_zone
        if monster_size >=2 do
          tribute_number = 2
        else
          result = :not_enough_tribute_monster_for_summon
        end
      x when x>8 ->
        monster_size = Dict.size player_battle_info.monster_card_zone
        if monster_size >=3 do
          tribute_number = 3
        else
          result = :not_enough_tribute_monster_for_summon
        end
      _ ->
        tribute_number = 0
    end

    if result == :ok do
      if tribute_number == 0 do
        handcards = List.delete_at(player_battle_info.handcards,handcards_index)

        avaible_pos = :lists.subtract([2,1,3,0,4],Dict.keys(player_battle_info.monster_card_zone))

        [pos|_] = avaible_pos
        
        monster_card_zone = Dict.put(player_battle_info.monster_card_zone,pos,monster)

        player_battle_info = player_battle_info.update(handcards: handcards,monster_card_zone: monster_card_zone)

        battle_data = battle_data.update [{player_atom,player_battle_info},{:normal_summoned,true}]

        presentation_id = Yugioh.Proto.PT12.presentation_id_from(presentation)
        summon_effect = Effect.new(type: :summon_effect,params: "#{handcards_index};#{summon_card_id};#{presentation_id}",targets: [Target.new(player_id: player_id,scene_type: :monster_card_zone,index: pos)])
        summon_effect_masked = Effect.new(type: :summon_effect,params: "#{handcards_index};0;#{presentation_id}",targets: [Target.new(player_id: player_id,scene_type: :monster_card_zone,index: pos)])    
    
        if presentation == :defense_down do
          case player_atom do
            :player1_battle_info->
              message_data = Yugioh.Proto.PT12.write(:effects,[summon_effect])
              send battle_data.player1_battle_info.player_pid , {:send,message_data}
              message_data = Yugioh.Proto.PT12.write(:effects,[summon_effect_masked])
              send battle_data.player2_battle_info.player_pid , {:send,message_data}
            :player2_battle_info->
              message_data = Yugioh.Proto.PT12.write(:effects,[summon_effect_masked])
              send battle_data.player1_battle_info.player_pid , {:send,message_data}
              message_data = Yugioh.Proto.PT12.write(:effects,[summon_effect])
              send battle_data.player2_battle_info.player_pid , {:send,message_data}
          end
        else
          message_data = Yugioh.Proto.PT12.write(:effects,[summon_effect])
          send battle_data.player1_battle_info.player_pid , {:send,message_data}
          send battle_data.player2_battle_info.player_pid , {:send,message_data}          
        end

        Lager.debug "battle after summon state [~p]",[battle_data]
        set_and_reply battle_data,result
      else
        index_list = Dict.keys player_battle_info.monster_card_zone
        message_data = Yugioh.Proto.PT12.write(:choose_card,[:tribute_choose,:self,:monster_card_zone,tribute_number,index_list])
        send player_battle_info.player_pid,{:send,message_data}
        battle_data = battle_data.update(phase: {:choose_tribute_card_for_summon_phase,phase,tribute_number,handcards_index,presentation})
        set_and_reply battle_data,result
      end
    else
      reply result
    end
  end

  defcall summon(_,_,_) do    
    reply :summon_in_invalid_phase
  end

  defcall choose_card(player_id,choose_index_list),
  state: battle_data=BattleData[phase: {:choose_tribute_card_for_summon_phase,old_phase,tribute_number,handcards_index,presentation},
  player1_id: player1_id,player2_id: player2_id,player1_battle_info: player1_battle_info,player2_battle_info: player2_battle_info] do    
    {player_atom,player_battle_info} = case player_id do
      ^player1_id->
        {:player1_battle_info,battle_data.player1_battle_info}
      ^player2_id->
        {:player2_battle_info,battle_data.player2_battle_info}
    end

    result = :ok

    if length(choose_index_list) != tribute_number do
      result = :wrong_tribute_choose_number
    end
    
    monster_card_zone = player_battle_info.monster_card_zone
    graveyardcards = player_battle_info.graveyardcards
    handcards = player_battle_info.handcards

    move_to_graveyard_targets = List.foldl choose_index_list,[],&(&2 ++ [Target.new(player_id: player_id,scene_type: :monster_card_zone,index: &1)])

    graveyardcards = List.foldl choose_index_list,graveyardcards,&([Dict.get(monster_card_zone,&1).id|&2])

    monster_card_zone = Dict.drop monster_card_zone,choose_index_list

    avaible_pos = :lists.subtract([2,1,3,0,4],Dict.keys(monster_card_zone))
    [pos|_] = avaible_pos

    summon_card_id = Enum.at(handcards,handcards_index)    
    handcards = List.delete_at(handcards,handcards_index)    
    card = Cards.get(summon_card_id)
    monster = Monster[id: card.id,attack: card.attack,defense: card.defense,level: card.level,presentation: presentation,presentation_changed: true]
    monster_card_zone = Dict.put monster_card_zone,pos,monster
    player_battle_info = player_battle_info.update(monster_card_zone: monster_card_zone,graveyardcards: graveyardcards,handcards: handcards)    
    battle_data = battle_data.update([{:phase, old_phase},{player_atom,player_battle_info},{:normal_summoned,true}])

    move_to_graveyard_effect = Effect.new(type: :move_to_graveyard_effect,params: BattleCore.get_graveyard_params_string(battle_data),targets: move_to_graveyard_targets)
    presentation_id = Yugioh.Proto.PT12.presentation_id_from presentation
    summon_effect = Effect.new(type: :summon_effect,params: "#{handcards_index};#{summon_card_id};#{presentation_id}",targets: [Target.new(player_id: player_id,scene_type: :monster_card_zone,index: pos)])
    summon_effect_masked = Effect.new(type: :summon_effect,params: "#{handcards_index};0;#{presentation_id}",targets: [Target.new(player_id: player_id,scene_type: :monster_card_zone,index: pos)])    
    
    if presentation == :defense_down do
      case player_atom do
        :player1_battle_info->
          message_data = Yugioh.Proto.PT12.write(:effects,[move_to_graveyard_effect,summon_effect])
          send battle_data.player2_battle_info.player_pid , {:send,message_data}
          message_data = Yugioh.Proto.PT12.write(:effects,[move_to_graveyard_effect,summon_effect_masked])
          send battle_data.player2_battle_info.player_pid , {:send,message_data}
        :player2_battle_info->
          message_data = Yugioh.Proto.PT12.write(:effects,[move_to_graveyard_effect,summon_effect_masked])
          send battle_data.player1_battle_info.player_pid , {:send,message_data}
          message_data = Yugioh.Proto.PT12.write(:effects,[move_to_graveyard_effect,summon_effect])
          send battle_data.player2_battle_info.player_pid , {:send,message_data}
      end
    else
      message_data = Yugioh.Proto.PT12.write(:effects,[move_to_graveyard_effect,summon_effect])
      send battle_data.player1_battle_info.player_pid , {:send,message_data}
      send battle_data.player2_battle_info.player_pid , {:send,message_data}          
    end

    set_and_reply battle_data,result
  end

  # choose_target_card_for_attack_phase
  defcall choose_card(player_id,[target_card_index]),
  state: battle_data=BattleData[phase: {:choose_target_card_for_attack_phase,old_phase,1,index_list,source_card_index}] do        
    Lager.debug "before attack battle data [~p]",[battle_data]
    {result,battle_data} = BattleCore.attack_card player_id,source_card_index,target_card_index,battle_data
    battle_data = battle_data.phase(old_phase)
    Lager.debug "after attack battle data [~p]",[battle_data]
    set_and_reply battle_data,result
  end

# flip card in mp1 mp2 phase
  defcall flip_card(player_id,card_index),
  state: battle_data = BattleData[phase: phase,player1_id: player1_id,player2_id: player2_id],
  when: phase == :mp1 or phase == :mp2 do

    {player_atom,player_battle_info} = case player_id do
      ^player1_id->
        {:player1_battle_info,battle_data.player1_battle_info}
      ^player2_id->
        {:player2_battle_info,battle_data.player2_battle_info}
    end

    monster = Dict.get(player_battle_info.monster_card_zone,card_index)

    cond do
      monster==nil ->
        reply :invalid_flip_card_index
      monster.presentation_changed ->
        reply :already_changed_presentation_in_one_turn
      true ->
        monster = case monster.presentation do
          :defense_down->
            monster.update(presentation: :attack,presentation_changed: true)
          :defense_up->
            monster.update(presentation: :attack,presentation_changed: true)
          :attack->
            monster.update(presentation: :defense_up,presentation_changed: true)
        end
        monster_card_zone = Dict.put(player_battle_info.monster_card_zone,card_index,monster)
        player_battle_info = player_battle_info.update(monster_card_zone: monster_card_zone)
        battle_data = battle_data.update([{player_atom,player_battle_info}])
        message_data = Yugioh.Proto.PT12.write(:flip_card,[player_id,card_index,monster.id,monster.presentation])
        send battle_data.player1_battle_info.player_pid , {:send,message_data}
        send battle_data.player2_battle_info.player_pid , {:send,message_data}
        set_and_reply battle_data,:ok      
    end    
  end

  # flip card in wrong phase
  defcall flip_card(_,_) do
    reply :flip_card_in_invalid_phase
  end

  defcall attack(player_id,source_card_index),
  state: battle_data = BattleData[phase: phase,player1_id: player1_id,player2_id: player2_id,
         player1_battle_info: player1_battle_info,player2_battle_info: player2_battle_info],
  when: phase == :bp do
    {source_player_battle_info,target_player_battle_info} = case player_id do
      ^player1_id->
        {player1_battle_info,player2_battle_info}
      ^player2_id->
        {player2_battle_info,player1_battle_info}
    end

    # attack_monster = Dict.get source_player_battle_info.monster_card_zone,source_card_index
    if Dict.size(target_player_battle_info.monster_card_zone) == 0 do
      # attack player directly
      Lager.debug "before attack battle data [~p]",[battle_data]
      {result,battle_data} = BattleCore.attack_player player_id,source_card_index,battle_data
      Lager.debug "after attack battle data [~p]",[battle_data]
      set_and_reply battle_data,result
    else      
      # need to choose a card to attack
      index_list = Dict.keys target_player_battle_info.monster_card_zone
      message_data = Yugioh.Proto.PT12.write(:choose_card,[:attack_choose,:other,:monster_card_zone,1,index_list])
      send source_player_battle_info.player_pid,{:send,message_data}
      battle_data = battle_data.update(phase: {:choose_target_card_for_attack_phase,phase,1,index_list,source_card_index})
      set_and_reply battle_data,:ok
    end
  end
        
  defcall attack(_,_),state: BattleData[turn_count: turn_count], when: turn_count == 1 do
    reply :cant_attack_at_first_turn
  end

  defcall attack(_,_) do
    reply :attack_in_invalid_phase
  end  

# change phase
  defcall change_phase_to(_player_id,phase),state: battle_data do
    old_phase = battle_data.phase
    result = case phase do
      :bp when old_phase in [:mp1]->
        battle_data = battle_data.phase(:bp)
        :ok
      :mp2 when old_phase in [:mp1,:bp]->
        battle_data = battle_data.phase(:mp2)
        :ok
      :ep when old_phase in [:mp1,:bp,:mp2]->
        send self , :new_turn_draw_phase
        :ok
      _->
        :invalid_phase_change
    end        
    if result == :ok do      
      battle_data.player1_battle_info.player_pid |> BattleCore.send_message :change_phase_to,phase
      battle_data.player2_battle_info.player_pid |> BattleCore.send_message :change_phase_to,phase
    end
    # Lager.debug "battle_state after change phase [~p] ",[battle_data]
    set_and_reply battle_data,result
  end  

#################
# cast
# init cast
  defcast init_cast(player1_pid,player2_pid) do

    # set battle pid to player state
    player1_state = :gen_server.call(player1_pid,:player_state)
    player2_state = :gen_server.call(player2_pid,:player_state)

    :ok = :gen_server.call(player1_pid,{:update_player_state,player1_state.battle_pid(self)})
    :ok = :gen_server.call(player2_pid,{:update_player_state,player2_state.battle_pid(self)})

    # set random seed
    :random.seed(:erlang.now)

    # shuffle deckcards
    player1_deckcards = Enum.shuffle(player1_state.deck)
    player2_deckcards = Enum.shuffle(player2_state.deck)

    # initialize handcards
    {player1_handcards,player1_deckcards} = Enum.split(player1_deckcards,5)
    {player2_handcards,player2_deckcards} = Enum.split(player2_deckcards,5)

    # initialize player_battle_info
    player1_battle_info = BattleInfo[player_pid: player1_pid,maxhp: player1_state.hp,curhp: player1_state.hp,handcards: player1_handcards,
    deckcards: player1_deckcards,socket: player1_state.socket]
    player2_battle_info = BattleInfo[player_pid: player2_pid,maxhp: player2_state.hp,curhp: player2_state.hp,handcards: player2_handcards,
    deckcards: player2_deckcards,socket: player2_state.socket]

    # wait for player to decide who first 
    # order_game

    # send battle_start message
    params = [1,player1_state.id,:dp,player1_state,player1_battle_info,player2_state,BattleCore.hide_handcards(player2_battle_info)]    
    Yugioh.Proto.PT11.send_message player1_pid,:battle_start,params

    params = [1,player1_state.id,:dp,player1_state,BattleCore.hide_handcards(player1_battle_info),player2_state,player2_battle_info]
    Yugioh.Proto.PT11.send_message player2_pid,:battle_start,params

    new_state BattleData[turn_count: 0,phase: :wait_load_finish_1,operator_id: player1_state.id,player1_id: player1_state.id,player2_id: player2_state.id,
                          player1_battle_info: player1_battle_info,player2_battle_info: player2_battle_info]
  end

# stop cast
  defcast stop_cast,state: state do
    {:stop, :normal, state}
  end

###############
# info
# new turn
  definfo :new_turn_draw_phase,state: battle_data do
    operator_id = BattleCore.get_new_turn_operator_id battle_data
    player_atom = BattleCore.get_player_atom operator_id,battle_data
    player_battle_info = BattleCore.get_player_battle_info operator_id,battle_data
    [draw_card_id|deckcards] = player_battle_info.deckcards
    handcards = player_battle_info.handcards++[draw_card_id]
    monster_card_zone = Enum.map(player_battle_info.monster_card_zone,fn({index,monster})-> {index,monster.turn_reset} end)
    player_battle_info = player_battle_info.update(deck: deckcards,handcards: handcards,monster_card_zone: monster_card_zone)
    battle_data = battle_data.update([{player_atom,player_battle_info},{:turn_count,battle_data.turn_count+1},
      {:phase,:dp},{:normal_summoned,false},{:operator_id,operator_id}])
    
    player1_id = battle_data.player1_id
    player2_id = battle_data.player2_id
    case operator_id do
      ^player1_id->
        message = Yugioh.Proto.PT12.write(:new_turn_draw,[battle_data.turn_count,battle_data.phase,battle_data.operator_id,draw_card_id])
        send battle_data.player1_battle_info.player_pid , {:send,message}
        message = Yugioh.Proto.PT12.write(:new_turn_draw,[battle_data.turn_count,battle_data.phase,battle_data.operator_id,0])
        send battle_data.player2_battle_info.player_pid , {:send,message}
      ^player2_id->
        message = Yugioh.Proto.PT12.write(:new_turn_draw,[battle_data.turn_count,battle_data.phase,battle_data.operator_id,0])
        send battle_data.player1_battle_info.player_pid , {:send,message}
        message = Yugioh.Proto.PT12.write(:new_turn_draw,[battle_data.turn_count,battle_data.phase,battle_data.operator_id,draw_card_id])
        send battle_data.player2_battle_info.player_pid , {:send,message}
    end
    # Lager.debug "battle_state when start new turn [~p]",[battle_data]
    send self, :standby_phase
    new_state battle_data
  end

# stand by
  definfo :standby_phase,state: battle_data do
      battle_data = battle_data.phase(:sp)
      send battle_data.player1_battle_info.player_pid , {:send,Yugioh.Proto.PT12.write(:change_phase_to,:sp)}
      send battle_data.player2_battle_info.player_pid , {:send,Yugioh.Proto.PT12.write(:change_phase_to,:sp)}
      send self , :main_phase_1
      # Lager.debug "battle_state when standby phase [~p]",[battle_data]
      new_state battle_data
  end

# mp1
  definfo :main_phase_1,state: battle_data do
      battle_data = battle_data.phase(:mp1)
      send battle_data.player1_battle_info.player_pid , {:send,Yugioh.Proto.PT12.write(:change_phase_to,:mp1)}
      send battle_data.player2_battle_info.player_pid , {:send,Yugioh.Proto.PT12.write(:change_phase_to,:mp1)}
      # Lager.debug "battle_state when main phase 1 [~p]",[battle_data]
      new_state battle_data
  end

# bp
  definfo :battle_phase,state: battle_data do
      battle_data = battle_data.phase(:bp)
      # Lager.debug "battle_state when battle phase [~p]",[battle_data]
      new_state battle_data
  end

# mp2
  definfo :main_phase_2,state: battle_data do
      battle_data = battle_data.phase(:mp2)
      # Lager.debug "battle_state when main phase 2 [~p]",[battle_data]
      new_state battle_data
  end

# battle end
  definfo :battle_end,state: battle_data do
    {result,lose_player_id,win_player_id} = cond do
      battle_data.player1_battle_info.curhp <= 0 ->
        {:win,battle_data.player1_id,battle_data.player2_id}
      battle_data.player2_battle_info.curhp <= 0 ->  
        {:win,battle_data.player2_id,battle_data.player1_id}
      true->
        # no cards,draw situation
        {:draw,0,0}
    end
    message = Yugioh.Proto.PT12.write(:battle_end,[result,win_player_id,lose_player_id])
    send battle_data.player1_battle_info.player_pid , {:send,message}
    send battle_data.player2_battle_info.player_pid , {:send,message}
    stop_cast self
    noreply
  end

  def terminate(reason,battle_data) do
    Lager.debug "battle_state when battle termianted with reason [~p] : [~p]",[reason,battle_data]
  end
end