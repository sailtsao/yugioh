defmodule Yugioh.Battle do
  require Lager
  use ExActor.GenServer
  alias Yugioh.Data.Cards

  definit {player1_pid,player2_pid} do
    Lager.debug "battle process [~p] for player [~p] created",[self,{player1_pid,player2_pid}]
    init_cast self,player1_pid,player2_pid
    initial_state {}
  end

  defcall get_card_operations(player_id,:handcard_scene,index),
  from: {pid,_},
  state: BattleData[phase: phase],
  when: phase == :mp1 or phase == :mp2 do
    message_data = Yugioh.Proto.PT12.write(:get_card_operations,[[:summon,:place]])
    send pid,{:send,message_data}
    reply :ok
  end

  defcall get_card_operations(player_id,:handcard_scene,index),from: {pid,_} do
    message_data = Yugioh.Proto.PT12.write(:get_card_operations,[[]])
    send pid,{:send,message_data}
    reply :ok
  end

  defcall get_card_operations(player_id,:graveyard_scene,index),from: {pid,_} do
    message_data = Yugioh.Proto.PT12.write(:get_card_operations,[[]])
    send pid,{:send,message_data}
    reply :ok
  end

  defcall get_card_operations(player_id,:monster_scene,index),from: {pid,_},
    state: BattleData[phase: phase,player1_id: player1_id,player2_id: player2_id,
                      player1_battle_info: player1_battle_info,player2_battle_info: player2_battle_info],
  when: phase == :bp do
    player_battle_info = case player_id do
      ^player1_id->
        player1_battle_info
      ^player2_id->
        player2_battle_info
    end
    operations = []
    monster = Dict.get player_battle_info.monster_card_zone,index
    if monster.attacked == false do
      operations = [:attack]
    end
    message_data = Yugioh.Proto.PT12.write(:get_card_operations,[[:attack]])
    send pid,{:send,message_data}
    reply :ok
  end

  defcall get_card_operations(player_id,:monster_scene,index),from: {pid,_} do
    message_data = Yugioh.Proto.PT12.write(:get_card_operations,[[]])
    send pid,{:send,message_data}
    reply :ok
  end

  defcall get_card_operations(player_id,:magic_trap_scene,index),from: {pid,_},state: battle_data do
    message_data = Yugioh.Proto.PT12.write(:get_card_operations,[[:fire_effect]])
    send pid,{:send,message_data}
    reply :ok
  end

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
  
  defcall summon(_,_,_),
    state: battle_data = BattleData[normal_summoned: normal_summoned],
    when: normal_summoned == true do
    
    reply :cant_normal_summon_twice_in_one_turn
  end

  defcall summon(player_id,handcards_index,summon_type),
    state: battle_data = BattleData[phase: phase,player1_id: player1_id,player2_id: player2_id],
    when: phase == :mp1 or phase == :mp2 do

    Lager.debug "battle before summon battle data [~p]",[battle_data]

    {player_atom,player_battle_info} = case player_id do
      ^player1_id->
        {:player1_battle_info,battle_data.player1_battle_info}
      ^player2_id->
        {:player2_battle_info,battle_data.player2_battle_info}
    end

    cond do
      Dict.size(player_battle_info.monster_card_zone)==5 ->
        reply :cant_summon_more
      true ->
        summon_card_id = Enum.at(player_battle_info.handcards,handcards_index)

        new_handcards = List.delete_at(player_battle_info.handcards,handcards_index)

        avaible_pos = :lists.subtract([2,1,3,0,4],Dict.keys(player_battle_info.monster_card_zone))

        [pos|_] = avaible_pos

        card = Cards.get(summon_card_id)
        monster = Monster[id: card.id,attack: card.attack,defense: card.defense,level: card.level,presentation: summon_type]
        new_monster_card_zone = Dict.put(player_battle_info.monster_card_zone,pos,monster)

        new_player_battle_info = player_battle_info.update(handcards: new_handcards,monster_card_zone: new_monster_card_zone)

        new_battle_data = battle_data.update [{player_atom,new_player_battle_info},{:normal_summoned,true}]
        if summon_type == :defense_down do
          case player_atom do
            :player1_battle_info->
              message_data = Yugioh.Proto.PT12.write(:summon,[player_id,handcards_index,summon_card_id,pos,summon_type])
              send battle_data.player1_battle_info.player_pid , {:send,message_data}
              message_data = Yugioh.Proto.PT12.write(:summon,[player_id,handcards_index,0,pos,summon_type])
              send battle_data.player2_battle_info.player_pid , {:send,message_data}
            :player2_battle_info->
              message_data = Yugioh.Proto.PT12.write(:summon,[player_id,handcards_index,0,pos,summon_type])
              send battle_data.player1_battle_info.player_pid , {:send,message_data}
              message_data = Yugioh.Proto.PT12.write(:summon,[player_id,handcards_index,summon_card_id,pos,summon_type])
              send battle_data.player2_battle_info.player_pid , {:send,message_data}
          end
        else
          message_data = Yugioh.Proto.PT12.write(:summon,[player_id,handcards_index,summon_card_id,pos,summon_type])
          send battle_data.player1_battle_info.player_pid , {:send,message_data}
          send battle_data.player2_battle_info.player_pid , {:send,message_data}          
        end        

        Lager.debug "battle after summon state [~p]",[new_battle_data]
        set_and_reply new_battle_data,:ok
    end    
  end

  defcall summon(_,_,_) do    
    reply :summon_in_invalid_phase
  end
  
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
        new_monster = case monster.presentation do
          :defense_down->
            monster.update(presentation: :attack,presentation_changed: true)
          :defense_up->
            monster.update(presentation: :attack,presentation_changed: true)
          :attack->
            monster.update(presentation: :defense_up,presentation_changed: true)
        end
        new_monster_card_zone = Dict.put(player_battle_info.monster_card_zone,card_index,new_monster)
        new_player_battle_info = player_battle_info.update(monster_card_zone: new_monster_card_zone)
        new_battle_data = battle_data.update([{player_atom,new_player_battle_info}])
        message_data = Yugioh.Proto.PT12.write(:flip_card,[player_id,card_index,new_monster.id,new_monster.presentation])
        send battle_data.player1_battle_info.player_pid , {:send,message_data}
        send battle_data.player2_battle_info.player_pid , {:send,message_data}
        set_and_reply new_battle_data,:ok      
    end    
  end

  defcall flip_card(_,_) do
    reply :flip_card_in_invalid_phase
  end

# 11 mean attack player directly
  defcall attack(player_id,source_card_index,11),
    state: battle_data = BattleData[phase: phase,player1_id: player1_id,player2_id: player2_id],
    when: phase==:bp do

    Lager.debug "before attack battle data [~p]",[battle_data]

    {source_player_id,target_player_id,source_player_atom,source_player_battle_info,target_player_atom,target_player_battle_info} = case player_id do
      ^player1_id->
        {player1_id,player2_id,:player1_battle_info,battle_data.player1_battle_info,:player2_battle_info,battle_data.player2_battle_info}
      ^player2_id->
        {player2_id,player1_id,:player2_battle_info,battle_data.player2_battle_info,:player1_battle_info,battle_data.player1_battle_info}
    end

    attack_monster = Dict.get source_player_battle_info.monster_card_zone,source_card_index
    hp_damage = 0
    result = :ok
    new_battle_data = battle_data
    cond do
      attack_monster.attacked ->
        result = :already_attacked
      Dict.size(target_player_battle_info.monster_card_zone)!=0->
        result = :attack_directly_invalid
      true->
        hp_damage = attack_monster.attack
        if hp_damage>target_player_battle_info.curhp do
          hp_damage = target_player_battle_info.curhp
        end
        new_target_curhp = target_player_battle_info.curhp - hp_damage
        new_target_player_battle_info = target_player_battle_info.update(curhp: new_target_curhp)
        new_source_monster_card_zone = Dict.put source_player_battle_info.monster_card_zone,source_card_index,attack_monster.attacked(true)
        new_source_player_battle_info = source_player_battle_info.update(monster_card_zone: new_source_monster_card_zone)
        new_battle_data = battle_data.update([{target_player_atom,new_target_player_battle_info},
            {source_player_atom,new_source_player_battle_info}])
        if new_target_curhp <= 0 do
          send self,:battle_end
        end
    end
    if result==:ok do
      message = Yugioh.Proto.PT12.write(:attack,[source_card_index,11,0,target_player_id,hp_damage,[],
        source_player_id,source_player_battle_info.graveyardcards,target_player_id,target_player_battle_info.graveyardcards])
      send battle_data.player1_battle_info.player_pid , {:send,message}
      send battle_data.player2_battle_info.player_pid , {:send,message}
    end
    Lager.debug "after attack battle data [~p]",[new_battle_data]
    set_and_reply new_battle_data,result
  end

  # attack with cards
  defcall attack(player_id,source_card_index,target_card_index),
    state: battle_data = BattleData[phase: phase,player1_id: player1_id,player2_id: player2_id],
    when: phase==:bp do

    Lager.debug "before attack battle data [~p]",[battle_data]
    
    {source_player_id,target_player_id,source_player_atom,source_player_battle_info,target_player_atom,target_player_battle_info} = case player_id do
      ^player1_id->
        {player1_id,player2_id,:player1_battle_info,battle_data.player1_battle_info,:player2_battle_info,battle_data.player2_battle_info}
      ^player2_id->
        {player2_id,player1_id,:player2_battle_info,battle_data.player2_battle_info,:player1_battle_info,battle_data.player1_battle_info}
    end

    # TODO: pelase consider the situation that there is no defender at all, how to directly attack player.
    attack_monster = Dict.get source_player_battle_info.monster_card_zone,source_card_index
    defense_monster = Dict.get target_player_battle_info.monster_card_zone,target_card_index

    damage_player_id = target_player_id
    hp_damage = 0
    destroy_cards = []
    result = :ok
    new_battle_data = battle_data
    cond do
      attack_monster.attacked ->
        result = :already_attacked
      true ->
        case {attack_monster.presentation,defense_monster.presentation} do
          {:attack,:attack} ->
            cond do
                # defender dead and update the defense player's hp
              attack_monster.attack>defense_monster.attack ->
                destroy_cards = destroy_cards ++ [{target_player_id,target_card_index}]
                new_target_graveyardcards = target_player_battle_info.graveyardcards++[defense_monster.id]
                new_target_monster_card_zone = Dict.delete target_player_battle_info.monster_card_zone,target_card_index
                damage_player_id = target_player_id
                hp_damage = attack_monster.attack - defense_monster.attack
                if hp_damage>target_player_battle_info.curhp do
                  hp_damage = target_player_battle_info.curhp
                end
                new_target_curhp = target_player_battle_info.curhp - hp_damage
                new_target_player_battle_info = target_player_battle_info.update(curhp: new_target_curhp,monster_card_zone: new_target_monster_card_zone,graveyardcards: new_target_graveyardcards)
                new_source_monster_card_zone = Dict.put source_player_battle_info.monster_card_zone,source_card_index,attack_monster.attacked(true)
                new_source_player_battle_info = source_player_battle_info.update(monster_card_zone: new_source_monster_card_zone)
                new_battle_data = battle_data.update([{target_player_atom,new_target_player_battle_info},{source_player_atom,new_source_player_battle_info}])
                if new_target_curhp <= 0 do
                  send self,:battle_end
                end

              # attacker dead and update the attack player's hp
              attack_monster.attack<defense_monster.attack ->
                destroy_cards = destroy_cards ++ [{source_player_id,source_card_index}]
                new_source_graveyardcards = source_player_battle_info.graveyardcards++[attack_monster.id]
                new_source_monster_card_zone = Dict.delete source_player_battle_info.monster_card_zone,source_card_index
                damage_player_id = source_player_id
                hp_damage = defense_monster.attack - attack_monster.attack
                if hp_damage>source_player_battle_info.curhp do
                  hp_damage = source_player_battle_info.curhp
                end
                new_source_curhp = source_player_battle_info.curhp - hp_damage
                new_source_player_battle_info = source_player_battle_info.update(curhp: new_source_curhp,monster_card_zone: new_source_monster_card_zone,graveyardcards: new_source_graveyardcards)
                new_battle_data = battle_data.update([{source_player_atom,new_source_player_battle_info}])
                if new_source_curhp <= 0 do
                  send self,:battle_end
                end

              # destroy all
              attack_monster.attack == defense_monster.attack ->
                destroy_cards = destroy_cards ++ [{source_player_id,source_card_index},{target_player_id,target_card_index}]
                new_source_graveyardcards = source_player_battle_info.graveyardcards++[attack_monster.id]
                new_target_graveyardcards = target_player_battle_info.graveyardcards++[defense_monster.id]
                new_source_monster_card_zone = Dict.delete source_player_battle_info.monster_card_zone,source_card_index
                new_target_monster_card_zone = Dict.delete target_player_battle_info.monster_card_zone,target_card_index
                new_source_player_battle_info = source_player_battle_info.update(monster_card_zone: new_source_monster_card_zone,graveyardcards: new_source_graveyardcards)
                new_target_player_battle_info = target_player_battle_info.update(monster_card_zone: new_target_monster_card_zone,graveyardcards: new_target_graveyardcards)
                new_battle_data = battle_data.update([{target_player_atom,new_target_player_battle_info},{source_player_atom,new_source_player_battle_info}])
            end

          {:attack,defense_state} ->
            cond do
                # defender get damage
              attack_monster.attack>defense_monster.defend ->
                destroy_cards = destroy_cards ++ [{target_player_id,target_card_index}]
                new_target_graveyardcards = target_player_battle_info.graveyardcards++[defense_monster.id]
                new_target_monster_card_zone = Dict.delete target_player_battle_info.monster_card_zone,target_card_index
                damage_player_id = target_player_id
                hp_damage = attack_monster.attack - defense_monster.defend
                if hp_damage>target_player_battle_info.curhp do
                  hp_damage = target_player_battle_info.curhp
                end
                new_target_curhp = target_player_battle_info.curhp - hp_damage                        
                new_target_player_battle_info = target_player_battle_info.update(curhp: new_target_curhp,
                  monster_card_zone: new_target_monster_card_zone,graveyardcards: new_target_graveyardcards)
                new_source_monster_card_zone = Dict.put source_player_battle_info.monster_card_zone,source_card_index,attack_monster.attacked(true)
                new_source_player_battle_info = source_player_battle_info.update(monster_card_zone: new_source_monster_card_zone)
                new_battle_data = battle_data.update([{target_player_atom,new_target_player_battle_info},{source_player_atom,new_source_player_battle_info}])
                if new_target_curhp <= 0 do
                  send self,:battle_end
                end
                
                # attacker get damage
              attack_monster.attack<defense_monster.defend ->
                damage_player_id = source_player_id
                hp_damage = defense_monster.defend - attack_monster.attack
                if hp_damage>source_player_battle_info.curhp do
                  hp_damage = source_player_battle_info.curhp
                end
                new_source_curhp = source_player_battle_info.curhp - hp_damage
                new_source_monster_card_zone = Dict.put source_player_battle_info.monster_card_zone,source_card_index,attack_monster.attacked(true)
                new_source_player_battle_info = source_player_battle_info.update(curhp: new_source_curhp,monster_card_zone: new_source_monster_card_zone)
                if defense_state == :defense_down do
                  new_target_monster_card_zone = Dict.put target_player_battle_info.monster_card_zone,target_card_index,defense_monster.update(presentation: :defense_up)
                  new_target_player_battle_info = target_player_battle_info.monster_card_zone new_target_monster_card_zone
                  new_battle_data = battle_data.update([{source_player_atom,new_source_player_battle_info},
                    {target_player_atom,new_target_player_battle_info}])
                else
                  new_battle_data = battle_data.update([{source_player_atom,new_source_player_battle_info}])
                end
                if new_source_curhp <= 0 do
                  send self,:battle_end
                end
                # no one is destroyed
              attack_monster.attack == defense_monster.defend ->
                new_source_monster_card_zone = Dict.put source_player_battle_info.monster_card_zone,source_card_index,attack_monster.attacked(true)
                new_source_player_battle_info = source_player_battle_info.update(monster_card_zone: new_source_monster_card_zone)
                if defense_state == :defense_down do
                  new_target_monster_card_zone = Dict.put target_player_battle_info.monster_card_zone,target_card_index,defense_monster.update(presentation: :defense_up)
                  new_target_player_battle_info = target_player_battle_info.monster_card_zone new_target_monster_card_zone
                  new_battle_data = battle_data.update([{target_player_atom,new_target_player_battle_info}])
                else
                  new_battle_data = battle_data.update([{source_player_atom,new_source_player_battle_info}])
                end
            end
          _->
            result = :card_is_not_attack_state
        end
    end    

    if result==:ok do
      message = Yugioh.Proto.PT12.write(:attack,[source_card_index,target_card_index,defense_monster.id,damage_player_id,hp_damage,destroy_cards,
        player1_id,new_battle_data.player1_battle_info.graveyardcards,player2_id,new_battle_data.player2_battle_info.graveyardcards])
      send battle_data.player1_battle_info.player_pid , {:send,message}
      send battle_data.player2_battle_info.player_pid , {:send,message}
    end
    Lager.debug "after attack battle data [~p]",[new_battle_data]
    set_and_reply new_battle_data,result
  end
        
  defcall attack(_player_id,_source_card_index,_target_card_index),state: BattleData[turn_count: turn_count], when: turn_count == 1 do
    reply :cant_attack_at_first_turn
  end

  defcall attack(_player_id,_source_card_index,_target_card_index) do
    reply :attack_in_invalid_phase
  end

  defcall change_phase_to(_player_id,phase),state: battle_data do
    now_phase = battle_data.phase
    case phase do
      :bp when now_phase in [:mp1]->
        new_battle_data = battle_data.phase(:bp)
        message = Yugioh.Proto.PT12.write(:change_phase_to,phase)
        send battle_data.player1_battle_info.player_pid , {:send,message}
        send battle_data.player2_battle_info.player_pid , {:send,message}
        set_and_reply new_battle_data,:ok
      :mp2 when now_phase in [:mp1,:bp]->
        new_battle_data = battle_data.phase(:mp2)
        message = Yugioh.Proto.PT12.write(:change_phase_to,phase)
        send battle_data.player1_battle_info.player_pid , {:send,message}
        send battle_data.player2_battle_info.player_pid , {:send,message}
        set_and_reply new_battle_data,:ok
      :ep when now_phase in [:mp1,:bp,:mp2]->
        message = Yugioh.Proto.PT12.write(:change_phase_to,phase)
        send battle_data.player1_battle_info.player_pid , {:send,message}
        send battle_data.player2_battle_info.player_pid , {:send,message}
        send self , :new_turn_draw_phase
        reply :ok
      _->
        reply :invalid_phase_change
    end        
  end  
  
  defcast init_cast(player1_pid,player2_pid) do

    player1_state = :gen_server.call(player1_pid,:player_state)
    player2_state = :gen_server.call(player2_pid,:player_state)

    :ok = :gen_server.call(player1_pid,{:update_player_state,player1_state.battle_pid(self)})
    :ok = :gen_server.call(player2_pid,{:update_player_state,player2_state.battle_pid(self)})

    :random.seed(:erlang.now)

    player1_deckcards = Enum.shuffle(player1_state.deck)
    player2_deckcards = Enum.shuffle(player2_state.deck)

    {player1_handcards,player1_deckcards} = Enum.split(player1_deckcards,5)
    {player2_handcards,player2_deckcards} = Enum.split(player2_deckcards,5)

    player1_battle_info = BattleInfo[player_pid: player1_pid,maxhp: player1_state.hp,curhp: player1_state.hp,handcards: player1_handcards,
    deckcards: player1_deckcards,socket: player1_state.socket]
    player2_battle_info = BattleInfo[player_pid: player2_pid,maxhp: player2_state.hp,curhp: player2_state.hp,handcards: player2_handcards,
    deckcards: player2_deckcards,socket: player2_state.socket]

    # wait for player to decide who first 
    # order_game
    # send battle info
    # random 5 cards 
    message_data = [1,player1_state.id,:dp,player1_state,player1_battle_info,player2_state,hide_handcards(player2_battle_info)]
    send player1_pid , {:send,Yugioh.Proto.PT11.write(:battle_start,message_data)}
    message_data = [1,player1_state.id,:dp,player1_state,hide_handcards(player1_battle_info),player2_state,player2_battle_info]
    send player2_pid , {:send,Yugioh.Proto.PT11.write(:battle_start,message_data)}

    new_state BattleData[turn_count: 0,phase: :wait_load_finish_1,operator_id: player1_state.id,player1_id: player1_state.id,player2_id: player2_state.id,
                          player1_battle_info: player1_battle_info,player2_battle_info: player2_battle_info]
  end

  defcast stop_cast,state: state do
    {:stop, :normal, state}
  end

  definfo :new_turn_draw_phase,state: battle_data = BattleData[player1_id: player1_id,player2_id: player2_id] do

    {player_atom,new_operator_id,player_battle_info} = 
    if battle_data.turn_count==0 do
        case battle_data.operator_id do
          ^player1_id->
            {:player1_battle_info,player1_id,battle_data.player1_battle_info}
          ^player2_id->
            {:player2_battle_info,player2_id,battle_data.player2_battle_info}
        end
    else      
        case battle_data.operator_id do
          ^player1_id->
            {:player2_battle_info,player2_id,battle_data.player2_battle_info}
          ^player2_id->
            {:player1_battle_info,player1_id,battle_data.player1_battle_info}
        end
    end
    [draw_card_id|new_deckcards] = player_battle_info.deckcards
    new_handcards = player_battle_info.handcards ++ [draw_card_id]
    new_monster_card_zone = Enum.map(player_battle_info.monster_card_zone,fn({index,monster})-> {index,monster.turn_reset} end)
    new_player_battle_info = player_battle_info.update(deck: new_deckcards,handcards: new_handcards,monster_card_zone: new_monster_card_zone)
    new_battle_data = battle_data.update([{player_atom,new_player_battle_info},{:turn_count,battle_data.turn_count+1},
      {:phase,:dp},{:normal_summoned,false},{:operator_id,new_operator_id}])
    
    case new_operator_id do
      ^player1_id->
        message = Yugioh.Proto.PT12.write(:new_turn_draw,[new_battle_data.turn_count,new_battle_data.phase,new_battle_data.operator_id,draw_card_id])
        send battle_data.player1_battle_info.player_pid , {:send,message}
        message = Yugioh.Proto.PT12.write(:new_turn_draw,[new_battle_data.turn_count,new_battle_data.phase,new_battle_data.operator_id,0])
        send battle_data.player2_battle_info.player_pid , {:send,message}
      ^player2_id->
        message = Yugioh.Proto.PT12.write(:new_turn_draw,[new_battle_data.turn_count,new_battle_data.phase,new_battle_data.operator_id,0])
        send battle_data.player1_battle_info.player_pid , {:send,message}
        message = Yugioh.Proto.PT12.write(:new_turn_draw,[new_battle_data.turn_count,new_battle_data.phase,new_battle_data.operator_id,draw_card_id])
        send battle_data.player2_battle_info.player_pid , {:send,message}
    end
    
    send self, :standby_phase
    new_state new_battle_data
  end

  definfo :standby_phase,state: battle_data do
      new_battle_data = battle_data.phase(:sp)
      send battle_data.player1_battle_info.player_pid , {:send,Yugioh.Proto.PT12.write(:change_phase_to,:sp)}
      send battle_data.player2_battle_info.player_pid , {:send,Yugioh.Proto.PT12.write(:change_phase_to,:sp)}
      send self , :main_phase_1
      new_state new_battle_data
  end

  definfo :main_phase_1,state: battle_data do
      new_battle_data = battle_data.phase(:mp1)
      send battle_data.player1_battle_info.player_pid , {:send,Yugioh.Proto.PT12.write(:change_phase_to,:mp1)}
      send battle_data.player2_battle_info.player_pid , {:send,Yugioh.Proto.PT12.write(:change_phase_to,:mp1)}
      new_state new_battle_data
  end

  definfo :battle_phase,state: battle_data do
      new_battle_data = battle_data.phase(:bp)
      new_state new_battle_data
  end

  definfo :main_phase_2,state: battle_data do
      new_battle_data = battle_data.phase(:mp2)
      new_state new_battle_data
  end

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

  # make all cards id to 0
  defp hide_handcards battle_info do
    cards_size = length battle_info.handcards
    battle_info.handcards(Enum.take Stream.cycle([0]),cards_size)
  end
end