defmodule Yugioh.Battle do
  require Lager
  use GenServer.Behaviour
  alias Yugioh.Data.Cards

  def start({player1_pid,player2_pid}) do
    :gen_server.start(__MODULE__,{player1_pid,player2_pid},[])
  end

  def stop(pid) do
    if is_pid(pid), do: :gen_server.cast(pid, :stop)
  end

  def init({player1_pid,player2_pid}) do
    Lager.debug "battle process [~p] for player [~p] created",[self,{player1_pid,player2_pid}]
    :gen_server.cast self,{:init,player1_pid,player2_pid}
    {:ok,{}}
  end


  def handle_call(:battle_load_finish,from,battle_data=BattleData[phase: phase]) when phase ==0 or phase == 1 do
    case phase do
      0->
        {:reply,:ok,battle_data.phase(phase + 1)}
      1->
        send self , :new_turn_draw_phase
        {:reply,:ok,battle_data.phase(:dp)}
    end
  end

  def handle_call(_,from,battle_data=BattleData[phase: phase]) when phase ==0 or phase == 1 do
    {:reply,:battle_not_ready_yet,battle_data}
  end  
  
  def handle_call({:summon,player_id,handcards_index,summon_type},from,battle_data) do    
    Lager.debug "battle before summon battle data [~p]",[battle_data]
    player1_id = battle_data.player1_id
    player2_id = battle_data.player2_id        
    {player_atom,player_battle_info} = case player_id do
      ^player1_id->
        {:player1_battle_info,battle_data.player1_battle_info}
      ^player2_id->
        {:player2_battle_info,battle_data.player2_battle_info}
    end
    cond do
      battle_data.summoned == true ->
        {:reply, :cant_summon_twice_in_one_turn, battle_data}
      Dict.size(player_battle_info.summon_cards)==5->
        {:reply, :cant_summon_more, battle_data}
      battle_data.phase == :mp1 or battle_data.phase ==:mp2 ->        

        summon_card_id = Enum.at(player_battle_info.handcards,handcards_index)

        new_handcards = List.delete_at(player_battle_info.handcards,handcards_index)

        avaible_pos = :lists.subtract([0,1,2,3,4],Dict.keys(player_battle_info.summon_cards))
        [pos|_] = avaible_pos
        new_summon_cards = Dict.put(player_battle_info.summon_cards,pos,{summon_card_id,summon_type})

        new_player_battle_info = player_battle_info.update(handcards: new_handcards,summon_cards: new_summon_cards)

        new_battle_data = battle_data.update([{player_atom,new_player_battle_info},{:summoned,true}])
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
        {:reply, :ok, new_battle_data}
      true->
        {:reply, :summon_in_invalid_phase, battle_data}
    end    
  end
  
  def handle_call({:flip_card,player_id,card_index},from,
    battle_data = BattleData[phase: phase,player1_id: player1_id,player2_id: player2_id,flipped_cards: flipped_cards])
  when phase == :mp1 or phase == :mp2 do

    {player_atom,player_battle_info} = case player_id do
      ^player1_id->
        {:player1_battle_info,battle_data.player1_battle_info}
      ^player2_id->
        {:player2_battle_info,battle_data.player2_battle_info}
    end

    if(card_index in flipped_cards) do      
      {:reply, :flip_many_times, battle_data}
    else
      card_id = 0
      {result,new_summon_cards,new_status} = case Dict.get(player_battle_info.summon_cards,card_index) do
        {card_id,:defense_down}->
          {:ok,Dict.put(player_battle_info.summon_cards,card_index,{card_id,:attack}),:attack}
        {card_id,:defense_up}->
          {:ok,Dict.put(player_battle_info.summon_cards,card_index,{card_id,:attack}),:attack}
        {card_id,:attack}->
          {:ok,Dict.put(player_battle_info.summon_cards,card_index,{card_id,:defense_up}),:defense_up}
        nil->
          {:invalid_flip_card_index, nil,nil}
      end
      case result do
        :ok->
          new_player_battle_info = player_battle_info.update(summon_cards: new_summon_cards)
          new_battle_data = battle_data.update([{player_atom,new_player_battle_info},{:flipped_cards,flipped_cards++[card_index]}])
          message_data = Yugioh.Proto.PT12.write(:flip_card,[player_id,card_index,card_id,new_status])
          send battle_data.player1_battle_info.player_pid , {:send,message_data}
          send battle_data.player2_battle_info.player_pid , {:send,message_data}
          {:reply, :ok, new_battle_data}
        reason->
          {:reply, reason, battle_data}
      end
    end    
  end

  def handle_call({:flip_card,_,_},_,battle_data)do
    {:reply, :flip_card_in_invalid_phase,battle_data}
  end

# attack player directly
  def handle_call({:attack,player_id,source_card_index,11},from,
    battle_data = BattleData[phase: phase])
  when phase==:bp do
    Lager.debug "before attack battle data [~p]",[battle_data]
    player1_id = battle_data.player1_id
    player2_id = battle_data.player2_id
    {source_player_id,target_player_id,source_player_atom,source_player_battle_info,target_player_atom,target_player_battle_info} = case player_id do
      ^player1_id->
        {player1_id,player2_id,:player1_battle_info,battle_data.player1_battle_info,:player2_battle_info,battle_data.player2_battle_info}
      ^player2_id->
        {player2_id,player1_id,:player2_battle_info,battle_data.player2_battle_info,:player1_battle_info,battle_data.player1_battle_info}
    end
    {attacker_id,attacker_summon_type} = Dict.get source_player_battle_info.summon_cards,source_card_index
    attacker_data = Cards.get(attacker_id)
    hp_damage = 0
    result = :ok
    new_battle_data = battle_data
    cond do
      Dict.size(target_player_battle_info.summon_cards)!=0->
        result = :attack_directly_invalid
      true->
        hp_damage = attacker_data.attack
        if hp_damage>target_player_battle_info.curhp do
          hp_damage = target_player_battle_info.curhp
        end
        new_target_curhp = target_player_battle_info.curhp - hp_damage
        new_target_player_battle_info = target_player_battle_info.update(curhp: new_target_curhp)
        new_battle_data = battle_data.update([{target_player_atom,new_target_player_battle_info}])
        if new_target_curhp <= 0 do
          send self,:battle_end
        end
    end
    if result==:ok do
      message = Yugioh.Proto.PT12.write(:attack,[source_card_index,11,0,target_player_id,hp_damage,[],
        source_player_id,source_player_battle_info.graveyard_cards,target_player_id,target_player_battle_info.graveyard_cards])
      send battle_data.player1_battle_info.player_pid , {:send,message}
      send battle_data.player2_battle_info.player_pid , {:send,message}
    end
    Lager.debug "after attack battle data [~p]",[new_battle_data]
    {:reply, result, new_battle_data}
  end

  # attack with cards
  def handle_call({:attack,player_id,source_card_index,target_card_index},from,battle_data = BattleData[phase: phase]) 
  when phase==:bp do
    Lager.debug "before attack battle data [~p]",[battle_data]
    player1_id = battle_data.player1_id
    player2_id = battle_data.player2_id
    {source_player_id,target_player_id,source_player_atom,source_player_battle_info,target_player_atom,target_player_battle_info} = case player_id do
      ^player1_id->
        {player1_id,player2_id,:player1_battle_info,battle_data.player1_battle_info,:player2_battle_info,battle_data.player2_battle_info}
      ^player2_id->
        {player2_id,player1_id,:player2_battle_info,battle_data.player2_battle_info,:player1_battle_info,battle_data.player1_battle_info}
    end
    # TODO: pelase consider the situation that there is no defender at all, how to directly attack player.
    {attacker_id,attacker_summon_type} = Dict.get source_player_battle_info.summon_cards,source_card_index
    {defender_id,defender_summon_type} = Dict.get target_player_battle_info.summon_cards,target_card_index
    attacker_data = Cards.get(attacker_id)
    defender_data = Cards.get(defender_id)
    damage_player_id = target_player_id
    hp_damage = 0
    destroy_cards = []
    result = :ok
    new_battle_data = battle_data
    case {attacker_summon_type,defender_summon_type} do
      {:attack,:attack} ->
        cond do
            # defender dead and update the defense player's hp
          attacker_data.attack>defender_data.attack ->
            destroy_cards = destroy_cards ++ [{target_player_id,target_card_index}]
            new_target_graveyard_cards = target_player_battle_info.graveyard_cards++[defender_id]
            new_target_summon_cards = Dict.delete target_player_battle_info.summon_cards,target_card_index
            damage_player_id = target_player_id
            hp_damage = attacker_data.attack - defender_data.attack
            if hp_damage>target_player_battle_info.curhp do
              hp_damage = target_player_battle_info.curhp
            end
            new_target_curhp = target_player_battle_info.curhp - hp_damage
            new_target_player_battle_info = target_player_battle_info.update(curhp: new_target_curhp,summon_cards: new_target_summon_cards,graveyard_cards: new_target_graveyard_cards)
            new_battle_data = battle_data.update([{target_player_atom,new_target_player_battle_info}])
            if new_target_curhp <= 0 do
              send self,:battle_end
            end

          # attacker dead and update the attack player's hp
          attacker_data.attack<defender_data.attack ->
            destroy_cards = destroy_cards ++ [{source_player_id,source_card_index}]
            new_source_graveyard_cards = source_player_battle_info.graveyard_cards++[attacker_id]
            new_source_summon_cards = Dict.delete source_player_battle_info.summon_cards,source_card_index
            damage_player_id = source_player_id
            hp_damage = defender_data.attack - attacker_data.attack
            if hp_damage>source_player_battle_info.curhp do
              hp_damage = source_player_battle_info.curhp
            end
            new_source_curhp = source_player_battle_info.curhp - hp_damage
            new_source_player_battle_info = source_player_battle_info.update(curhp: new_source_curhp,summon_cards: new_source_summon_cards,graveyard_cards: new_source_graveyard_cards)
            new_battle_data = battle_data.update([{source_player_atom,new_source_player_battle_info}])
            if new_source_curhp <= 0 do
              send self,:battle_end
            end

          # destroy all
          attacker_data.attack == defender_data.attack ->
            destroy_cards = destroy_cards ++ [{source_player_id,source_card_index},{target_player_id,target_card_index}]
            new_source_graveyard_cards = source_player_battle_info.graveyard_cards++[attacker_id]
            new_target_graveyard_cards = target_player_battle_info.graveyard_cards++[defender_id]
            new_source_summon_cards = Dict.delete source_player_battle_info.summon_cards,source_card_index
            new_target_summon_cards = Dict.delete target_player_battle_info.summon_cards,target_card_index
            new_source_player_battle_info = source_player_battle_info.update(summon_cards: new_source_summon_cards,graveyard_cards: new_source_graveyard_cards)
            new_target_player_battle_info = target_player_battle_info.update(summon_cards: new_target_summon_cards,graveyard_cards: new_target_graveyard_cards)
            new_battle_data = battle_data.update([{target_player_atom,new_target_player_battle_info},{source_player_atom,new_source_player_battle_info}])
        end

      {:attack,defense_state} ->
        cond do
            # defender get damage
          attacker_data.attack>defender_data.defend ->
            destroy_cards = destroy_cards ++ [{target_player_id,target_card_index}]
            new_target_graveyard_cards = target_player_battle_info.graveyard_cards++[defender_id]
            new_target_summon_cards = Dict.delete target_player_battle_info.summon_cards,target_card_index
            damage_player_id = target_player_id
            hp_damage = attacker_data.attack - defender_data.defend
            if hp_damage>target_player_battle_info.curhp do
              hp_damage = target_player_battle_info.curhp
            end
            new_target_curhp = target_player_battle_info.curhp - hp_damage                        
            new_target_player_battle_info = target_player_battle_info.update(curhp: new_target_curhp,
              summon_cards: new_target_summon_cards,graveyard_cards: new_target_graveyard_cards)
            new_battle_data = battle_data.update([{target_player_atom,new_target_player_battle_info}])
            if new_target_curhp <= 0 do
              send self,:battle_end
            end
            
            # attacker get damage
          attacker_data.attack<defender_data.defend ->
            damage_player_id = source_player_id
            hp_damage = defender_data.defend - attacker_data.attack
            if hp_damage>source_player_battle_info.curhp do
              hp_damage = source_player_battle_info.curhp
            end
            new_source_curhp = source_player_battle_info.curhp - hp_damage
            new_source_player_battle_info = source_player_battle_info.curhp new_source_curhp
            if defense_state == :defense_down do
              new_target_summon_cards = Dict.put target_player_battle_info.summon_cards,target_card_index,{defender_id,:defense_up}
              new_target_player_battle_info = target_player_battle_info.summon_cards new_target_summon_cards
              new_battle_data = battle_data.update([{source_player_atom,new_source_player_battle_info},
                {target_player_atom,new_target_player_battle_info}])
            else
              new_battle_data = battle_data.update([{source_player_atom,new_source_player_battle_info}])
            end
            if new_source_curhp <= 0 do
              send self,:battle_end
            end
            # no one is destroyed
          attacker_data.attack == defender_data.defend ->
            if defense_state == :defense_down do
              new_target_summon_cards = Dict.put target_player_battle_info.summon_cards,target_card_index,{defender_id,:defense_up}
              new_target_player_battle_info = target_player_battle_info.summon_cards new_target_summon_cards
              new_battle_data = battle_data.update([{target_player_atom,new_target_player_battle_info}])
            end
        end
      _->
        result = :card_is_not_attack_state
    end

    if result==:ok do
      message = Yugioh.Proto.PT12.write(:attack,[source_card_index,target_card_index,defender_id,damage_player_id,hp_damage,destroy_cards,
        player1_id,new_battle_data.player1_battle_info.graveyard_cards,player2_id,new_battle_data.player2_battle_info.graveyard_cards])
      send battle_data.player1_battle_info.player_pid , {:send,message}
      send battle_data.player2_battle_info.player_pid , {:send,message}
    end
    Lager.debug "after attack battle data [~p]",[new_battle_data]
    {:reply, result, new_battle_data}
  end
        
  def handle_call({:attack,_,_,_},_from,battle_data = BattleData[turn_count: turn_count]) when turn_count == 1 do
    {:reply, :cant_attack_at_first_turn, battle_data}
  end

  def handle_call({:attack,_,_,_},_from,battle_data) do
    {:reply, :attack_in_invalid_phase, battle_data}
  end

  def handle_call({:change_phase_to,player_id,phase},from,battle_data) do
    now_phase = battle_data.phase
    case phase do
      :bp when now_phase in [:mp1]->
        new_battle_data = battle_data.phase(:bp)
        message = Yugioh.Proto.PT12.write(:change_phase_to,phase)
        send battle_data.player1_battle_info.player_pid , {:send,message}
        send battle_data.player2_battle_info.player_pid , {:send,message}
        {:reply, :ok, new_battle_data}
      :mp2 when now_phase in [:mp1,:bp]->
        new_battle_data = battle_data.phase(:mp2)
        message = Yugioh.Proto.PT12.write(:change_phase_to,phase)
        send battle_data.player1_battle_info.player_pid , {:send,message}
        send battle_data.player2_battle_info.player_pid , {:send,message}
        {:reply, :ok, new_battle_data}
      :ep when now_phase in [:mp1,:bp,:mp2]->
        message = Yugioh.Proto.PT12.write(:change_phase_to,phase)
        send battle_data.player1_battle_info.player_pid , {:send,message}
        send battle_data.player2_battle_info.player_pid , {:send,message}
        send self , :new_turn_draw_phase
        {:reply, :ok, battle_data}
      _->
        {:reply, :invalid_phase_change, battle_data}
    end        
  end

  def handle_call(_msg, _from, state) do
    reply = :ok
    {:reply, reply, state}
  end
  
  def handle_cast({:init,player1_pid,player2_pid},state) do

    player1_state = :gen_server.call(player1_pid,:player_state)
    player2_state = :gen_server.call(player2_pid,:player_state)

    :ok = :gen_server.call(player1_pid,{:update_player_state,player1_state.battle_pid(self)})
    :ok = :gen_server.call(player2_pid,{:update_player_state,player2_state.battle_pid(self)})

    :random.seed(:erlang.now)

    player1_cards = Enum.shuffle(player1_state.cards)
    player2_cards = Enum.shuffle(player2_state.cards)

    {player1_handcards,player1_cards} = Enum.split(player1_cards,5)
    {player2_handcards,player2_cards} = Enum.split(player2_cards,5)

    player1_battle_info = BattleInfo[player_pid: player1_pid,maxhp: player1_state.hp,curhp: player1_state.hp,handcards: player1_handcards,
    remaincards: player1_cards,socket: player1_state.socket]
    player2_battle_info = BattleInfo[player_pid: player2_pid,maxhp: player2_state.hp,curhp: player2_state.hp,handcards: player2_handcards,
    remaincards: player2_cards,socket: player2_state.socket]

    # wait for player to decide who first 
    # order_game
    # send battle info
    # random 5 cards 
    message_data = [1,player1_state.id,:dp,player1_state,player1_battle_info,player2_state,hide_handcards(player2_battle_info)]
    send player1_pid , {:send,Yugioh.Proto.PT11.write(:battle_start,message_data)}
    message_data = [1,player1_state.id,:dp,player1_state,hide_handcards(player1_battle_info),player2_state,player2_battle_info]
    send player2_pid , {:send,Yugioh.Proto.PT11.write(:battle_start,message_data)}

    {:noreply, BattleData[turn_count: 0,phase: 0,operator_id: player1_state.id,player1_id: player1_state.id,player2_id: player2_state.id,
                          player1_battle_info: player1_battle_info,player2_battle_info: player2_battle_info]}
  end

  def handle_cast(:stop, state) do
    {:stop, :normal, state}
  end

  def handle_cast(_msg, state) do
    {:noreply, state}
  end

  def handle_info(:new_turn_draw_phase,battle_data) do
    player1_id = battle_data.player1_id
    player2_id = battle_data.player2_id
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
    [draw_card_id|new_remaincards] = player_battle_info.remaincards
    new_handcards = player_battle_info.handcards ++ [draw_card_id]
    new_player_battle_info = player_battle_info.update(remaincards: new_remaincards,handcards: new_handcards)
    new_battle_data = battle_data.update([{player_atom,new_player_battle_info},{:turn_count,battle_data.turn_count+1},
      {:phase,:dp},{:summoned,false},{:operator_id,new_operator_id},{:flipped_cards,[]}])
    
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
    
    send self , :standby_phase
    {:noreply,new_battle_data}
  end

  def handle_info(:standby_phase,battle_data) do
      new_battle_data = battle_data.phase(:sp)
      send battle_data.player1_battle_info.player_pid , {:send,Yugioh.Proto.PT12.write(:change_phase_to,:sp)}
      send battle_data.player2_battle_info.player_pid , {:send,Yugioh.Proto.PT12.write(:change_phase_to,:sp)}
      send self , :main_phase_1
      {:noreply,new_battle_data}
  end

  def handle_info(:main_phase_1,battle_data) do
      new_battle_data = battle_data.phase(:mp1)
      send battle_data.player1_battle_info.player_pid , {:send,Yugioh.Proto.PT12.write(:change_phase_to,:mp1)}
      send battle_data.player2_battle_info.player_pid , {:send,Yugioh.Proto.PT12.write(:change_phase_to,:mp1)}
      {:noreply,new_battle_data}
  end

  def handle_info(:battle_phase,battle_data) do
      new_battle_data = battle_data.phase(:bp)
      {:noreply,new_battle_data}
  end

  def handle_info(:main_phase_2,battle_data) do
      new_battle_data = battle_data.phase(:mp2)
      {:noreply,new_battle_data}
  end

  def handle_info(:battle_end,battle_data) do
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
    stop(self)
    {:noreply,battle_data}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end

  def terminate(reason,state) do
    Lager.debug "battle process [~p] state [~p] died for reason [~p]",[self,state,reason]
  end

  def code_change(_oldVsn, state, _extra) do
    {:ok, state}
  end

  def change_phase_to(battle_pid,player_id,phase) do
    :gen_server.call(battle_pid,{:change_phase_to,player_id,phase})
  end

  def summon(battle_pid,player_id,handcards_index,summon_type) do
    :gen_server.call(battle_pid,{:summon,player_id,handcards_index,summon_type})
  end

  def flip_card(battle_pid,player_id,card_index) do
    :gen_server.call(battle_pid,{:flip_card,player_id,card_index})
  end

  def attack(battle_pid,player_id,source_card_index,target_card_index) do
    :gen_server.call(battle_pid,{:attack,player_id,source_card_index,target_card_index})
  end

  def battle_load_finish(battle_pid,player_id) do
    :gen_server.call(battle_pid,:battle_load_finish)
  end

  defp hide_handcards battle_info do
    cards_size = length battle_info.handcards
    battle_info.handcards(Enum.take Stream.cycle([0]),cards_size)
  end
end