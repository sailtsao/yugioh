defmodule Yugioh.Core.BattleCore do
  require Lager

  def send_message player_pid,message,params do
    message_data = Yugioh.Proto.PT12.write(message,params)
    send player_pid,{:send,message_data}
  end  

  def hide_handcards battle_info do
    cards_size = length battle_info.handcards    
    battle_info.handcards Enum.take(Stream.cycle([0]),cards_size)
  end

  def get_player_battle_info player_id,battle_data = BattleData[player1_id: player1_id,player2_id: player2_id] do
    case player_id do
      ^player1_id ->
        battle_data.player1_battle_info
      ^player2_id ->
        battle_data.player2_battle_info
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
  
  # 0 mean that we have not start our battle,we start the new turn after we collected two battle load finish message
  def get_new_turn_operator_id(battle_data = BattleData[turn_count: turn_count,player1_id: player1_id,player2_id: player2_id]) 
  when turn_count == 0 do
    case battle_data.operator_id do
      ^player1_id->
        player1_id
      ^player2_id->
        player2_id
    end
  end

  def get_new_turn_operator_id(battle_data = BattleData[player1_id: player1_id,player2_id: player2_id]) do
    case battle_data.operator_id do
      ^player1_id->
        player2_id
      ^player2_id->
        player1_id
    end
  end  

  def get_presentation_operation presentation do
    case presentation do
      :attack ->
        :change_to_defense_present_operation
      :defense_down ->
        :reverse_operation
      :defense_up ->
        :change_to_attack_present_operation
    end
  end
  

  def get_handcard_operations card_level,monster_summoned_count do
    case card_level do
      x when x==5 or x==6 ->        
        if monster_summoned_count >=1 do
          [:summon_operation,:place_operation]
        else
          []
        end
      x when x==7 or x==8 ->
        if monster_summoned_count >=2 do
          [:summon_operation,:place_operation]
        else
          []
        end
      x when x>8 ->
        [:summon_operation,:place_operation]
      _ ->
        [:summon_operation,:place_operation]
    end
  end

  def attack_card(player_id,source_card_index,target_card_index,
    battle_data = BattleData[player1_id: player1_id,player2_id: player2_id]) do
    
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
              attack_monster.attack>defense_monster.defense ->
                destroy_cards = destroy_cards ++ [{target_player_id,target_card_index}]
                new_target_graveyardcards = target_player_battle_info.graveyardcards++[defense_monster.id]
                new_target_monster_card_zone = Dict.delete target_player_battle_info.monster_card_zone,target_card_index
                new_target_player_battle_info = target_player_battle_info.update(
                  monster_card_zone: new_target_monster_card_zone,graveyardcards: new_target_graveyardcards)
                new_source_monster_card_zone = Dict.put source_player_battle_info.monster_card_zone,source_card_index,attack_monster.attacked(true)
                new_source_player_battle_info = source_player_battle_info.update(monster_card_zone: new_source_monster_card_zone)
                new_battle_data = battle_data.update([{target_player_atom,new_target_player_battle_info},{source_player_atom,new_source_player_battle_info}])
                
                # attacker get damage
              attack_monster.attack<defense_monster.defense ->
                damage_player_id = source_player_id
                new_source_monster_card_zone = Dict.put source_player_battle_info.monster_card_zone,source_card_index,attack_monster.attacked(true)
                new_source_player_battle_info = source_player_battle_info.update(monster_card_zone: new_source_monster_card_zone)
                if defense_state == :defense_down do
                  new_target_monster_card_zone = Dict.put target_player_battle_info.monster_card_zone,target_card_index,defense_monster.presentation(:defense_up)
                  new_target_player_battle_info = target_player_battle_info.monster_card_zone new_target_monster_card_zone
                  new_battle_data = battle_data.update([{source_player_atom,new_source_player_battle_info},
                    {target_player_atom,new_target_player_battle_info}])
                else
                  new_battle_data = battle_data.update([{source_player_atom,new_source_player_battle_info}])
                end
                # no one is destroyed
              attack_monster.attack == defense_monster.defense ->
                new_source_monster_card_zone = Dict.put source_player_battle_info.monster_card_zone,source_card_index,attack_monster.attacked(true)
                new_source_player_battle_info = source_player_battle_info.update(monster_card_zone: new_source_monster_card_zone)
                if defense_state == :defense_down do
                  new_target_monster_card_zone = Dict.put target_player_battle_info.monster_card_zone,target_card_index,defense_monster.presentation(:defense_up)
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
      # TODO
      attack_effect = Effect.new(type: :attack_effect,params: "",targets: [])
      message_data = Yugioh.Proto.PT12.write(:effects,[attack_effect])
      # message = Yugioh.Proto.PT12.write(:attack,[source_card_index,target_card_index,defense_monster.id,damage_player_id,hp_damage,destroy_cards,
      #   player1_id,new_battle_data.player1_battle_info.graveyardcards,player2_id,new_battle_data.player2_battle_info.graveyardcards])
      send battle_data.player1_battle_info.player_pid , {:send,message_data}
      send battle_data.player2_battle_info.player_pid , {:send,message_data}
    end    
    {result,new_battle_data}
  end

  def attack_player player_id,source_card_index,battle_data = BattleData[player1_id: player1_id,player2_id: player2_id] do

    {source_player_id,target_player_id,source_player_atom,source_player_battle_info,target_player_atom,target_player_battle_info} = case player_id do
      ^player1_id->
        {player1_id,player2_id,:player1_battle_info,battle_data.player1_battle_info,:player2_battle_info,battle_data.player2_battle_info}
      ^player2_id->
        {player2_id,player1_id,:player2_battle_info,battle_data.player2_battle_info,:player1_battle_info,battle_data.player1_battle_info}
    end

    attack_monster = Dict.get source_player_battle_info.monster_card_zone,source_card_index
    hp_damage = 0
    result = :ok
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
        target_player_battle_info = target_player_battle_info.curhp - hp_damage |> target_player_battle_info.curhp 

        source_player_battle_info = source_player_battle_info.monster_card_zone
        |> Dict.put source_card_index,attack_monster.attacked(true)
        |> source_player_battle_info.monster_card_zone
         
        battle_data = battle_data.update([{target_player_atom,target_player_battle_info},
            {source_player_atom,source_player_battle_info}])
        if target_player_battle_info.curhp <= 0 do
          send self,:battle_end
        end
    end
    if result == :ok do
      # TODO
      attack_effect = Effect.new(type: :attack_effect,params: "",targets: [])
      message_data = Yugioh.Proto.PT12.write(:effects,[attack_effect])
      # message = Yugioh.Proto.PT12.write(:attack,[source_card_index,11,0,target_player_id,hp_damage,[],
      #   source_player_id,source_player_battle_info.graveyardcards,target_player_id,target_player_battle_info.graveyardcards])
      send battle_data.player1_battle_info.player_pid , {:send,message_data}
      send battle_data.player2_battle_info.player_pid , {:send,message_data}
    end
    {result,battle_data}
  end  
end