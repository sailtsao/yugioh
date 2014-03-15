defmodule BattleCore do
  require Lager

  def destroy_card_to_graveyard(battle_data,player_id,scene_type,pos)
  when scene_type in [:spell_trap_zone,:monster_card_zone] do

    player_battle_info = battle_data.get_player_battle_info player_id
    cards_dict = Util.get_cards_from_scene player_battle_info,scene_type
    card_id = Dict.get(cards_dict,pos).id
    graveyardcards = [card_id|player_battle_info.graveyardcards]
    cards_dict = Dict.drop(cards_dict,[pos])
    scene_atom = IDUtil.get_scene_atom scene_type
    player_battle_info = player_battle_info.update([{:graveyardcards,graveyardcards},{scene_atom,cards_dict}])

    player_atom = battle_data.get_player_atom player_id
    battle_data = battle_data.update([{player_atom,player_battle_info}])

    targets = create_effect_targets player_id,scene_type,[pos]
    move_to_graveyard_effect = create_move_to_graveyard_effect(targets,battle_data)
    message_data = Proto.PT12.write :effects,[move_to_graveyard_effect]
    battle_data.send_message_to_all message_data

    {:ok,battle_data}
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
        params: battle_data.get_graveyard_params_string,
        targets: targets)
  end    
  
end