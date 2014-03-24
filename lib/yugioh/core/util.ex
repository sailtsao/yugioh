defmodule Util do
  require Lager

  def filter_id_index_list id_index_list,:monster_card,attribute,level_limit,nil do
    Enum.filter id_index_list,fn({card_id,_index})->
      card_data = Data.Cards.get(card_id)
      if attribute == :none do
        card_data.level > level_limit
      else
        (card_data.level > level_limit)&&(card_data.attribute == attribute)
      end
    end
  end

  def filter_id_index_list id_index_list,:monster_card,attribute,level_limit,exclude_index do
    Enum.filter id_index_list,fn({card_id,index})->
      card_data = Data.Cards.get(card_id)
      if attribute == :none do
        (card_data.level > level_limit)&&(index != exclude_index)
      else
        (card_data.level > level_limit)&&(card_data.attribute == attribute)&&(index != exclude_index)
      end
    end
  end

  def filter_id_index_list id_index_list,_card_type,_attribute,_level_limit,nil do
    id_index_list
  end

  def filter_id_index_list id_index_list,_card_type,_attribute,_level_limit,exclude_index do
    Enum.filter id_index_list,fn({_,index})->
      index != exclude_index
    end
  end


  # def filter_id_index_list id_index_list,card_type,attribute,level_limit do
  #   Lager.debug "id_index_list [~p]",[id_index_list]
  #   Enum.filter id_index_list,fn({card_id,_})->
  #     card_data = Data.Cards.get(card_id)
  #     if card_type == :none do
  #         if attribute == :none do
  #           card_data.level > level_limit
  #         else
  #           (card_data.level > level_limit)&&(card_data.attribute == attribute)
  #         end
  #       end
  #     else
  #       if card_data.card_type == card_type do
  #         if attribute == :none do
  #           card_data.level > level_limit
  #         else
  #           (card_data.level > level_limit)&&(card_data.attribute == attribute)
  #         end
  #       else
  #         false
  #       end
  #     end

  #   end
  # end


  def get_id_index_list_from_scene(player_battle_info = BattlePlayerInfo[id: player_id],scene_type,card_type,attribute,level_limit,{player_id,scene_type,index})
  when scene_type in [:handcard_zone,:graveyard_zone,:deck_zone,:extra_deck_zone,:banished_zone] do
    cards = player_battle_info.get_cards_of_scene scene_type
    id_index_list = Enum.with_index cards
    filter_id_index_list id_index_list,card_type,attribute,level_limit,index
  end

  def get_id_index_list_from_scene(player_battle_info,scene_type,card_type,attribute,level_limit,_)
  when scene_type in [:handcard_zone,:graveyard_zone,:deck_zone,:extra_deck_zone,:banished_zone] do
    cards = player_battle_info.get_cards_of_scene scene_type
    id_index_list = Enum.with_index cards
    filter_id_index_list id_index_list,card_type,attribute,level_limit,nil
  end

  def get_id_index_list_from_scene(player_battle_info = BattlePlayerInfo[id: player_id],scene_type,card_type,attribute,level_limit,{player_id,scene_type,index})
  when scene_type in [:monster_zone,:spell_trap_zone] do
    cards = player_battle_info.get_cards_of_scene scene_type
    id_index_list = Enum.map cards,fn({index,card})-> {card.id,index} end
    filter_id_index_list id_index_list,card_type,attribute,level_limit,index
  end

  def get_id_index_list_from_scene(player_battle_info,scene_type,card_type,attribute,level_limit,_)
  when scene_type in [:monster_zone,:spell_trap_zone] do
    cards = player_battle_info.get_cards_of_scene scene_type
    id_index_list = Enum.map cards,fn({index,card})-> {card.id,index} end
    filter_id_index_list id_index_list,card_type,attribute,level_limit,nil
  end

  def get_id_index_list_from_scene _,_,_,_,_,_ do
    []
  end
end