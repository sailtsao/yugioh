defmodule Proto.PT13 do

  defp parse_deck_string "" do
    HashDict.new
  end

  defp parse_deck_string str do
    deck_list = Enum.map String.split(str,";"),fn(card_str)->
      [card_id_str,card_count_str] = String.split(card_str,",")
      card_id = binary_to_integer card_id_str
      card_count = binary_to_integer card_count_str
      {card_id,card_count}
    end
    HashDict.new(deck_list)
  end


  def read(13000,_) do
    {:ok,{:load_data,[]}}
  end

  def read(13001,bin) do
    <<deck_id::16,rest_bin::binary>> = bin
    {main_deck_string,rest_bin} = ProtoUtil.read_string rest_bin
    main_deck_dict = parse_deck_string main_deck_string
    {extra_deck_string,rest_bin} = ProtoUtil.read_string rest_bin
    extra_deck_dict = parse_deck_string extra_deck_string
    {side_deck_string,rest_bin} = ProtoUtil.read_string rest_bin
    side_deck_dict = parse_deck_string side_deck_string
    {:ok,{:save_data,[deck_id,main_deck_dict,extra_deck_dict,side_deck_dict]}}
  end


  def write(:load_data,[player_state]) do
    cards_binary = ProtoUtil.pack_dict player_state.cards,fn({id,count})->
      <<id::32,count::8>>
    end
    decks_binary = ProtoUtil.pack_dict player_state.decks,fn({id,deck})->
      main_deck_string = Enum.map_join deck.main_deck,";",fn({id,count})->
        "#{id},#{count}"
      end
      extra_deck_string = Enum.map_join deck.extra_deck,";",fn({id,count})->
        "#{id},#{count}"
      end
      side_deck_string = Enum.map_join deck.side_deck,";",fn({id,count})->
        "#{id},#{count}"
      end
      <<id::16,
      ProtoUtil.pack_string(deck.name)::binary,
      ProtoUtil.pack_string(main_deck_string)::binary,
      ProtoUtil.pack_string(extra_deck_string)::binary,
      ProtoUtil.pack_string(side_deck_string)::binary>>
    end
    ProtoUtil.pack(13000,<<player_state.game_deck_id::16,cards_binary::binary,decks_binary::binary>>)
  end

  def write(:save_data,[result]) do
    ProtoUtil.pack(13001,<<result::16>>)
  end

end