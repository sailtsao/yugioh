defmodule RoomTest do
  use ExUnit.Case

  test "room test" do
    :timer.sleep 1000
    started_process_count = length(Process.list) 

    ## login
    {:ok,socket1} = :gen_tcp.connect('localhost',1234,[:binary,{:packet,0},{:active,false},{:reuseaddr,true}])

    # web login test
    :gen_tcp.send(socket1,<<16::size(16),10007::size(16),1::size(32),6::size(16),"123456">>)

    # normal login test
    # :gen_tcp.send(socket1,<<17::size(16),10000::size(16),3::size(16),"xqy",6::size(16),"123456">>)

    {:ok,data}=:gen_tcp.recv(socket1,0)
    assert data == <<6::size(16),10000::size(16),1::size(16)>>

    {:ok,socket2} = :gen_tcp.connect('localhost',1234,[:binary,{:packet,0},{:active,false},{:reuseaddr,true}])
    :gen_tcp.send(socket2,<<17::size(16),10000::size(16),3::size(16),"xqy",6::size(16),"123456">>)
    {:ok,data}=:gen_tcp.recv(socket2,0)
    assert data == <<6::size(16),10000::size(16),1::size(16)>>

    ## get roles
    :gen_tcp.send(socket1,<<4::size(16),10004::size(16)>>)
    {:ok,_data} = :gen_tcp.recv(socket1,0)

    ## get roles
    :gen_tcp.send(socket2,<<4::size(16),10004::size(16)>>)
    {:ok,_data} = :gen_tcp.recv(socket2,0)

    ## enter game
    :gen_tcp.send(socket1,<<8::size(16),10005::size(16),6::size(32)>>)
    {:ok,_data} = :gen_tcp.recv(socket1,0)

    ## enter game
    :gen_tcp.send(socket2,<<8::size(16),10005::size(16),8::size(32)>>)
    {:ok,_data} = :gen_tcp.recv(socket2,0)

    ############
    # login again test
    
    # {:ok,socket3} = :gen_tcp.connect('localhost',1234,[:binary,{:packet,0},{:active,false},{:reuseaddr,true}])
    # :gen_tcp.send(socket3,<<18::size(16),10000::size(16),4::size(16),"sail",6::size(16),"123456">>)
    # {:ok,data}=:gen_tcp.recv(socket3,0)
    # assert data == <<6::size(16),10000::size(16),1::size(16)>>

    # :gen_tcp.send(socket3,<<8::size(16),10005::size(16),8::size(32)>>)
    # {:ok,_data} = :gen_tcp.recv(socket3,0)

    # {:ok,data} = :gen_tcp.recv(socket2,0)
    # <<_::size(16),10006::size(16),_::size(16),str::binary>> = data
    # IO.puts str

    ###################
    # create room
    :gen_tcp.send(socket1,<<12::size(16),11000::size(16),4::size(16),"room",1::size(16)>>)
    {:ok,data} = :gen_tcp.recv(socket1,0)    
    <<
    _::size(16),11000::size(16),1::size(16),room_id::size(32),_status::size(16),4::size(16),"room",1::size(16),
    1::size(16),
    1::size(8),6::size(32),4::size(16),"sail",2::size(8),1::size(8),1::size(8)
    >> = data

    # enter room
    :gen_tcp.send socket2,<<8::size(16),11002::size(16),room_id::size(32)>>
    {:ok,data} = :gen_tcp.recv(socket2,0)    
    assert <<
    _::size(16),11002::size(16),1::size(16),room_id::size(32),_status::size(16),4::size(16),"room",1::size(16),
    2::size(16),
    2::size(8),8::size(32),3::size(16),"xqy",1::size(8),0::size(8),0::size(8),
    1::size(8),6::size(32),4::size(16),"sail",2::size(8),1::size(8),1::size(8)    
    >> = data

    # new member notify
    {:ok,data}=:gen_tcp.recv(socket1,0)
    assert data == <<15::size(16),11003::size(16),2::size(8),8::size(32),3::size(16),"xqy",1::size(8)>>

    # battle ready    
    :gen_tcp.send socket2,<<4::size(16),11006::size(16)>>

    # battle ready notify
    {:ok,data}=:gen_tcp.recv(socket2,0)
    assert data == <<6::size(16),11006::size(16),2::size(8),1::size(8)>>

    {:ok,data}=:gen_tcp.recv(socket1,0)
    assert data == <<6::size(16),11006::size(16),2::size(8),1::size(8)>>

    # # battle ready    
    # :gen_tcp.send socket2,<<4::size(16),11006::size(16)>>

    # # battle ready notify
    # {:ok,data}=:gen_tcp.recv(socket2,0)
    # assert data == <<6::size(16),11006::size(16),2::size(8),0::size(8)>>

    # {:ok,data}=:gen_tcp.recv(socket1,0)
    # assert data == <<6::size(16),11006::size(16),2::size(8),0::size(8)>>

    # battle start
    :gen_tcp.send socket1,<<4::size(16),11007::size(16)>>

    # battle start notify

    {:ok,data}=:gen_tcp.recv(socket1,0)
    # IO.inspect data,limit: 1000    
    assert <<84::size(16),11007::size(16),1::size(16),6::size(32),1::size(8),6::size(32),4::size(16),"sail",2::size(8),3000::size(16),3000::size(16),5::size(16),_card_1::size(160),8::size(32),3::size(16),"xqy",1::size(8),3000::size(16),3000::size(16),5::size(16),_card_2::size(160)>> = data
    
    {:ok,data}=:gen_tcp.recv(socket2,0)
    assert <<84::size(16),11007::size(16),1::size(16),6::size(32),1::size(8),6::size(32),4::size(16),"sail",2::size(8),3000::size(16),3000::size(16),5::size(16),_card_1::size(160),8::size(32),3::size(16),"xqy",1::size(8),3000::size(16),3000::size(16),5::size(16),_card_2::size(160)>> = data        

    :gen_tcp.send socket1,<<4::size(16),12006::size(16)>>
    :gen_tcp.send socket2,<<4::size(16),12006::size(16)>>

    # player1 receive new turn draw
    {:ok,data}=:gen_tcp.recv(socket1,14)    
    assert <<14::size(16),12002::size(16),1::size(8),1::size(8),6::size(32),_draw_card_id::size(32)>> = data

    {:ok,data}=:gen_tcp.recv(socket1,5)
    assert data == <<5::size(16),12000::size(16),2::size(8)>>

    {:ok,data}=:gen_tcp.recv(socket1,0)
    assert data == <<5::size(16),12000::size(16),3::size(8)>>
    

    # player2 receive new turn draw
    {:ok,data}=:gen_tcp.recv(socket2,14)
    assert <<14::size(16),12002::size(16),1::size(8),1::size(8),6::size(32),_draw_card_id::size(32)>> = data

    {:ok,data}=:gen_tcp.recv(socket2,5)
    assert data == <<5::size(16),12000::size(16),2::size(8)>>

    {:ok,data}=:gen_tcp.recv(socket2,0)
    assert data == <<5::size(16),12000::size(16),3::size(8)>>    

#   refresh_roominfo test
    # :gen_tcp.send socket2,<<4::size(16),11005::size(16)>>
    # {:ok,data}=:gen_tcp.recv(socket2,0)
    # assert <<
    # _::size(16),11005::size(16),room_id::size(32),1::size(16),4::size(16),"room",1::size(16),
    # 2::size(16),
    # 1::size(8),6::size(32),4::size(16),"sail",2::size(8),1::size(8),1::size(8),
    # 2::size(8),8::size(32),3::size(16),"xqy",1::size(8),0::size(8),0::size(8)
    # >> = data 

    # summon
    :gen_tcp.send socket1,<<6::size(16), 12001::size(16),0::size(8),2::size(8)>> 
    {:ok,data}=:gen_tcp.recv(socket1,0)
    assert <<15::size(16), 12001::size(16),6::size(32),0::size(8),_summon_card_id::size(32),0::size(8),2::size(8)>> = data
    {:ok,data}=:gen_tcp.recv(socket2,0)
    assert <<15::size(16), 12001::size(16),6::size(32),0::size(8),_summon_card_id::size(32),0::size(8),2::size(8)>> = data

    # flip card test
    :gen_tcp.send socket1,<<5::size(16), 12004::size(16),0::size(8)>>
    {:ok,data}=:gen_tcp.recv(socket1,0)
    assert <<14::size(16), 12004::size(16),6::size(32),0::size(8),_card_id::size(32),1::size(8)>> = data
    {:ok,data}=:gen_tcp.recv(socket2,0)
    assert <<14::size(16), 12004::size(16),6::size(32),0::size(8),_card_id::size(32),1::size(8)>> = data

    # flip many times test
    # :gen_tcp.send socket1,<<5::size(16), 12004::size(16),0::size(8)>>

    # change phase to bp
    :gen_tcp.send socket1,<<5::size(16),12000::size(16),4::size(8)>>
    
    {:ok,data}=:gen_tcp.recv(socket1,0)
    assert data == <<5::size(16),12000::size(16),4::size(8)>>
    {:ok,data}=:gen_tcp.recv(socket2,0)
    assert data == <<5::size(16),12000::size(16),4::size(8)>>

    # mp2
    :gen_tcp.send socket1,<<5::size(16),12000::size(16),5::size(8)>>
    {:ok,data}=:gen_tcp.recv(socket1,0)
    assert data == <<5::size(16),12000::size(16),5::size(8)>>
    {:ok,data}=:gen_tcp.recv(socket2,0)
    assert data == <<5::size(16),12000::size(16),5::size(8)>>

    # ep
    :gen_tcp.send socket1,<<5::size(16),12000::size(16),6::size(8)>>
    {:ok,data}=:gen_tcp.recv(socket1,5)
    assert data == <<5::size(16),12000::size(16),6::size(8)>>

    {:ok,data}=:gen_tcp.recv(socket2,5)
    assert data == <<5::size(16),12000::size(16),6::size(8)>>

    # player1 receive new turn draw
    {:ok,data}=:gen_tcp.recv(socket1,14)    
    assert <<14::size(16),12002::size(16),2::size(8),1::size(8),8::size(32),_draw_card_id::size(32)>> = data

    {:ok,data}=:gen_tcp.recv(socket1,5)
    assert data == <<5::size(16),12000::size(16),2::size(8)>>

    {:ok,data}=:gen_tcp.recv(socket1,0)
    assert data == <<5::size(16),12000::size(16),3::size(8)>>


    # player2 receive new turn draw
    {:ok,data}=:gen_tcp.recv(socket2,14)
    assert <<14::size(16),12002::size(16),2::size(8),1::size(8),8::size(32),_draw_card_id::size(32)>> = data

    {:ok,data}=:gen_tcp.recv(socket2,5)
    assert data == <<5::size(16),12000::size(16),2::size(8)>>

    {:ok,data}=:gen_tcp.recv(socket2,0)
    assert data == <<5::size(16),12000::size(16),3::size(8)>>    


    # summon test
    :gen_tcp.send socket2,<<6::size(16), 12001::size(16),0::size(8),1::size(8)>>
    {:ok,data}=:gen_tcp.recv(socket1,0)
    assert <<15::size(16), 12001::size(16),8::size(32),0::size(8),_summon_card_id::size(32),0::size(8),1::size(8)>> = data
    {:ok,data}=:gen_tcp.recv(socket2,0)
    assert <<15::size(16), 12001::size(16),8::size(32),0::size(8),_summon_card_id::size(32),0::size(8),1::size(8)>> = data

    # change phase to bp
    :gen_tcp.send socket2,<<5::size(16),12000::size(16),4::size(8)>>
    
    {:ok,data}=:gen_tcp.recv(socket1,0)
    assert data == <<5::size(16),12000::size(16),4::size(8)>>
    {:ok,data}=:gen_tcp.recv(socket2,0)
    assert data == <<5::size(16),12000::size(16),4::size(8)>>

    # attack
    :gen_tcp.send socket2,<<6::size(16),12003::size(16),0::size(8),0::size(8)>>
    {:ok,data}=:gen_tcp.recv(socket1,16)
    assert <<total_size::size(16),12003::size(16),0::size(8),0::size(8),target_card_id::size(32),damage_player_id::size(32),hp_damage::size(16)>> = data    
    IO.puts damage_player_id
    IO.puts hp_damage
    if total_size>16 do
      IO.inspect :gen_tcp.recv(socket1,total_size-16)
    end
    
    {:ok,data}=:gen_tcp.recv(socket2,16)
    assert <<total_size::size(16),12003::size(16),0::size(8),0::size(8),target_card_id::size(32),_damage_player_id::size(32),_hp_damage::size(16)>> = data
    if total_size>16 do
      IO.inspect :gen_tcp.recv(socket2,total_size-16)
    end    

    # battle end receive
    {:ok,data}=:gen_tcp.recv(socket1,0)
      assert data == <<13::size(16),12005::size(16),1::size(8),8::size(32),6::size(32)>>
    {:ok,data}=:gen_tcp.recv(socket2,0)
      assert data == <<13::size(16),12005::size(16),1::size(8),8::size(32),6::size(32)>>

    #leave room
    :gen_tcp.send socket1,<<4::size(16),11004::size(16)>>
    {:ok,data} = :gen_tcp.recv(socket1,0)
    assert data == <<6::size(16),11004::size(16),1::size(16)>>

    {:ok,data}=:gen_tcp.recv(socket2,0)
    assert <<
    _::size(16),11005::size(16),room_id::size(32),1::size(16),4::size(16),"room",1::size(16),
    1::size(16),
    2::size(8),8::size(32),3::size(16),"xqy",1::size(8),1::size(8),1::size(8)
    >> = data 

    # leave room
    :gen_tcp.send socket2,<<4::size(16),11004::size(16)>>
    {:ok,data} = :gen_tcp.recv(socket2,0)    
    assert data == <<6::size(16),11004::size(16),1::size(16)>>

    :gen_tcp.close(socket1)
    :gen_tcp.close(socket2)

    :timer.sleep 1000

    ended_process_count = length(Process.list)     

    assert ended_process_count == started_process_count
  end

end