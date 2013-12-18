defmodule RoomTest do
  use ExUnit.Case

  test "room test" do
    ## login
    {:ok,socket1} = :gen_tcp.connect('localhost',1234,[:binary,{:packet,0},{:active,false},{:reuseaddr,true}])
    :gen_tcp.send(socket1,<<17::size(16),10000::size(16),3::size(16),"xqy",6::size(16),"123456">>)
    {:ok,data}=:gen_tcp.recv(socket1,0)
    assert data === <<6::size(16),10000::size(16),1::size(16)>>

    {:ok,socket2} = :gen_tcp.connect('localhost',1234,[:binary,{:packet,0},{:active,false},{:reuseaddr,true}])
    :gen_tcp.send(socket2,<<18::size(16),10000::size(16),4::size(16),"sail",6::size(16),"123456">>)
    {:ok,data}=:gen_tcp.recv(socket2,0)
    assert data === <<6::size(16),10000::size(16),1::size(16)>>

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

    # create room
    :gen_tcp.send(socket1,<<12::size(16),11000::size(16),4::size(16),"room",1::size(16)>>)
    {:ok,data} = :gen_tcp.recv(socket1,0)    
    assert <<
    35::size(16),11000::size(16),1::size(16),room_id::size(32),status::size(16),4::size(16),"room",1::size(16),
    1::size(16),
    1::size(8),6::size(32),4::size(16),"sail",1::size(8),1::size(8)
    >> = data

    # enter room
    :gen_tcp.send socket2,<<8::size(16),11002::size(16),room_id::size(32)>>
    {:ok,data} = :gen_tcp.recv(socket2,0)    
    assert data ===
    <<
    47::size(16),11002::size(16),1::size(16),1::size(32),status::size(16),4::size(16),"room",1::size(16),
    2::size(16),
    1::size(8),6::size(32),4::size(16),"sail",1::size(8),1::size(8),
    2::size(8),8::size(32),3::size(16),"xqy",1::size(8),0::size(8)
    >>

    # new member notify
    {:ok,data}=:gen_tcp.recv(socket1,0)
    assert data === <<15::size(16),11003::size(16),2::size(8),8::size(32),3::size(16),"xqy",1::size(8)>>
    
    # room owner leave room
    :gen_tcp.close(socket1)

    {:ok,data}=:gen_tcp.recv(socket2,0)
    assert data ===
    <<
    32::size(16),11005::size(16),1::size(32),1::size(16),4::size(16),"room",1::size(16),
    1::size(16),
    2::size(8),8::size(32),3::size(16),"xqy",1::size(8),1::size(8)
    >>

    # leave room
    :gen_tcp.send socket2,<<8::size(16),11004::size(16),room_id::size(32)>>
    {:ok,data} = :gen_tcp.recv(socket2,0)    
    assert data === <<6::size(16),11004::size(16),1::size(16)>>
  end

end