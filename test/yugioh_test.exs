Code.require_file "../test_helper.exs", __FILE__
defmodule YugiohFacts do
  use Amrita.Sweet

  fact "xqy client" do
    ## login
    {:ok,socket} = :gen_tcp.connect('localhost',1234,[:binary,{:packet,0},{:active,false},{:reuseaddr,true}])
    :gen_tcp.send(socket,<<17::size(16),10000::size(16),3::size(16),"xqy",6::size(16),"123456">>)
    {:ok,data}=:gen_tcp.recv(socket,0)
    data |> equals <<6::size(16),10000::size(16),1::size(16)>>    

    ## get roles
    :gen_tcp.send(socket,<<4::size(16),10004::size(16)>>)
    {:ok,_data} = :gen_tcp.recv(socket,0)

    ## enter game
    :gen_tcp.send(socket,<<8::size(16),10005::size(16),6::size(32)>>)
    {:ok,_data} = :gen_tcp.recv(socket,0)
    # IO.inspect data  

    
  end

  fact "integration test" do

    ## login
    {:ok,socket} = :gen_tcp.connect('localhost',1234,[:binary,{:packet,0},{:active,false},{:reuseaddr,true}])
    :gen_tcp.send(socket,<<18::size(16),10000::size(16),4::size(16),"sail",6::size(16),"123456">>)
    {:ok,data}=:gen_tcp.recv(socket,0)
    data |> equals <<6::size(16),10000::size(16),1::size(16)>>

    ## check role name 
    :gen_tcp.send(socket,<<10::size(16),10001::size(16),4::size(16),"sail">>)
    {:ok,data} = :gen_tcp.recv(socket,0)
    data |> equals <<5::size(16),10001::size(16),1::size(8)>>

    ## create roles                                                         avatar_id   card_type  
    :gen_tcp.send(socket,<<13::size(16),10002::size(16),5::size(16),"abcde",1::size(8),1::size(8)>>)
    {:ok,data} = :gen_tcp.recv(socket,0)
    # IO.inspect data
    data |> equals <<6::size(16),10002::size(16),1::size(16)>>

    ## delete role
    ###should get a result code specific that the delete operation is successed or failed.
    :gen_tcp.send(socket,<<11::size(16),10003::size(16),5::size(16),"abcde">>)
    {:ok,data} = :gen_tcp.recv(socket,0)
    data |> equals <<6::size(16),10003::size(16),1::size(16)>>

    ## get roles
    ####should get all role's information belonged to this account
    :gen_tcp.send(socket,<<4::size(16),10004::size(16)>>)
    {:ok,_data} = :gen_tcp.recv(socket,0)
    # data |> equals <<32::size(16),10004::size(16),2::size(16),6::size(32),4::size(16),"sail",1::size(8),9::size(32),8::size(16),"sailtsao",2::size(8)>>

    ## enter game
    :gen_tcp.send(socket,<<8::size(16),10005::size(16),6::size(32)>>)
    {:ok,_data} = :gen_tcp.recv(socket,0)
    # IO.inspect data  
    # data |> equals <<6::size(16),10005::size(16),1::size(16)>>

    ## create room
    :gen_tcp.send(socket,<<14::size(16),11000::size(16),6::size(16),"123456",1::size(16)>>)
    {:ok,data} = :gen_tcp.recv(socket,0)
    # IO.inspect data      
    <<6::size(16),11000::size(16),1::size(16)>> = data
    data |> equals <<6::size(16),11000::size(16),1::size(16)>>

    :gen_tcp.send(socket,<<14::size(16),11000::size(16),6::size(16),"654321",1::size(16)>>)
    {:ok,data} = :gen_tcp.recv(socket,0)
    <<6::size(16),11000::size(16),0::size(16)>> = data
    data |> equals <<6::size(16),11000::size(16),0::size(16)>> 

    :gen_tcp.send socket,<<4::size(16),11001::size(16)>>
    {:ok,data} = :gen_tcp.recv(socket,0)
    # IO.inspect data

    # :gen_tcp.send(socket,<<8::size(16),11002::size(16),0::size(32)>>)
    # {:ok,data} = :gen_tcp.recv(socket,0)
    # data |> equals <<6::size(16),11002::size(16),1::size(16)>>
  end

  fact "account login failed with wrong password" do
    {:ok,socket} = :gen_tcp.connect('localhost',1234,[:binary,{:packet,0},{:active,false},{:reuseaddr,true}])
    :gen_tcp.send(socket,<<19::size(16),10000::size(16),5::size(16),"sail1",6::size(16),"123456">>)

    # worng password client should be closed
    :gen_tcp.recv(socket,0) |> {:error,:closed}
  end

end
