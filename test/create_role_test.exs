Code.require_file "../test_helper.exs", __FILE__
defmodule YugiohFacts do
  use Amrita.Sweet

  fact "integration test" do
    ## login
    {:ok,socket} = :gen_tcp.connect('localhost',1234,[:binary,{:packet,0},{:active,false},{:reuseaddr,true}])
    :gen_tcp.send(socket,<<18::size(16),10000::size(16),4::size(16),"sail",6::size(16),"123456">>)
    {:ok,data}=:gen_tcp.recv(socket,0)
    data |> equals <<6::size(16),10000::size(16),1::size(16)>>

    ## check role name 
    :gen_tcp.send(socket,<<10::size(16),10001::size(16),4::size(16),"abcd">>)
    {:ok,data} = :gen_tcp.recv(socket,0)
    data |> equals <<5::size(16),10001::size(16),0::size(8)>>

    ## create role
    :gen_tcp.send(socket,<<12::size(16),10002::size(16),4::size(16),"abcd",2::size(8),1::size(8)>>)
    {:ok,data} = :gen_tcp.recv(socket,0)
    # IO.inspect data
    data |> equals <<6::size(16),10002::size(16),1::size(16)>>
    
  end  

end
