Code.require_file "../test_helper.exs", __FILE__

    
    # case :gen_tcp.connect("localhost", 1234, [:binary, {:packet, 0}]) do
    #     {:ok, socket1} ->
    #         :gen_tcp.send(socket1, <<"<policy-file-request/>\0">>)
    #         :gen_tcp.close(socket1)
    #     {:error, _reason1} ->
    #         IO.puts("connect failed!~n")
    # end

defmodule YugiohFacts do
  use Amrita.Sweet

  # fact "pt.read_string test" do
  #   bin = <<10::size(16),"sailtsao12",8::size(16),"sailtsao">>
  #   {str,binRest}=Yugioh.Proto.read_string(bin) 
  #   str |> "sailtsao12"
  #   {str,binRest}=Yugioh.Proto.read_string(binRest)
  #   str |> "sailtsao"
  #   Yugioh.Proto.read_string(binRest) |> {[],<<>>}
  # end
  # fact "pt.pack test" do
  #   a = <<1,2,3>>
  #   b = Yugioh.Proto.pack(10001,a)
  #   b |> equals <<7::size(16),10001::size(16),a::binary>>
  # end
  
  # fact "pt10.login test" do
  #   bin = <<4::size(16),"sail",6::size(16),"123456">>
  #   Yugioh.Proto.PT10.read(10000,bin) |> {:ok,:login,["sail","123456"]}
  #   Yugioh.Proto.PT10.write(10000,1) |> equals <<6::size(16),10000::size(16),1::size(16)>>
  # end

  # fact "decode message test" do
  #   bin = <<10::size(16),"sailtsao12",8::size(16),"sailtsao">>
  #   Yugioh.Acceptor.Worker.decode_message(10000,bin) |> {:ok,:login,["sailtsao12","sailtsao"]}
  # end

  # fact "repo test" do    
  #   import Ecto.Query
  #   query = from(u in Model.User,where: u.name == "sail",select: u)
  #   r =case Yugioh.Repo.all(query) do
  #     [user] ->
  #       case user.password=="123456" do
  #         true->
  #           {:ok,user.id}
  #         false->
  #           {:fail,:wrong_password}
  #       end
  #     []->
  #       {:fail,:no_user_find}
  #   end
  #   r |> equals {:ok,1}
    # roles = Yugioh.Repo.all(user.roles)
    # length(roles) |> 2
    # IO.puts hd(roles).name
    # IO.puts hd(roles).gender
    # Yugioh.Module.Account.login(["sail","123456"],nil) |> {:ok,1}
    # name = "sail"    
    # Yugioh.Module.Account.role_exist?("sail",nil) |> true
    # length(Yugioh.Module.Account.get_roles(1,nil)) |> 2
  # end

  fact "account module test" do
    # login
    {:ok,socket} = :gen_tcp.connect('sailtsao.com',1234,[:binary,{:packet,0},{:active,false},{:reuseaddr,true}])
    :gen_tcp.send(socket,<<18::size(16),10000::size(16),4::size(16),"sail",6::size(16),"123456">>)
    {:ok,data}=:gen_tcp.recv(socket,0)
    data |> equals <<6::size(16),10000::size(16),1::size(16)>>
    # check role name 
    :gen_tcp.send(socket,<<10::size(16),10001::size(16),4::size(16),"sail">>)
    {:ok,data} = :gen_tcp.recv(socket,0)
    data |> equals <<5::size(16),10001::size(16),1::size(8)>>
    # create role
    :gen_tcp.send(socket,<<12::size(16),10002::size(16),5::size(16),"abcde",1::size(8)>>)
    {:ok,data} = :gen_tcp.recv(socket,0)
    data |> equals <<5::size(16),10002::size(16),1::size(16)>>
    # delete role
    :gen_tcp.send(socket,<<11::size(16),10003::size(16),5::size(16),"abcde">>)
    {:ok,data} = :gen_tcp.recv(socket,0)
    data |> equals <<5::size(16),10003::size(16),1::size(16)>>
    # get roles
    :gen_tcp.send(socket,<<4::size(16),10004::size(16)>>)
    {:ok,data} = :gen_tcp.recv(socket,0)
    IO.inspect data
    # data |> equals <<32::size(16),10004::size(16),2::size(16),6::size(32),4::size(16),"sail",1::size(8),9::size(32),8::size(16),"sailtsao",2::size(8)>>
  end

  fact "account login failed with wrong password" do
    {:ok,socket} = :gen_tcp.connect('sailtsao.com',1234,[:binary,{:packet,0},{:active,false},{:reuseaddr,true}])
    :gen_tcp.send(socket,<<19::size(16),10000::size(16),5::size(16),"sail1",6::size(16),"123456">>)
    :gen_tcp.recv(socket,0) |> {:error,:closed}
  end
  # fact "parse packet test" do
  #   provided [
  #     :gen_tcp.recv(_,4) |> {:ok,<<22::size(16),10000::size(16)>>},
  #     :gen_tcp.recv(_,18) |> <<10::size(16),"sailtsao12",8::size(16),"sailtsao">>
  #   ] do
  #     defrecordp :rClient,[accid: 0,login: false]
  #     Yugioh.Acceptor.Worker.parse_packet("socket",rClient())
  #   end
  # end
end
