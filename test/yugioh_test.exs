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

  fact "pt.read_string test" do
    bin = <<10::size(16),"sailtsao12",8::size(16),"sailtsao">>
    {str,binRest}=Yugioh.Proto.read_string(bin) 
    str |> "sailtsao12"
    {str,binRest}=Yugioh.Proto.read_string(binRest)
    str |> "sailtsao"
    Yugioh.Proto.read_string(binRest) |> {[],<<>>}
  end
  fact "pt.pack test" do
    a = <<1,2,3>>
    b = Yugioh.Proto.pack(10001,a)
    b |> equals <<7::size(16),10001::size(16),a::binary>>
  end
  
  fact "pt10.login test" do
    bin = <<10::size(16),"sailtsao12",8::size(16),"sailtsao">>
    Yugioh.Proto.PT10.read(10000,bin) |> {:ok,:login,["sailtsao12","sailtsao"]}
    Yugioh.Proto.PT10.write(10000,1) |> {:ok,<<6::size(16),10000::size(16),1::size(16)>>}
  end

  fact "decode message test" do
    bin = <<10::size(16),"sailtsao12",8::size(16),"sailtsao">>
    Yugioh.Acceptor.Worker.decode_message(10000,bin) |> {:ok,:login,["sailtsao12","sailtsao"]}
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
