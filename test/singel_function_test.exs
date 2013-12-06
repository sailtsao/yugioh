Code.require_file "../test_helper.exs", __FILE__
defmodule YugiohFunctionFacts do
  use Amrita.Sweet

  fact "pt.read_string test" do
    bin = <<10::size(16),"sailtsao12",8::size(16),"sailtsao">>
    {str,binRest}=Yugioh.Proto.read_string(bin) |>{"sailtsao12",<<8::size(16),"sailtsao">>}
    {str,binRest}=Yugioh.Proto.read_string(binRest) |> {"sailtsao",<<>>}
    Yugioh.Proto.read_string(binRest) |> {[],<<>>}
  end
  
  fact "pt.pack test" do
    a = <<1,2,3>>
    Yugioh.Proto.pack(10001,a) |> equals <<7::size(16),10001::size(16),<<1,2,3>>::binary>>
  end

  fact "Yugioh.Acceptor.Worker.decode_message test" do
    bin = <<10::size(16),"sailtsao12",8::size(16),"sailtsao">>
    Yugioh.Acceptor.Worker.decode_message(10000,bin) |> {:ok,:login,["sailtsao12","sailtsao"]}
  end

end
