Code.require_file "../test_helper.exs", __FILE__
defmodule FlashPolicyFacts do
  use Amrita.Sweet

  fact "flash policty test" do
    case :gen_tcp.connect("localhost", 1234, [:binary, {:packet, 0}]) do
        {:ok, socket1} ->
            :gen_tcp.send(socket1, <<"<policy-file-request/>\0">>)
            :gen_tcp.close(socket1)
        {:error, _reason1} ->
            IO.puts("connect failed!~n")
    end
  end
  
end