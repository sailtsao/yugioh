defmodule YugiohApp do
  use Application.Behaviour

  # See http://elixir-lang.org/docs/stable/Application.Behaviour.html
  # for more information on OTP Applications
  def start(_type, [acceptor_count: acceptor_count,port: port]) do
    YugiohAppSup.start_link [acceptor_count,port]
  end
    
end
