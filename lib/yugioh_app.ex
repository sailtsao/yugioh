defmodule YugiohApp do
  use Application.Behaviour
  require Lager
  # See http://elixir-lang.org/docs/stable/Application.Behaviour.html
  # for more information on OTP Applications
  def start(_type, [acceptor_count: acceptor_count,port: port]) do    
    
    # set lager parameters
    if  Mix.env == :dev or Mix.env == :test do
      Lager.set_loglevel(:lager_file_backend,'log/console.log',:debug)
      Lager.set_loglevel(:lager_console_backend,:debug)
    end

    YugiohAppSup.start_link [acceptor_count,port]
  end
    
end
