defmodule Yugioh do
  use Application.Behaviour

  # See http://elixir-lang.org/docs/stable/Application.Behaviour.html
  # for more information on OTP Applications
  def start(_type, _args) do
    :supervisor.start_link(Yugioh.Supervisor,[])
  end
  def stop(_state) do
    IO.puts "stop"
  end
  
end
