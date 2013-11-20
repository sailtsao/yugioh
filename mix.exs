defmodule Yugioh.Mixfile do
  use Mix.Project

  def project do
    [ app: :yugioh,
      version: "0.0.1",
      elixir: "~> 0.11.2",
      deps: deps ]
  end

  # Configuration for the OTP application
  def application do
    [mod: { Yugioh, [] }]
  end

  # Returns the list of dependencies in the format:
  # { :foobar, "~> 0.1", git: "https://github.com/elixir-lang/foobar.git" }
  defp deps do
    [
      {:amrita, "~>0.2", github: "josephwilk/amrita"},
      { :postgrex, github: "ericmj/postgrex" },
      { :ecto, github: "elixir-lang/ecto" }
    ]
  end
end
