defmodule Yugioh.Mixfile do
  use Mix.Project

  def project do
    [ app: :yugioh,
      version: "0.0.1",
      elixir: "~> 0.12.0",
      deps: deps,
      elixirc_options: options(Mix.env),
      buildperenvironment: true
    ]
  end

  # Configuration for the OTP application
  def application do
    [
      mod: { Yugioh, [] },
      applications: [
        :exlager
      ]
    ]
  end

  # Returns the list of dependencies in the format:
  # { :foobar, "~> 0.1", git: "https://github.com/elixir-lang/foobar.git" }
  defp deps do
    [
      # { :amrita, github: "josephwilk/amrita" },
      # { :postgrex, github: "ericmj/postgrex" },
      { :ecto, github: "elixir-lang/ecto" },
      { :exlager,github: "khia/exlager" }
    ]
  end

  defp options(env) when env in [:dev, :test] do
    [exlager_level: :debug, exlager_truncation_size: 8096]
  end

  defp options(env) when env in [:prod] do
    []
  end
end
