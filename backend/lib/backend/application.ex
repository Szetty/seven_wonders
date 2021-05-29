defmodule Backend.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  alias Backend.Crux
  alias BackendWeb.{Endpoint, Telemetry}

  use Application

  def start(_type, _args) do
    Vapor.load!([%Vapor.Provider.Dotenv{}])
    load_system_env()

    children = [
      Telemetry,
      {Phoenix.PubSub, name: Backend.PubSub},
      Endpoint,
      Crux.Supervisor
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Backend.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  def config_change(changed, _new, removed) do
    BackendWeb.Endpoint.config_change(changed, removed)
    :ok
  end

  defp load_system_env() do
    access_token =
      case System.get_env("ACCESS_TOKEN") do
        nil -> raise("Environment variable ACCESS_TOKEN must be set")
        value -> value
      end

    jwt_secret =
      case System.get_env("JWT_SECRET") do
        nil -> raise("Environment variable JWT_SECRET must be set")
        value -> value
      end

    Application.put_env(:backend, :access_token, access_token)
    Application.put_env(:backend, :jwt_secret, jwt_secret)
  end
end
