defmodule BackendWeb.LoginController do
  alias BackendWeb.Common.ErrorTypes
  alias BackendWeb.Plugs.InvalidRequestHandler
  alias BackendWeb.Schemas
  alias Backend.Crux.Authorizer
  alias OpenApiSpex.Plug.CastAndValidate
  alias __MODULE__.{Request, Response}

  use BackendWeb, :controller
  use OpenApiSpex.ControllerSpecs

  require Logger

  action_fallback BackendWeb.FallbackController

  plug CastAndValidate, render_error: InvalidRequestHandler

  tags ["Login"]

  operation :login,
    summary: "Authenticate and create the user",
    request_body: {"LoginRequest", "application/json", Request},
    responses: %{
      200 => {"LoginResponse", "application/json", Response},
      400 => Schemas.ErrorResponse.response()
    }

  def login(%{body_params: %{access_token: access_token, name: name}} = conn, _) do
    with :ok <- check_access_token(access_token),
         :ok <- check_name(name),
         {:ok, response} <- create_user(name) do
      json(conn, response)
    end
  end

  defp check_access_token(access_token) do
    if access_token == Application.get_env(:backend, :access_token) do
      :ok
    else
      {:error, 401, ErrorTypes.invalid_access_token(), "Wrong access token #{access_token}"}
    end
  end

  defp check_name(name) do
    if Authorizer.user_name_valid?(name) do
      :ok
    else
      {:error, 400, ErrorTypes.invalid_name(), "Name is empty or already taken: #{name}"}
    end
  end

  defp create_user(name) do
    case Authorizer.create_user(name) do
      {:ok, jwt, user} ->
        {:ok, Response.new(jwt, user)}

      {:error, reason} ->
        Logger.error(reason)
        {:error, 500, ErrorTypes.server_error(), "Could not create user"}
    end
  end

  defmodule Request do
    alias OpenApiSpex.Schema
    require OpenApiSpex

    OpenApiSpex.schema(%{
      title: "LoginRequest",
      type: :object,
      properties: %{
        access_token: %Schema{type: :string, description: "Seven Wonder Access Token"},
        name: %Schema{type: :string, description: "User name"}
      },
      required: [:access_token, :name],
      example: %{
        "access_token" => "test",
        "name" => "test_name"
      }
    })
  end

  defmodule Response do
    alias OpenApiSpex.Schema
    alias BackendWeb.Schemas
    require OpenApiSpex

    OpenApiSpex.schema(%{
      title: "LoginResponse",
      type: :object,
      properties: %{
        name: %Schema{type: :string, description: "User name"},
        user_token: Schemas.JWT,
        game_id: Schemas.GameID
      },
      required: [:name, :user_token, :game_id],
      example: %{
        "name" => "test_name",
        "user_token" =>
          "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJnaWQiOiI3ZDY4NDJiNC1iMjhlLTQ0ZTItOTNjZC01MTQzZDcxZWNkYjQiLCJqdGkiOiIycTFwb2Vwb20yc3ExbDQycGswMDAwOTYiLCJzdWIiOiJ1c2VyIn0.Fh1zieh7CanvHKDFk5yBd2qQgZkqXKV4YJ4rKe7mRiI",
        "game_id" => "6ca37462-7e4e-43bd-9d29-850cdb3d6a67"
      }
    })

    def new(jwt, %Authorizer.User{name: name, game_id: game_id}) do
      %Response{
        name: name,
        user_token: jwt,
        game_id: game_id
      }
    end
  end
end
