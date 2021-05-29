defmodule BackendWeb.Common.ErrorTypes do
  @error_types %{
    server_error: "SERVER_ERROR",
    invalid_body: "INVALID_BODY",
    cannot_parse_payload: "CANNOT_PARSE_PAYLOAD",
    invalid_endpoint: "INVALID_ENDPOINT",
    invalid_access_token: "INVALID_ACCESS_TOKEN",
    invalid_name: "INVALID_NAME",
    invalid_game_id: "INVALID_GAME_ID",
    unauthorized: "UNAUTHORIZED",
    invalid_user: "INVALID_USER"
  }

  @error_types
  |> Enum.each(fn {name, value} ->
    def unquote(name)(), do: unquote(value)
  end)
end
