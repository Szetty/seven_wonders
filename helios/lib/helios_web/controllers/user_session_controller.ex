defmodule HeliosWeb.UserSessionController do
  @moduledoc """
  Writes and clears the cookie session. `HeliosWeb.LoginLive` validates first
  and then submits its form here via `phx-trigger-action`. The form also works
  as a plain HTML POST, for example when Enter is pressed before LiveView has
  connected.
  """
  use HeliosWeb, :controller

  alias Helios.Accounts
  alias HeliosWeb.UserAuth

  def create(conn, params) do
    login = login_params(params)

    case Accounts.login(Map.get(login, "access_token", ""), Map.get(login, "name", "")) do
      {:ok, user, token} ->
        UserAuth.log_in_user(conn, user, token)

      {:error, _reason, message} ->
        conn
        |> put_flash(:error, message)
        |> redirect(to: ~p"/login")
    end
  end

  def delete(conn, _params), do: UserAuth.log_out_user(conn)

  defp login_params(%{"login" => %{} = login}), do: login
  defp login_params(_params), do: %{}
end
