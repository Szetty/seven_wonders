defmodule HeliosWeb.PageController do
  use HeliosWeb, :controller

  alias Helios.Accounts.Scope
  alias Helios.Lobbies

  def home(conn, _params) do
    case conn.assigns[:current_scope] do
      %Scope{user: %{} = user} ->
        lobby = Lobbies.get_or_create_own_lobby(user)
        redirect(conn, to: ~p"/lobby/#{lobby.id}")

      _ ->
        redirect(conn, to: ~p"/login")
    end
  end
end
