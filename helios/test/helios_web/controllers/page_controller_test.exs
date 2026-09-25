defmodule HeliosWeb.PageControllerTest do
  use HeliosWeb.ConnCase, async: false

  import Helios.LobbiesFixtures

  alias Helios.Lobbies

  test "GET / redirects guests to /login", %{conn: conn} do
    conn = get(conn, ~p"/")

    assert redirected_to(conn) == ~p"/login"
  end

  test "GET / redirects a logged-in user to their own table", %{conn: conn} do
    {conn, user} = log_in_player(conn)

    conn = get(conn, ~p"/")

    lobby = Lobbies.get_or_create_own_lobby(user)
    assert redirected_to(conn) == ~p"/lobby/#{lobby.id}"
  end
end
