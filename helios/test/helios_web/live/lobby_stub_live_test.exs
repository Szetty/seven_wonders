defmodule HeliosWeb.LobbyStubLiveTest do
  use HeliosWeb.ConnCase

  import Phoenix.LiveViewTest

  alias HeliosWeb.Presence

  describe "logged in" do
    setup :register_and_log_in_user

    test "greets the user and marks them online", %{conn: conn, user: user} do
      {:ok, view, _html} = live(conn, ~p"/lobby")

      assert has_element?(view, "#lobby-greeting", "Welcome, #{user.name}")
      assert Presence.user_online?(user.id)

      # Untrack synchronously so a later test that reuses this (rolled-back)
      # user id never sees a leftover presence entry.
      :ok = Presence.untrack(view.pid, Presence.online_topic(), to_string(user.id))
    end
  end

  test "guests are redirected to /login", %{conn: conn} do
    assert {:error, {:redirect, %{to: "/login", flash: flash}}} = live(conn, ~p"/lobby")
    assert flash["error"] == "You must log in to access this page."
  end

  test "a stale token on a fresh page load is treated as a guest", %{conn: conn} do
    conn = init_test_session(conn, %{user_token: "stale"})

    assert {:error, {:redirect, %{to: "/login", flash: flash}}} = live(conn, ~p"/lobby")
    assert flash["error"] == "You must log in to access this page."
  end
end
