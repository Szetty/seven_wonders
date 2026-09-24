defmodule HeliosWeb.PageControllerTest do
  use HeliosWeb.ConnCase

  test "GET / sends guests to /login", %{conn: conn} do
    assert redirected_to(get(conn, ~p"/")) == ~p"/login"
  end

  test "GET / with a stale session token sends to /login and forgets the token", %{conn: conn} do
    conn = conn |> init_test_session(%{user_token: "stale"}) |> get(~p"/")

    assert redirected_to(conn) == ~p"/login"
    refute get_session(conn, :user_token)
  end

  describe "logged in" do
    setup :register_and_log_in_user

    test "GET / sends users to /lobby", %{conn: conn} do
      assert redirected_to(get(conn, ~p"/")) == ~p"/lobby"
    end
  end
end
