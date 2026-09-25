defmodule HeliosWeb.UserSessionControllerTest do
  use HeliosWeb.ConnCase

  import Helios.AccountsFixtures

  alias Helios.Accounts
  alias HeliosWeb.Presence

  defp login_params(access_token, name),
    do: %{"login" => %{"access_token" => access_token, "name" => name}}

  describe "POST /session" do
    test "logs the user in and redirects to /", %{conn: conn} do
      name = unique_user_name()
      conn = post(conn, ~p"/session", login_params(valid_access_token(), name))

      assert redirected_to(conn) == ~p"/"
      token = get_session(conn, :user_token)
      assert %{name: ^name} = Accounts.get_user_by_session_token(token)
      assert get_session(conn, :live_socket_id) == Accounts.live_socket_id(token)

      conn = conn |> recycle() |> get(~p"/")
      assert redirected_to(conn) =~ ~r"^/lobby/[0-9a-f-]{36}$"

      conn = conn |> recycle() |> get(redirected_to(conn))
      assert html_response(conn, 200) =~ name
    end

    test "redirects back to /login with the error message on failure", %{conn: conn} do
      conn = post(conn, ~p"/session", login_params("nope", unique_user_name()))

      assert redirected_to(conn) == ~p"/login"
      assert Phoenix.Flash.get(conn.assigns.flash, :error) == "Wrong access token"
      refute get_session(conn, :user_token)
    end

    test "rejects a name held by an online user", %{conn: conn} do
      user = user_fixture()
      {:ok, _ref} = Presence.track_user(self(), user)

      conn = post(conn, ~p"/session", login_params(valid_access_token(), user.name))

      assert redirected_to(conn) == ~p"/login"
      assert Phoenix.Flash.get(conn.assigns.flash, :error) == "Name is already taken"
      :ok = Presence.untrack(self(), Presence.online_topic(), to_string(user.id))
    end

    test "handles missing or malformed params without crashing", %{conn: conn} do
      cases = [
        {%{}, "Access token can't be empty!"},
        {%{"login" => "oops"}, "Access token can't be empty!"},
        {%{"login" => %{"name" => "x"}}, "Access token can't be empty!"},
        {%{"login" => %{"access_token" => valid_access_token(), "name" => ["x"]}},
         "Name can't be empty!"}
      ]

      for {params, message} <- cases do
        conn = post(conn, ~p"/session", params)
        assert redirected_to(conn) == ~p"/login"
        assert Phoenix.Flash.get(conn.assigns.flash, :error) == message
      end
    end
  end

  describe "DELETE /session" do
    setup :register_and_log_in_user

    test "logs out, forgets the token and redirects to /login", %{conn: conn, token: token} do
      conn = delete(conn, ~p"/session")

      assert redirected_to(conn) == ~p"/login"
      refute get_session(conn, :user_token)
      assert Accounts.get_user_by_session_token(token) == nil

      conn = conn |> recycle() |> get(~p"/lobby/#{Ecto.UUID.generate()}")
      assert redirected_to(conn) == ~p"/login"
    end
  end

  test "DELETE /session as a guest just redirects to /login", %{conn: conn} do
    assert redirected_to(delete(conn, ~p"/session")) == ~p"/login"
  end
end
