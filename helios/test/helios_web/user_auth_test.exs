defmodule HeliosWeb.UserAuthTest do
  use HeliosWeb.ConnCase

  import Helios.AccountsFixtures

  alias Helios.Accounts
  alias Helios.Accounts.Scope
  alias HeliosWeb.UserAuth
  alias Phoenix.LiveView

  setup %{conn: conn} do
    conn =
      conn
      |> Map.replace!(:secret_key_base, HeliosWeb.Endpoint.config(:secret_key_base))
      |> init_test_session(%{})

    {:ok, user, token} = Accounts.login(valid_access_token(), unique_user_name())
    %{conn: conn, user: user, token: token}
  end

  defp socket do
    %LiveView.Socket{endpoint: HeliosWeb.Endpoint, assigns: %{__changed__: %{}, flash: %{}}}
  end

  describe "fetch_current_scope/2" do
    test "assigns the scope for a valid token", %{conn: conn, user: user, token: token} do
      conn = conn |> put_session(:user_token, token) |> UserAuth.fetch_current_scope([])
      assert %Scope{user: %{id: id}} = conn.assigns.current_scope
      assert id == user.id
    end

    test "assigns nil without a token", %{conn: conn} do
      conn = UserAuth.fetch_current_scope(conn, [])
      assert conn.assigns.current_scope == nil
    end

    test "assigns nil and drops an unknown token from the session", %{conn: conn} do
      conn =
        conn
        |> put_session(:user_token, "stale")
        |> put_session(:live_socket_id, "users_sessions:stale")
        |> put_session(:other, "kept")
        |> UserAuth.fetch_current_scope([])

      assert conn.assigns.current_scope == nil
      refute get_session(conn, :user_token)
      refute get_session(conn, :live_socket_id)
      assert get_session(conn, :other) == "kept"
    end
  end

  describe "log_in_user/3" do
    test "stores the token and live_socket_id and redirects to /", %{
      conn: conn,
      user: user,
      token: token
    } do
      conn = UserAuth.log_in_user(conn, user, token)

      assert get_session(conn, :user_token) == token
      assert get_session(conn, :live_socket_id) == Accounts.live_socket_id(token)
      assert redirected_to(conn) == ~p"/"
    end

    test "clears everything previously stored in the session", %{
      conn: conn,
      user: user,
      token: token
    } do
      conn = conn |> put_session(:to_be_removed, "value") |> UserAuth.log_in_user(user, token)
      refute get_session(conn, :to_be_removed)
    end
  end

  describe "log_out_user/1" do
    test "deletes the token, disconnects live sessions and redirects to /login", %{
      conn: conn,
      token: token
    } do
      live_socket_id = Accounts.live_socket_id(token)
      HeliosWeb.Endpoint.subscribe(live_socket_id)

      conn =
        conn
        |> put_session(:user_token, token)
        |> put_session(:live_socket_id, live_socket_id)
        |> UserAuth.log_out_user()

      assert_receive %Phoenix.Socket.Broadcast{event: "disconnect", topic: ^live_socket_id}
      refute get_session(conn, :user_token)
      refute get_session(conn, :live_socket_id)
      assert redirected_to(conn) == ~p"/login"
      assert Accounts.get_user_by_session_token(token) == nil
    end

    test "works for guests", %{conn: conn} do
      conn = UserAuth.log_out_user(conn)
      assert redirected_to(conn) == ~p"/login"
    end
  end

  describe "on_mount :require_user" do
    test "assigns the scope for a valid token", %{user: user, token: token} do
      assert {:cont, socket} =
               UserAuth.on_mount(:require_user, %{}, %{"user_token" => token}, socket())

      assert socket.assigns.current_scope.user.id == user.id
    end

    test "redirects guests to /login (:unauthorized)" do
      assert {:halt, socket} = UserAuth.on_mount(:require_user, %{}, %{}, socket())
      assert {:redirect, %{to: "/login"}} = socket.redirected
      assert socket.assigns.flash["error"] == "You must log in to access this page."
    end

    test "redirects unknown or revoked tokens to /login (:invalid_user)" do
      assert {:halt, socket} =
               UserAuth.on_mount(:require_user, %{}, %{"user_token" => "revoked"}, socket())

      assert {:redirect, %{to: "/login"}} = socket.redirected
      assert socket.assigns.flash["error"] == "Your session has expired, please log in again."
    end
  end

  describe "on_mount :redirect_if_user" do
    test "redirects logged-in users to /", %{token: token} do
      assert {:halt, socket} =
               UserAuth.on_mount(:redirect_if_user, %{}, %{"user_token" => token}, socket())

      assert {:redirect, %{to: "/"}} = socket.redirected
    end

    test "lets guests through with a nil scope" do
      assert {:cont, socket} = UserAuth.on_mount(:redirect_if_user, %{}, %{}, socket())
      assert socket.assigns.current_scope == nil
    end
  end
end
