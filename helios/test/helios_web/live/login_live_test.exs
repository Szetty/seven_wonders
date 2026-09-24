defmodule HeliosWeb.LoginLiveTest do
  use HeliosWeb.ConnCase

  import Phoenix.LiveViewTest
  import Helios.AccountsFixtures

  alias Helios.Accounts
  alias HeliosWeb.Presence

  defp submit(view, access_token, name) do
    view
    |> form("#login_form", login: %{access_token: access_token, name: name})
    |> render_submit()
  end

  describe "rendering" do
    test "shows the login card with both inputs and no site header", %{conn: conn} do
      {:ok, view, _html} = live(conn, ~p"/login")

      assert has_element?(view, "#login-title", "7 WONDERS")
      assert has_element?(view, "#login-card #login_form[action='/session'][method='post']")

      assert has_element?(
               view,
               "#login_form input[name='login[access_token]'][type='password'][autocomplete='off']"
             )

      assert has_element?(
               view,
               "#login_form input[name='login[name]'][type='text'][autocomplete='username']"
             )

      assert has_element?(view, "#login-submit")
      refute has_element?(view, "#login-error")
      refute has_element?(view, "#login_form[phx-trigger-action]")
      refute has_element?(view, "#site-header")
    end

    test "a stale session token still shows the form (no redirect loop)", %{conn: conn} do
      conn = init_test_session(conn, %{user_token: "stale"})
      {:ok, view, _html} = live(conn, ~p"/login")
      assert has_element?(view, "#login_form")
    end
  end

  describe "logged-in users" do
    setup :register_and_log_in_user

    test "are redirected to /", %{conn: conn} do
      assert {:error, {:redirect, %{to: "/"}}} = live(conn, ~p"/login")
    end
  end

  describe "validation" do
    test "shows the legacy messages and never triggers the POST", %{conn: conn} do
      {:ok, view, _html} = live(conn, ~p"/login")

      submit(view, "", "alice")
      assert has_element?(view, "#login-error", "Access token can't be empty!")

      submit(view, "wrong", "")
      assert has_element?(view, "#login-error", "Wrong access token")

      submit(view, valid_access_token(), "   ")
      assert has_element?(view, "#login-error", "Name can't be empty!")

      submit(view, valid_access_token(), String.duplicate("a", 25))
      assert has_element?(view, "#login-error", "Name must be at most 24 characters")

      refute has_element?(view, "#login_form[phx-trigger-action]")
    end

    test "rejects a name held by an online user", %{conn: conn} do
      user = user_fixture()
      {:ok, _ref} = Presence.track_user(self(), user)
      {:ok, view, _html} = live(conn, ~p"/login")

      submit(view, valid_access_token(), user.name)
      assert has_element?(view, "#login-error", "Name is already taken")

      :ok = Presence.untrack(self(), Presence.online_topic(), to_string(user.id))
    end

    test "typing clears the error", %{conn: conn} do
      {:ok, view, _html} = live(conn, ~p"/login")

      submit(view, "", "alice")
      assert has_element?(view, "#login-error")

      view
      |> form("#login_form", login: %{access_token: "x", name: "alice"})
      |> render_change()

      refute has_element?(view, "#login-error")
    end
  end

  describe "successful login" do
    test "triggers the form POST that creates the session", %{conn: conn} do
      name = unique_user_name()
      {:ok, view, _html} = live(conn, ~p"/login")

      form =
        form(view, "#login_form",
          login: %{access_token: valid_access_token(), name: "  #{name} "}
        )

      render_submit(form)
      assert has_element?(view, "#login_form[phx-trigger-action]")

      conn = follow_trigger_action(form, conn)
      assert redirected_to(conn) == ~p"/"
      assert %{name: ^name} = Accounts.get_user_by_session_token(get_session(conn, :user_token))
    end
  end
end
