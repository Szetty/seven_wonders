defmodule HeliosWeb.LoginLiveTest do
  use HeliosWeb.ConnCase
  import Phoenix.LiveViewTest

  test "renders login page", %{conn: conn} do
    {:ok, _view, html} = live(conn, ~p"/")

    assert html =~ "7 WONDERS"
    assert html =~ "Access Token"
    assert html =~ "Name"
  end

  test "namespaces fields under login and attaches no custom Enter hook", %{conn: conn} do
    {:ok, view, _html} = live(conn, ~p"/")

    assert has_element?(view, "#login_form input[name='login[access_token]']")
    assert has_element?(view, "#login_form input[name='login[name]']")
    refute has_element?(view, "#login_form[phx-hook]")
  end

  test "handles login attempt", %{conn: conn} do
    {:ok, view, _html} = live(conn, ~p"/")

    result =
      view
      |> form("#login_form", login: %{access_token: "test_token", name: "test_user"})
      |> render_submit()

    assert result =~ "Login successful for test_user!"
  end

  test "shows an error per blank field, treating whitespace as blank", %{conn: conn} do
    {:ok, view, _html} = live(conn, ~p"/")

    view
    |> form("#login_form", login: %{access_token: "   ", name: ""})
    |> render_submit()

    assert has_element?(view, "#login_errors", "Name can't be empty!")
    assert has_element?(view, "#login_errors", "Access token can't be empty!")
    refute render(view) =~ "Login successful"
  end

  test "validate keeps the typed values in the form", %{conn: conn} do
    {:ok, view, _html} = live(conn, ~p"/")

    view
    |> form("#login_form", login: %{access_token: "", name: "arnold"})
    |> render_change()

    assert has_element?(view, "#login_name[value='arnold']")
  end
end
