defmodule HeliosWeb.LoginLiveTest do
  use HeliosWeb.ConnCase
  import Phoenix.LiveViewTest

  test "renders login page", %{conn: conn} do
    {:ok, _view, html} = live(conn, ~p"/")

    assert html =~ "7 WONDERS"
    assert html =~ "Access Token"
    assert html =~ "Name"
  end

  test "handles login attempt", %{conn: conn} do
    {:ok, view, _html} = live(conn, ~p"/")

    result =
      view
      |> form("#login_form", %{access_token: "test_token", name: "test_user"})
      |> render_submit()

    assert result =~ "Login successful for test_user!"
  end
end
