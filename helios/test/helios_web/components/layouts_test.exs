defmodule HeliosWeb.LayoutsTest do
  use ExUnit.Case, async: true

  import Phoenix.Component
  import Phoenix.LiveViewTest

  alias Helios.Accounts.{Scope, User}
  alias HeliosWeb.Layouts

  defp render_app(scope) do
    assigns = %{scope: scope}

    rendered_to_string(~H"""
    <Layouts.app flash={%{}} current_scope={@scope}>
      <p id="content">Hello</p>
    </Layouts.app>
    """)
    |> LazyHTML.from_fragment()
  end

  test "shows the site header with the user's name and a logout link" do
    doc = render_app(Scope.for_user(%User{id: 1, name: "alice"}))

    assert doc
           |> LazyHTML.query("#site-header #current-user-name")
           |> LazyHTML.text()
           |> String.trim() == "alice"

    logout = LazyHTML.query(doc, "#site-header #logout-link")
    assert LazyHTML.attribute(logout, "href") == ["/session"]
    assert LazyHTML.attribute(logout, "data-method") == ["delete"]
    assert doc |> LazyHTML.query("main #content") |> Enum.count() == 1
  end

  test "renders no header for guests" do
    doc = render_app(nil)

    assert doc |> LazyHTML.query("#site-header") |> Enum.empty?()
    assert doc |> LazyHTML.query("main #content") |> Enum.count() == 1
  end
end
