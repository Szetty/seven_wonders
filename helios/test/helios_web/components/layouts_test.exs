defmodule HeliosWeb.LayoutsTest do
  use ExUnit.Case, async: true

  import Phoenix.Component
  import Phoenix.LiveViewTest

  alias Helios.Accounts.{Scope, User}
  alias HeliosWeb.Layouts

  defp render_app(scope, background \\ nil) do
    assigns = %{scope: scope, background: background}

    rendered_to_string(~H"""
    <Layouts.app flash={%{}} current_scope={@scope} background={@background}>
      <p id="content">Hello</p>
    </Layouts.app>
    """)
    |> LazyHTML.from_fragment()
  end

  defp render_with_tables(tables) do
    assigns = %{scope: Scope.for_user(%User{id: 1, name: "alice"}), tables: tables}

    rendered_to_string(~H"""
    <Layouts.app flash={%{}} current_scope={@scope} recent_tables={@tables}>
      <p id="content">Hello</p>
    </Layouts.app>
    """)
    |> LazyHTML.from_fragment()
  end

  test "the Recent tables menu links each table and starts closed" do
    id = Ecto.UUID.generate()
    doc = render_with_tables([%{lobby_id: id, owner_name: "bob", last_active_at: nil}])

    button = LazyHTML.query(doc, "#site-header #recent-tables-button")
    assert LazyHTML.attribute(button, "aria-controls") == ["recent-tables-menu"]
    assert LazyHTML.attribute(button, "aria-haspopup") == ["menu"]
    assert LazyHTML.attribute(button, "aria-expanded") == ["false"]
    assert [js] = LazyHTML.attribute(button, "phx-click")
    assert js =~ "#recent-tables-menu"

    # Phones: icon only, the label stays for screen readers.
    assert doc |> LazyHTML.query("#recent-tables-button .hero-user-group") |> Enum.count() == 1

    assert doc |> LazyHTML.query("#recent-tables-button .sr-only") |> LazyHTML.text() =~
             "Recent tables"

    wrapper = LazyHTML.query(doc, "#recent-tables")
    assert [_] = LazyHTML.attribute(wrapper, "phx-click-away")
    assert LazyHTML.attribute(wrapper, "phx-key") == ["Escape"]

    [menu_class] = doc |> LazyHTML.query("#recent-tables-menu") |> LazyHTML.attribute("class")
    assert "hidden" in String.split(menu_class)

    link = LazyHTML.query(doc, "#recent-tables-menu #recent-table-#{id}")
    assert LazyHTML.attribute(link, "href") == ["/lobby/#{id}"]
    assert link |> LazyHTML.text() |> String.trim() == "bob's table"
  end

  # Review Focus 5.
  test "no Recent tables button when there are none" do
    doc = render_with_tables([])
    assert doc |> LazyHTML.query("#recent-tables, #recent-tables-button") |> Enum.empty?()
    assert doc |> LazyHTML.query("#site-header #my-table-link") |> Enum.count() == 1
  end

  test "the background class wraps the header and the content" do
    doc = render_app(Scope.for_user(%User{id: 1, name: "alice"}), "bg-test-paper")
    assert doc |> LazyHTML.query(".bg-test-paper #site-header") |> Enum.count() == 1
    assert doc |> LazyHTML.query(".bg-test-paper main #content") |> Enum.count() == 1
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

    assert doc |> LazyHTML.query("#site-header #past-games-link") |> LazyHTML.attribute("href") ==
             ["/games"]

    assert doc |> LazyHTML.query("main #content") |> Enum.count() == 1
  end

  test "renders no header for guests" do
    doc = render_app(nil)

    assert doc |> LazyHTML.query("#site-header") |> Enum.empty?()
    assert doc |> LazyHTML.query("main #content") |> Enum.count() == 1
  end
end
