defmodule HeliosWeb.LobbyLiveTest do
  use HeliosWeb.ConnCase, async: false

  import Phoenix.LiveViewTest
  import Helios.LobbiesFixtures

  alias Helios.Lobbies
  alias Phoenix.Socket.Broadcast

  defp count(view, selector) do
    view |> render() |> LazyHTML.from_fragment() |> LazyHTML.query(selector) |> Enum.count()
  end

  defp player_with_lobby(conn) do
    {conn, user} = log_in_player(conn)
    %{conn: conn, user: user, lobby: Lobbies.get_or_create_own_lobby(user)}
  end

  describe "owner view" do
    test "the owner is the only, connected leader with 6 FREE seats", %{conn: conn} do
      %{conn: conn, user: me, lobby: lobby} = player_with_lobby(conn)

      {:ok, view, _html} = live(conn, ~p"/lobby/#{lobby.id}")

      assert has_element?(view, "#lobby-title", "My table")
      assert count(view, "#members-table tr[data-member]") == 1

      assert has_element?(
               view,
               "#member-#{me.id}[data-leader='true'][data-self='true'][data-connected='true']",
               me.name
             )

      assert count(view, "#members-table tr[data-free-slot]") == 6
      assert has_element?(view, "#invite-form #invite_user_id")
      assert has_element?(view, "#invite-button[disabled]")
      refute has_element?(view, "#uninvite-#{me.id}")
      assert has_element?(view, "#my-table-link[href='/']", "My table")
    end

    test "owner invites an online player, who then joins the table", %{conn: conn} do
      a = player_with_lobby(conn)
      b = player_with_lobby(build_conn())

      # B online first, so A's initial presence list already contains B.
      {:ok, _b_own_view, _} = live(b.conn, ~p"/lobby/#{b.lobby.id}")
      {:ok, view_a, _} = live(a.conn, ~p"/lobby/#{a.lobby.id}")

      assert has_element?(view_a, "#invite_user_id option[value='#{b.user.id}']", b.user.name)
      refute has_element?(view_a, "#invite_user_id option[value='#{a.user.id}']")

      view_a
      |> form("#invite-form", invite: %{user_id: to_string(b.user.id)})
      |> render_change()

      refute has_element?(view_a, "#invite-button[disabled]")

      view_a
      |> form("#invite-form", invite: %{user_id: to_string(b.user.id)})
      |> render_submit()

      assert has_element?(
               view_a,
               "#member-#{b.user.id}[data-leader='false'][data-connected='false']",
               b.user.name
             )

      refute has_element?(view_a, "#invite_user_id option[value='#{b.user.id}']")
      assert has_element?(view_a, "#invite-button[disabled]")
      assert count(view_a, "#members-table tr[data-free-slot]") == 5
      assert Lobbies.authorize(a.lobby, b.user) == :ok

      Phoenix.PubSub.subscribe(Helios.PubSub, Lobbies.lobby_topic(a.lobby.id))
      {:ok, view_b, _} = live(b.conn, ~p"/lobby/#{a.lobby.id}")
      b_key = to_string(b.user.id)
      assert_receive %Broadcast{event: "presence_diff", payload: %{joins: %{^b_key => _}}}

      assert has_element?(view_a, "#member-#{b.user.id}[data-connected='true']")

      # Guest view: connected members only, no controls.
      assert has_element?(view_b, "#lobby-title", "#{a.user.name}'s table")
      refute has_element?(view_b, "#invite-form")
      refute has_element?(view_b, "button[id^='uninvite-']")
      assert count(view_b, "#members-table tr[data-free-slot]") == 0

      assert has_element?(
               view_b,
               "#member-#{a.user.id}[data-leader='true'][data-connected='true']"
             )

      assert has_element?(view_b, "#member-#{b.user.id}[data-self='true']")
    end

    test "owner removes an invitee", %{conn: conn} do
      a = player_with_lobby(conn)
      b = player_fixture()
      invite_fixture(a.lobby, a.user, b)

      {:ok, view, _} = live(a.conn, ~p"/lobby/#{a.lobby.id}")
      assert has_element?(view, "#uninvite-#{b.id}[aria-label='Remove #{b.name}']")

      view |> element("#uninvite-#{b.id}") |> render_click()

      refute has_element?(view, "#member-#{b.id}")
      assert Lobbies.authorize(a.lobby, b) == {:error, :unauthorized}
    end

    # Review Focus 1: tampered params never crash the LiveView.
    test "tampered invite/uninvite params show an error flash", %{conn: conn} do
      a = player_with_lobby(conn)
      {:ok, view, _} = live(a.conn, ~p"/lobby/#{a.lobby.id}")

      render_hook(view, "invite", %{"invite" => %{"user_id" => "abc"}})
      assert has_element?(view, "#flash-error", "That player does not exist")

      render_hook(view, "invite", %{"invite" => %{"user_id" => to_string(a.user.id)}})
      assert has_element?(view, "#flash-error", "You can't invite yourself")

      render_hook(view, "invite", %{})
      assert has_element?(view, "#flash-error", "That player does not exist")

      render_hook(view, "uninvite", %{"id" => "abc"})
      assert has_element?(view, "#flash-error", "That invitation is no longer valid")

      render_hook(view, "select_invitee", %{})
      assert has_element?(view, "#lobby")
    end
  end

  describe "guest view" do
    test "lists only connected members", %{conn: conn} do
      a = player_with_lobby(build_conn())
      b = player_with_lobby(conn)
      c = player_fixture()
      invite_fixture(a.lobby, a.user, b.user)
      invite_fixture(a.lobby, a.user, c)

      {:ok, view_b, _} = live(b.conn, ~p"/lobby/#{a.lobby.id}")

      assert count(view_b, "#members-table tr[data-member]") == 1
      assert has_element?(view_b, "#member-#{b.user.id}[data-self='true'][data-connected='true']")
      refute has_element?(view_b, "#member-#{a.user.id}")
      refute has_element?(view_b, "#member-#{c.id}")
    end

    test "non-owner invite and uninvite are rejected server-side", %{conn: conn} do
      a = player_with_lobby(build_conn())
      b = player_with_lobby(conn)
      c = player_fixture()
      invite_fixture(a.lobby, a.user, b.user)

      {:ok, view_b, _} = live(b.conn, ~p"/lobby/#{a.lobby.id}")

      render_hook(view_b, "invite", %{"invite" => %{"user_id" => to_string(c.id)}})
      assert has_element?(view_b, "#flash-error", "Only the table leader can do that")
      assert Lobbies.authorize(a.lobby, c) == {:error, :unauthorized}

      render_hook(view_b, "uninvite", %{"id" => to_string(b.user.id)})
      assert has_element?(view_b, "#flash-error", "Only the table leader can do that")
      assert Lobbies.authorize(a.lobby, b.user) == :ok
    end
  end

  describe "access" do
    test "a malformed game id redirects to the own lobby", %{conn: conn} do
      %{conn: conn, lobby: own} = player_with_lobby(conn)
      own_path = ~p"/lobby/#{own.id}"

      assert {:error, {_kind, %{to: ^own_path, flash: %{"error" => "That table does not exist"}}}} =
               live(conn, ~p"/lobby/not-a-uuid")
    end

    test "an unknown game id redirects to the own lobby", %{conn: conn} do
      %{conn: conn, lobby: own} = player_with_lobby(conn)
      own_path = ~p"/lobby/#{own.id}"

      assert {:error, {_kind, %{to: ^own_path, flash: %{"error" => "That table does not exist"}}}} =
               live(conn, ~p"/lobby/#{Ecto.UUID.generate()}")
    end

    test "an uninvited user is redirected to their own lobby", %{conn: conn} do
      a = player_with_lobby(build_conn())
      %{conn: conn, lobby: own} = player_with_lobby(conn)
      own_path = ~p"/lobby/#{own.id}"

      assert {:error,
              {_kind,
               %{to: ^own_path, flash: %{"error" => "Only invited players can join this table"}}}} =
               live(conn, ~p"/lobby/#{a.lobby.id}")
    end
  end
end
