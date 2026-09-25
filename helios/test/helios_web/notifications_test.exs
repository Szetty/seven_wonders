defmodule HeliosWeb.NotificationsTest do
  use HeliosWeb.ConnCase, async: false

  import Phoenix.LiveViewTest
  import Helios.LobbiesFixtures

  alias Helios.Accounts.Scope
  alias Helios.Lobbies
  alias HeliosWeb.Notifications
  alias Phoenix.Socket.Broadcast

  describe "add/2 (pure)" do
    test "newest first" do
      a = Notifications.approve(%{lobby_id: "l1", owner_name: "Ann"})
      s = Notifications.simple("hello")

      assert [] |> Notifications.add(a) |> Notifications.add(s) == [s, a]
      assert a.message == "You are expected on table Ann"
      assert a.id == "invite-l1"
    end

    # Review Focus 4: the same invite delivered twice yields one notification.
    test "de-duplicates by id, moving the notification to the front" do
      a = Notifications.approve(%{lobby_id: "l1", owner_name: "Ann"})
      s = Notifications.simple("hello")

      assert [] |> Notifications.add(a) |> Notifications.add(s) |> Notifications.add(a) == [a, s]
    end

    test "keeps at most 5 by evicting the oldest simple notifications" do
      list =
        Enum.reduce(1..6, [], fn i, acc ->
          Notifications.add(acc, Notifications.simple("s#{i}"))
        end)

      assert Enum.map(list, & &1.message) == ["s6", "s5", "s4", "s3", "s2"]
    end

    test "never evicts approve notifications to make room for simple ones" do
      approves =
        for i <- 1..5, do: Notifications.approve(%{lobby_id: "l#{i}", owner_name: "O#{i}"})

      list =
        approves
        |> Enum.reduce([], &Notifications.add(&2, &1))
        |> Notifications.add(Notifications.simple("late"))

      assert length(list) == 5
      assert Enum.all?(list, &(&1.kind == :approve))
    end

    test "a mix evicts simple before approve" do
      list =
        [
          Notifications.approve(%{lobby_id: "l1", owner_name: "O1"}),
          Notifications.simple("s1"),
          Notifications.approve(%{lobby_id: "l2", owner_name: "O2"}),
          Notifications.simple("s2"),
          Notifications.approve(%{lobby_id: "l3", owner_name: "O3"}),
          Notifications.simple("s3")
        ]
        |> Enum.reduce([], &Notifications.add(&2, &1))

      assert Enum.map(list, & &1.id) |> Enum.filter(&String.starts_with?(&1, "invite-")) ==
               ["invite-l3", "invite-l2", "invite-l1"]

      assert Enum.map(list, & &1.message) |> Enum.reject(&String.starts_with?(&1, "You")) ==
               ["s3", "s2"]
    end

    test "remove/2 and visible/1" do
      items = for i <- 1..7, do: Notifications.approve(%{lobby_id: "l#{i}", owner_name: "O"})

      assert length(Notifications.visible(items)) == 5

      assert Notifications.remove(items, "invite-l3")
             |> Enum.map(& &1.id)
             |> Enum.member?("invite-l3") == false
    end
  end

  defp two_players(conn) do
    {conn_a, a} = log_in_player(conn)
    {conn_b, b} = log_in_player(build_conn())

    %{
      conn_a: conn_a,
      a: a,
      lobby_a: Lobbies.get_or_create_own_lobby(a),
      conn_b: conn_b,
      b: b,
      lobby_b: Lobbies.get_or_create_own_lobby(b)
    }
  end

  describe "hook" do
    test "pending invites are rendered on mount", %{conn: conn} do
      ctx = two_players(conn)
      invite_fixture(ctx.lobby_a, ctx.a, ctx.b)

      {:ok, view_b, _} = live(ctx.conn_b, ~p"/lobby/#{ctx.lobby_b.id}")

      assert has_element?(
               view_b,
               "#notification-invite-#{ctx.lobby_a.id}[data-notification='approve']",
               "You are expected on table #{ctx.a.name}"
             )

      assert has_element?(view_b, "#accept-invite-#{ctx.lobby_a.id}", "Accept")
      assert has_element?(view_b, "#decline-invite-#{ctx.lobby_a.id}", "Decline")
    end

    test "an invite arriving while mounted is shown once", %{conn: conn} do
      ctx = two_players(conn)
      {:ok, view_b, _} = live(ctx.conn_b, ~p"/lobby/#{ctx.lobby_b.id}")

      invite_fixture(ctx.lobby_a, ctx.a, ctx.b)

      assert has_element?(view_b, "#notification-invite-#{ctx.lobby_a.id}")

      assert render(view_b)
             |> LazyHTML.from_fragment()
             |> LazyHTML.query("#notifications [data-notification]")
             |> Enum.count() == 1
    end

    test "accept marks the invite accepted and navigates to the table", %{conn: conn} do
      ctx = two_players(conn)
      invite_fixture(ctx.lobby_a, ctx.a, ctx.b)
      {:ok, view_b, _} = live(ctx.conn_b, ~p"/lobby/#{ctx.lobby_b.id}")

      {:ok, view_on_a, _} =
        view_b
        |> element("#accept-invite-#{ctx.lobby_a.id}")
        |> render_click()
        |> follow_redirect(ctx.conn_b, ~p"/lobby/#{ctx.lobby_a.id}")

      assert has_element?(view_on_a, "#members-table")
      refute has_element?(view_on_a, "#notification-invite-#{ctx.lobby_a.id}")
      assert Lobbies.pending_invites(ctx.b) == []
      assert Lobbies.authorize(ctx.lobby_a, ctx.b) == :ok
    end

    test "decline removes the notification and the authorization; the owner is told", %{
      conn: conn
    } do
      ctx = two_players(conn)
      invite_fixture(ctx.lobby_a, ctx.a, ctx.b)
      {:ok, view_a, _} = live(ctx.conn_a, ~p"/lobby/#{ctx.lobby_a.id}")
      {:ok, view_b, _} = live(ctx.conn_b, ~p"/lobby/#{ctx.lobby_b.id}")

      view_b |> element("#decline-invite-#{ctx.lobby_a.id}") |> render_click()

      refute has_element?(view_b, "#notification-invite-#{ctx.lobby_a.id}")
      assert Lobbies.authorize(ctx.lobby_a, ctx.b) == {:error, :unauthorized}

      assert has_element?(
               view_a,
               "#notifications [data-notification='simple']",
               "User #{ctx.b.name} declined your invitation!"
             )

      refute has_element?(view_a, "#member-#{ctx.b.id}")
    end

    test "simple notifications can be dismissed and expire", %{conn: conn} do
      ctx = two_players(conn)
      invite_fixture(ctx.lobby_a, ctx.a, ctx.b)
      {:ok, view_a, _} = live(ctx.conn_a, ~p"/lobby/#{ctx.lobby_a.id}")
      :ok = Lobbies.decline(Scope.for_user(ctx.b), ctx.lobby_a.id)

      [dom_id] =
        view_a
        |> render()
        |> LazyHTML.from_fragment()
        |> LazyHTML.query("#notifications [data-notification='simple']")
        |> LazyHTML.attribute("id")

      "notification-" <> id = dom_id
      send(view_a.pid, {:expire_notification, id})
      refute has_element?(view_a, "#notifications [data-notification='simple']")

      invite_fixture(ctx.lobby_a, ctx.a, ctx.b)
      :ok = Lobbies.decline(Scope.for_user(ctx.b), ctx.lobby_a.id)
      assert has_element?(view_a, "#notifications [data-notification='simple'] button", "OK")
      view_a |> element("#notifications [data-notification='simple'] button") |> render_click()
      refute has_element?(view_a, "#notifications [data-notification='simple']")
    end

    # Review Focus 5: resolved in another tab.
    test "accepting in one tab clears the notification in the other", %{conn: conn} do
      ctx = two_players(conn)
      invite_fixture(ctx.lobby_a, ctx.a, ctx.b)
      {:ok, tab1, _} = live(ctx.conn_b, ~p"/lobby/#{ctx.lobby_b.id}")
      {:ok, tab2, _} = live(ctx.conn_b, ~p"/lobby/#{ctx.lobby_b.id}")

      tab1 |> element("#accept-invite-#{ctx.lobby_a.id}") |> render_click()

      refute has_element?(tab2, "#notification-invite-#{ctx.lobby_a.id}")
    end

    test "being uninvited while at the table sends the user home with a flash", %{conn: conn} do
      ctx = two_players(conn)
      invite_fixture(ctx.lobby_a, ctx.a, ctx.b)
      :ok = Lobbies.accept(Scope.for_user(ctx.b), ctx.lobby_a.id)
      {:ok, view_b, _} = live(ctx.conn_b, ~p"/lobby/#{ctx.lobby_a.id}")

      :ok = Lobbies.uninvite(Scope.for_user(ctx.a), ctx.lobby_a, ctx.b.id)

      flash = assert_redirect(view_b, ~p"/lobby/#{ctx.lobby_b.id}")
      assert flash["info"] == "You were removed from #{ctx.a.name}'s table"
    end

    test "being uninvited elsewhere replaces the invite with a simple notification", %{conn: conn} do
      ctx = two_players(conn)
      invite_fixture(ctx.lobby_a, ctx.a, ctx.b)
      {:ok, view_b, _} = live(ctx.conn_b, ~p"/lobby/#{ctx.lobby_b.id}")

      :ok = Lobbies.uninvite(Scope.for_user(ctx.a), ctx.lobby_a, ctx.b.id)

      refute has_element?(view_b, "#notification-invite-#{ctx.lobby_a.id}")

      assert has_element?(
               view_b,
               "#notifications [data-notification='simple']",
               "You were removed from #{ctx.a.name}'s table"
             )
    end

    test "a player coming online is announced to others", %{conn: conn} do
      ctx = two_players(conn)
      {:ok, view_a, _} = live(ctx.conn_a, ~p"/lobby/#{ctx.lobby_a.id}")
      Phoenix.PubSub.subscribe(Helios.PubSub, "users:online")

      {:ok, _view_b, _} = live(ctx.conn_b, ~p"/lobby/#{ctx.lobby_b.id}")

      b_key = to_string(ctx.b.id)
      assert_receive %Broadcast{event: "presence_diff", payload: %{joins: %{^b_key => _}}}

      assert has_element?(
               view_a,
               "#notifications [data-notification='simple']",
               "User #{ctx.b.name} got online!"
             )

      assert has_element?(view_a, "#invite_user_id option[value='#{ctx.b.id}']")
    end

    # Review Focus 1: tampered notification events never crash.
    test "tampered notification events are handled", %{conn: conn} do
      ctx = two_players(conn)
      {:ok, view_b, _} = live(ctx.conn_b, ~p"/lobby/#{ctx.lobby_b.id}")

      render_hook(view_b, "accept_invite", %{"id" => "not-a-uuid"})
      assert has_element?(view_b, "#flash-error", "That invitation is no longer valid")

      render_hook(view_b, "decline_invite", %{"id" => Ecto.UUID.generate()})
      assert has_element?(view_b, "#flash-error", "That invitation is no longer valid")

      render_hook(view_b, "accept_invite", %{})
      render_hook(view_b, "decline_invite", %{})
      render_hook(view_b, "dismiss_notification", %{})
      assert has_element?(view_b, "#lobby")
    end
  end
end
