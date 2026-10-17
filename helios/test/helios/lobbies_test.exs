defmodule Helios.LobbiesTest do
  use Helios.DataCase, async: false

  import Helios.LobbiesFixtures

  alias Helios.Accounts.Scope
  alias Helios.Accounts.User
  alias Helios.Games.Game
  alias Helios.Lobbies
  alias Helios.Lobbies.AISeat
  alias Helios.Lobbies.Invite
  alias Helios.Lobbies.Lobby

  describe "topics" do
    test "lobby and user topics" do
      assert Lobbies.lobby_topic("abc") == "lobby:abc"
      assert Lobbies.user_topic(5) == "user:5"
    end
  end

  describe "get_or_create_own_lobby/1" do
    test "creates the lobby once and returns the same lobby afterwards" do
      user = player_fixture()

      lobby = Lobbies.get_or_create_own_lobby(user)
      assert %Lobby{owner_id: owner_id} = lobby
      assert owner_id == user.id
      assert Lobbies.get_or_create_own_lobby(user).id == lobby.id
      assert Repo.aggregate(from(l in Lobby, where: l.owner_id == ^user.id), :count) == 1
    end

    test "every user gets a distinct lobby" do
      a = player_fixture()
      b = player_fixture()

      refute Lobbies.get_or_create_own_lobby(a).id == Lobbies.get_or_create_own_lobby(b).id
    end
  end

  describe "fetch_lobby/1" do
    test "returns the lobby for its game id" do
      lobby = lobby_fixture(player_fixture())
      lobby_id = lobby.id

      assert {:ok, %Lobby{id: ^lobby_id}} = Lobbies.fetch_lobby(lobby_id)
    end

    test "malformed ids are :invalid_game_id" do
      for id <- ["", "nope", "123", "not-a-uuid-at-all-really", nil, 42, %{}] do
        assert Lobbies.fetch_lobby(id) == {:error, :invalid_game_id}, "for #{inspect(id)}"
      end
    end

    test "an unknown uuid is :invalid_game_id" do
      assert Lobbies.fetch_lobby(Ecto.UUID.generate()) == {:error, :invalid_game_id}
    end
  end

  describe "authorize/2" do
    setup do
      owner = player_fixture()
      %{owner: owner, lobby: lobby_fixture(owner)}
    end

    test "the owner is authorized", %{owner: owner, lobby: lobby} do
      assert Lobbies.authorize(lobby, owner) == :ok
    end

    test "a stranger is not", %{lobby: lobby} do
      assert Lobbies.authorize(lobby, player_fixture()) == {:error, :unauthorized}
    end

    test "pending and accepted invitees are authorized", %{lobby: lobby} do
      pending = player_fixture()
      accepted = player_fixture()
      Repo.insert!(%Invite{lobby_id: lobby.id, user_id: pending.id})
      Repo.insert!(%Invite{lobby_id: lobby.id, user_id: accepted.id, status: "accepted"})

      assert Lobbies.authorize(lobby, pending) == :ok
      assert Lobbies.authorize(lobby, accepted) == :ok
    end
  end

  describe "members/1" do
    test "owner alone is the leader" do
      owner = player_fixture()
      owner_id = owner.id

      assert [%{user: %User{id: ^owner_id}, leader?: true}] =
               Lobbies.members(lobby_fixture(owner))
    end

    test "owner first, then invitees in invitation order" do
      owner = player_fixture()
      lobby = lobby_fixture(owner)
      b = player_fixture()
      c = player_fixture()
      Repo.insert!(%Invite{lobby_id: lobby.id, user_id: b.id})
      Repo.insert!(%Invite{lobby_id: lobby.id, user_id: c.id})

      assert Enum.map(Lobbies.members(lobby), &{&1.user.id, &1.leader?}) ==
               [{owner.id, true}, {b.id, false}, {c.id, false}]
    end
  end

  describe "error_message/1" do
    test "human messages for every error atom" do
      assert Lobbies.error_message(:not_leader) == "Only the table leader can do that"
      assert Lobbies.error_message(:unauthorized) == "Only invited players can join this table"
      assert Lobbies.error_message(:invalid_game_id) == "That table does not exist"
      assert Lobbies.error_message(:self_invite) == "You can't invite yourself"
      assert Lobbies.error_message(:already_invited) == "That player is already invited"
      assert Lobbies.error_message(:lobby_full) == "The table is full (7 players max)"
      assert Lobbies.error_message(:invalid_user) == "That player does not exist"
      assert Lobbies.error_message(:not_invited) == "That invitation is no longer valid"
      assert Lobbies.error_message(:invalid_wonder_mode) == "That board mode does not exist"
    end
  end

  test "max_players/0 is 7" do
    assert Lobbies.max_players() == 7
  end

  defp owner_and_lobby do
    owner = player_fixture()
    %{owner: owner, lobby: lobby_fixture(owner), scope: Scope.for_user(owner)}
  end

  describe "invite/3" do
    test "creates a pending invite that authorizes the invitee" do
      %{lobby: lobby, scope: scope} = owner_and_lobby()
      guest = player_fixture()

      assert {:ok, %Invite{status: "pending"}} = Lobbies.invite(scope, lobby, guest.id)
      assert Lobbies.authorize(lobby, guest) == :ok
    end

    test "accepts the invitee id as a string (form params)" do
      %{lobby: lobby, scope: scope} = owner_and_lobby()
      guest = player_fixture()

      assert {:ok, _} = Lobbies.invite(scope, lobby, Integer.to_string(guest.id))
    end

    test "broadcasts to the invitee (with the owner preloaded) and to the lobby" do
      %{owner: owner, lobby: lobby, scope: scope} = owner_and_lobby()
      guest = player_fixture()
      Phoenix.PubSub.subscribe(Helios.PubSub, Lobbies.user_topic(guest.id))
      Phoenix.PubSub.subscribe(Helios.PubSub, Lobbies.lobby_topic(lobby.id))

      {:ok, _invite} = Lobbies.invite(scope, lobby, guest.id)

      lobby_id = lobby.id
      owner_name = owner.name

      assert_receive {:invited,
                      %Invite{lobby_id: ^lobby_id, lobby: %Lobby{owner: %User{name: ^owner_name}}}}

      assert_receive {:members_changed}
    end

    test "only the owner may invite" do
      %{lobby: lobby} = owner_and_lobby()
      other = player_fixture()

      assert Lobbies.invite(Scope.for_user(other), lobby, player_fixture().id) ==
               {:error, :not_leader}
    end

    test "the owner cannot invite themselves" do
      %{owner: owner, lobby: lobby, scope: scope} = owner_and_lobby()

      assert Lobbies.invite(scope, lobby, owner.id) == {:error, :self_invite}
    end

    test "inviting twice is :already_invited, whatever the status" do
      %{lobby: lobby, scope: scope} = owner_and_lobby()
      guest = player_fixture()
      {:ok, _} = Lobbies.invite(scope, lobby, guest.id)

      assert Lobbies.invite(scope, lobby, guest.id) == {:error, :already_invited}

      :ok = Lobbies.accept(Scope.for_user(guest), lobby.id)
      assert Lobbies.invite(scope, lobby, guest.id) == {:error, :already_invited}
    end

    test "unknown or malformed user ids are :invalid_user" do
      %{lobby: lobby, scope: scope} = owner_and_lobby()

      for id <- [0, -1, 9_999_999, "abc", "", "12abc", nil, 1.5] do
        assert Lobbies.invite(scope, lobby, id) == {:error, :invalid_user}, "for #{inspect(id)}"
      end
    end

    test "a table holds at most 7 players, owner included" do
      %{lobby: lobby, scope: scope} = owner_and_lobby()

      for _ <- 1..6 do
        assert {:ok, _} = Lobbies.invite(scope, lobby, player_fixture().id)
      end

      assert Lobbies.invite(scope, lobby, player_fixture().id) == {:error, :lobby_full}
      assert length(Lobbies.members(lobby)) == 7
    end
  end

  describe "uninvite/3" do
    test "removes authorization and broadcasts" do
      %{owner: owner, lobby: lobby, scope: scope} = owner_and_lobby()
      guest = player_fixture()
      invite_fixture(lobby, owner, guest)
      Phoenix.PubSub.subscribe(Helios.PubSub, Lobbies.user_topic(guest.id))
      Phoenix.PubSub.subscribe(Helios.PubSub, Lobbies.lobby_topic(lobby.id))

      assert Lobbies.uninvite(scope, lobby, Integer.to_string(guest.id)) == :ok
      assert Lobbies.authorize(lobby, guest) == {:error, :unauthorized}

      lobby_id = lobby.id
      assert_receive {:uninvited, ^lobby_id}
      assert_receive {:members_changed}
    end

    test "only the owner may uninvite" do
      %{owner: owner, lobby: lobby} = owner_and_lobby()
      guest = player_fixture()
      invite_fixture(lobby, owner, guest)

      assert Lobbies.uninvite(Scope.for_user(guest), lobby, guest.id) == {:error, :not_leader}
      assert Lobbies.authorize(lobby, guest) == :ok
    end

    test "uninviting someone who is not invited is :not_invited" do
      %{owner: owner, lobby: lobby, scope: scope} = owner_and_lobby()

      assert Lobbies.uninvite(scope, lobby, player_fixture().id) == {:error, :not_invited}
      assert Lobbies.uninvite(scope, lobby, owner.id) == {:error, :not_invited}
      assert Lobbies.uninvite(scope, lobby, "abc") == {:error, :not_invited}
    end
  end

  describe "accept/2" do
    test "marks the invite accepted, keeps authorization, broadcasts :invite_resolved" do
      %{owner: owner, lobby: lobby} = owner_and_lobby()
      guest = player_fixture()
      invite_fixture(lobby, owner, guest)
      Phoenix.PubSub.subscribe(Helios.PubSub, Lobbies.user_topic(guest.id))

      assert Lobbies.accept(Scope.for_user(guest), lobby.id) == :ok

      lobby_id = lobby.id
      assert_receive {:invite_resolved, ^lobby_id}
      assert Repo.get_by!(Invite, lobby_id: lobby.id, user_id: guest.id).status == "accepted"
      assert Lobbies.authorize(lobby, guest) == :ok
      assert Lobbies.pending_invites(guest) == []
    end

    test "is idempotent" do
      %{owner: owner, lobby: lobby} = owner_and_lobby()
      guest = player_fixture()
      invite_fixture(lobby, owner, guest)

      assert Lobbies.accept(Scope.for_user(guest), lobby.id) == :ok
      assert Lobbies.accept(Scope.for_user(guest), lobby.id) == :ok
    end

    test "without an invite or with a malformed id it is :not_invited" do
      %{lobby: lobby} = owner_and_lobby()
      stranger = Scope.for_user(player_fixture())

      assert Lobbies.accept(stranger, lobby.id) == {:error, :not_invited}
      assert Lobbies.accept(stranger, "not-a-uuid") == {:error, :not_invited}
      assert Lobbies.accept(stranger, nil) == {:error, :not_invited}
    end
  end

  describe "decline/2" do
    test "deletes the invite and broadcasts to the lobby and the invitee" do
      %{owner: owner, lobby: lobby} = owner_and_lobby()
      guest = player_fixture()
      invite_fixture(lobby, owner, guest)
      Phoenix.PubSub.subscribe(Helios.PubSub, Lobbies.user_topic(guest.id))
      Phoenix.PubSub.subscribe(Helios.PubSub, Lobbies.lobby_topic(lobby.id))

      assert Lobbies.decline(Scope.for_user(guest), lobby.id) == :ok
      assert Lobbies.authorize(lobby, guest) == {:error, :unauthorized}

      lobby_id = lobby.id
      guest_id = guest.id
      assert_receive {:declined, %User{id: ^guest_id}}
      assert_receive {:invite_resolved, ^lobby_id}
    end

    test "an accepted invite can no longer be declined" do
      %{owner: owner, lobby: lobby} = owner_and_lobby()
      guest = player_fixture()
      invite_fixture(lobby, owner, guest)
      :ok = Lobbies.accept(Scope.for_user(guest), lobby.id)

      assert Lobbies.decline(Scope.for_user(guest), lobby.id) == {:error, :not_invited}
      assert Lobbies.authorize(lobby, guest) == :ok
    end

    test "without an invite or with a malformed id it is :not_invited" do
      %{lobby: lobby} = owner_and_lobby()
      stranger = Scope.for_user(player_fixture())

      assert Lobbies.decline(stranger, lobby.id) == {:error, :not_invited}
      assert Lobbies.decline(stranger, "garbage") == {:error, :not_invited}
    end
  end

  describe "set_wonder_mode/3" do
    test "the owner sets the mode, which is persisted and broadcast" do
      %{lobby: lobby, scope: scope} = owner_and_lobby()
      assert lobby.wonder_mode == "random"
      Phoenix.PubSub.subscribe(Helios.PubSub, Lobbies.lobby_topic(lobby.id))

      for mode <- ~w(choose_side choose_board random) do
        assert {:ok, %Lobby{wonder_mode: ^mode}} = Lobbies.set_wonder_mode(scope, lobby, mode)
        assert_receive {:settings_changed}
        assert Repo.get!(Lobby, lobby.id).wonder_mode == mode
      end
    end

    test "only the owner may change it" do
      %{owner: owner, lobby: lobby} = owner_and_lobby()
      guest = player_fixture()
      invite_fixture(lobby, owner, guest)
      Phoenix.PubSub.subscribe(Helios.PubSub, Lobbies.lobby_topic(lobby.id))

      assert {:error, :not_leader} =
               Lobbies.set_wonder_mode(Scope.for_user(guest), lobby, "choose_board")

      assert Repo.get!(Lobby, lobby.id).wonder_mode == "random"
      refute_received {:settings_changed}
    end

    test "unknown modes are rejected without creating atoms" do
      %{lobby: lobby, scope: scope} = owner_and_lobby()
      name = "never_a_mode_#{System.unique_integer([:positive])}"

      for mode <- [name, "", nil, 3, "Random"] do
        assert Lobbies.set_wonder_mode(scope, lobby, mode) == {:error, :invalid_wonder_mode},
               "accepted #{inspect(mode)}"
      end

      assert Repo.get!(Lobby, lobby.id).wonder_mode == "random"
      assert_raise ArgumentError, fn -> String.to_existing_atom(name) end
    end
  end

  describe "AI seats" do
    test "the owner adds ready models; seats are listed in order and broadcast" do
      %{lobby: lobby, scope: scope} = owner_and_lobby()
      Phoenix.PubSub.subscribe(Helios.PubSub, Lobbies.lobby_topic(lobby.id))

      assert {:ok, %AISeat{model_id: "fake-first"}} =
               Lobbies.add_ai_seat(scope, lobby, "fake-first")

      assert_receive {:members_changed}
      assert {:ok, _} = Lobbies.add_ai_seat(scope, lobby, "fake-first")
      assert {:ok, _} = Lobbies.add_ai_seat(scope, lobby, "fake-last")

      assert Enum.map(Lobbies.ai_seats(lobby), & &1.model_id) ==
               ~w(fake-first fake-first fake-last)
    end

    test "unknown or unready models are rejected" do
      %{lobby: lobby, scope: scope} = owner_and_lobby()

      for model <- ["fake-down", "not-configured", nil, 3] do
        assert Lobbies.add_ai_seat(scope, lobby, model) == {:error, :model_unavailable}
      end

      assert Lobbies.ai_seats(lobby) == []
    end

    test "only the owner adds or removes" do
      %{owner: owner, lobby: lobby, scope: scope} = owner_and_lobby()
      guest = player_fixture()
      invite_fixture(lobby, owner, guest)
      {:ok, seat} = Lobbies.add_ai_seat(scope, lobby, "fake-first")

      assert {:error, :not_leader} =
               Lobbies.add_ai_seat(Scope.for_user(guest), lobby, "fake-first")

      assert {:error, :not_leader} = Lobbies.remove_ai_seat(Scope.for_user(guest), lobby, seat.id)
      assert length(Lobbies.ai_seats(lobby)) == 1
    end

    test "remove deletes the seat; a second remove is :no_ai_seat" do
      %{lobby: lobby, scope: scope} = owner_and_lobby()
      {:ok, seat} = Lobbies.add_ai_seat(scope, lobby, "fake-first")
      Phoenix.PubSub.subscribe(Helios.PubSub, Lobbies.lobby_topic(lobby.id))

      assert :ok = Lobbies.remove_ai_seat(scope, lobby, to_string(seat.id))
      assert_receive {:members_changed}
      assert Lobbies.ai_seats(lobby) == []
      assert {:error, :no_ai_seat} = Lobbies.remove_ai_seat(scope, lobby, seat.id)
      assert {:error, :no_ai_seat} = Lobbies.remove_ai_seat(scope, lobby, "nope")
    end

    test "AI seats and invites share the 7 seats" do
      %{owner: owner, lobby: lobby, scope: scope} = owner_and_lobby()
      for _ <- 1..4, do: invite_fixture(lobby, owner, player_fixture())
      assert {:ok, _} = Lobbies.add_ai_seat(scope, lobby, "fake-first")
      assert {:ok, _} = Lobbies.add_ai_seat(scope, lobby, "fake-first")
      assert {:error, :lobby_full} = Lobbies.add_ai_seat(scope, lobby, "fake-first")
      assert {:error, :lobby_full} = Lobbies.invite(scope, lobby, player_fixture().id)
    end

    test "error messages" do
      assert Lobbies.error_message(:model_unavailable) ==
               "That AI model is not available right now"

      assert Lobbies.error_message(:no_ai_seat) == "That AI player is no longer at the table"
    end
  end

  describe "pending_invites/1 and owner_name/1" do
    test "lists pending invites newest first with the owner's name" do
      guest = player_fixture()
      first = owner_and_lobby()
      second = owner_and_lobby()
      invite_fixture(first.lobby, first.owner, guest)
      invite_fixture(second.lobby, second.owner, guest)

      assert Lobbies.pending_invites(guest) == [
               %{lobby_id: second.lobby.id, owner_name: second.owner.name},
               %{lobby_id: first.lobby.id, owner_name: first.owner.name}
             ]
    end

    test "owner_name/1" do
      %{owner: owner, lobby: lobby} = owner_and_lobby()

      assert Lobbies.owner_name(lobby.id) == owner.name
      assert Lobbies.owner_name(Ecto.UUID.generate()) == nil
      assert Lobbies.owner_name("nope") == nil
    end
  end

  describe "touch_visit/2" do
    test "stamps last_visited_at on the user's invite for that lobby only" do
      guest = player_fixture()
      first = owner_and_lobby()
      second = owner_and_lobby()
      invite_fixture(first.lobby, first.owner, guest)
      invite_fixture(second.lobby, second.owner, guest)

      assert Repo.get_by!(Invite, lobby_id: first.lobby.id, user_id: guest.id).last_visited_at ==
               nil

      assert Lobbies.touch_visit(Scope.for_user(guest), first.lobby.id) == :ok

      assert %DateTime{} =
               Repo.get_by!(Invite, lobby_id: first.lobby.id, user_id: guest.id).last_visited_at

      assert Repo.get_by!(Invite, lobby_id: second.lobby.id, user_id: guest.id).last_visited_at ==
               nil
    end

    test "is a no-op without an invite or with a malformed id" do
      %{lobby: lobby, scope: owner_scope} = owner_and_lobby()
      stranger = Scope.for_user(player_fixture())

      assert Lobbies.touch_visit(stranger, lobby.id) == :ok
      assert Lobbies.touch_visit(owner_scope, lobby.id) == :ok
      assert Lobbies.touch_visit(stranger, "not-a-uuid") == :ok
      assert Lobbies.touch_visit(stranger, nil) == :ok
    end
  end

  describe "accept_pending/2" do
    test "accepts a pending invite and broadcasts :invite_resolved" do
      %{owner: owner, lobby: lobby} = owner_and_lobby()
      guest = player_fixture()
      invite_fixture(lobby, owner, guest)
      Phoenix.PubSub.subscribe(Helios.PubSub, Lobbies.user_topic(guest.id))

      assert Lobbies.accept_pending(Scope.for_user(guest), lobby.id) == :ok

      lobby_id = lobby.id
      assert_receive {:invite_resolved, ^lobby_id}
      assert Repo.get_by!(Invite, lobby_id: lobby.id, user_id: guest.id).status == "accepted"
    end

    # Review Focus 1: an accepted invite is never re-accepted on arrival.
    test "does nothing for an accepted, missing or malformed invite" do
      %{owner: owner, lobby: lobby, scope: owner_scope} = owner_and_lobby()
      guest = player_fixture()
      invite_fixture(lobby, owner, guest)
      :ok = Lobbies.accept(Scope.for_user(guest), lobby.id)
      Phoenix.PubSub.subscribe(Helios.PubSub, Lobbies.user_topic(guest.id))
      Phoenix.PubSub.subscribe(Helios.PubSub, Lobbies.user_topic(owner.id))

      assert Lobbies.accept_pending(Scope.for_user(guest), lobby.id) == :none
      assert Lobbies.accept_pending(owner_scope, lobby.id) == :none
      assert Lobbies.accept_pending(Scope.for_user(guest), "not-a-uuid") == :none
      assert Lobbies.accept_pending(Scope.for_user(guest), nil) == :none
      refute_receive {:invite_resolved, _}
    end
  end

  describe "recent_tables/1" do
    defp table_for(guest, accept? \\ true) do
      %{owner: owner, lobby: lobby} = owner_and_lobby()
      invite_fixture(lobby, owner, guest)
      if accept?, do: :ok = Lobbies.accept(Scope.for_user(guest), lobby.id)
      %{owner: owner, lobby: lobby}
    end

    defp visited(%{lobby: lobby}, guest, at) do
      Repo.update_all(
        from(i in Invite, where: i.lobby_id == ^lobby.id and i.user_id == ^guest.id),
        set: [last_visited_at: at]
      )
    end

    # Finished, so several games may share one lobby.
    defp played(%{lobby: lobby}, users, at) do
      game = Helios.GamesFixtures.game_fixture(users, lobby: lobby)

      Repo.update_all(from(g in Game, where: g.id == ^game.id),
        set: [inserted_at: at, status: "finished"]
      )
    end

    test "orders by the later of last visit and last game played, never-active last" do
      guest = player_fixture()
      visit_only = table_for(guest)
      play_only = table_for(guest)
      both = table_for(guest)
      idle = table_for(guest)

      visited(visit_only, guest, ~U[2026-10-01 10:00:00Z])
      played(play_only, [play_only.owner, guest], ~U[2026-10-03 10:00:00Z])
      visited(both, guest, ~U[2026-09-01 10:00:00Z])
      played(both, [both.owner, guest], ~U[2026-09-15 10:00:00Z])
      played(both, [both.owner, guest], ~U[2026-10-02 10:00:00Z])

      assert Lobbies.recent_tables(guest) == [
               %{
                 lobby_id: play_only.lobby.id,
                 owner_name: play_only.owner.name,
                 last_active_at: ~U[2026-10-03 10:00:00Z]
               },
               %{
                 lobby_id: both.lobby.id,
                 owner_name: both.owner.name,
                 last_active_at: ~U[2026-10-02 10:00:00Z]
               },
               %{
                 lobby_id: visit_only.lobby.id,
                 owner_name: visit_only.owner.name,
                 last_active_at: ~U[2026-10-01 10:00:00Z]
               },
               %{lobby_id: idle.lobby.id, owner_name: idle.owner.name, last_active_at: nil}
             ]
    end

    # Review Focus 3.
    test "pending, uninvited and own tables are excluded" do
      guest = player_fixture()
      _pending = table_for(guest, false)
      removed = table_for(guest)
      :ok = Lobbies.uninvite(Scope.for_user(removed.owner), removed.lobby, guest.id)
      own = %{lobby: lobby_fixture(guest)}
      played(own, [guest, player_fixture()], ~U[2026-10-05 10:00:00Z])

      assert Lobbies.recent_tables(guest) == []
    end

    # Review Focus 3.
    test "a game the user was not seated in does not count" do
      guest = player_fixture()
      table = table_for(guest)
      visited(table, guest, ~U[2026-10-01 10:00:00Z])
      played(table, [table.owner, player_fixture()], ~U[2026-10-09 10:00:00Z])

      assert [%{last_active_at: ~U[2026-10-01 10:00:00Z]}] = Lobbies.recent_tables(guest)
    end

    test "keeps the 5 most recent" do
      guest = player_fixture()

      tables =
        for day <- 1..6 do
          table = table_for(guest)
          visited(table, guest, DateTime.new!(Date.new!(2026, 10, day), ~T[10:00:00], "Etc/UTC"))
          table
        end

      expected = tables |> Enum.reverse() |> Enum.take(5) |> Enum.map(& &1.lobby.id)
      assert Enum.map(Lobbies.recent_tables(guest), & &1.lobby_id) == expected
    end
  end
end
