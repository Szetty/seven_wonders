defmodule Helios.LobbiesTest do
  use Helios.DataCase, async: false

  import Helios.LobbiesFixtures

  alias Helios.Accounts.Scope
  alias Helios.Accounts.User
  alias Helios.Lobbies
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
end
