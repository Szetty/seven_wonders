defmodule Helios.LobbiesTest do
  use Helios.DataCase, async: false

  import Helios.LobbiesFixtures

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
end
