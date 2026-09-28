defmodule Helios.Games.GameTest do
  use Helios.DataCase, async: false

  import Helios.GamesFixtures

  alias Helios.Lobbies
  alias Helios.Games.{Game, GameAction, GamePlayer}

  setup do
    owner = player_fixture()
    %{owner: owner, lobby: Lobbies.get_or_create_own_lobby(owner)}
  end

  defp attrs, do: %{seed: 1, engine_version: 1, wonders: nil}

  test "allows only one active game per lobby", %{lobby: lobby} do
    assert {:ok, first} = Repo.insert(Game.create_changeset(%Game{lobby_id: lobby.id}, attrs()))
    assert first.status == "active"

    assert {:error, changeset} = Repo.insert(Game.create_changeset(%Game{lobby_id: lobby.id}, attrs()))
    assert %{lobby_id: ["game in progress"]} = errors_on(changeset)

    Repo.update!(Game.finish_changeset(first, %{"scores" => []}))
    assert {:ok, _second} = Repo.insert(Game.create_changeset(%Game{lobby_id: lobby.id}, attrs()))
  end

  test "an aborted game frees the lobby too", %{lobby: lobby} do
    {:ok, first} = Repo.insert(Game.create_changeset(%Game{lobby_id: lobby.id}, attrs()))
    Repo.update!(Game.abort_changeset(first))
    assert {:ok, _} = Repo.insert(Game.create_changeset(%Game{lobby_id: lobby.id}, attrs()))
  end

  test "seed must fit in a signed 64-bit integer", %{lobby: lobby} do
    max = 2 ** 63 - 1

    assert {:ok, game} =
             Repo.insert(Game.create_changeset(%Game{lobby_id: lobby.id}, %{attrs() | seed: max}))

    assert Repo.get!(Game, game.id).seed == max

    for bad <- [-1, 2 ** 63] do
      changeset = Game.create_changeset(%Game{lobby_id: lobby.id}, %{attrs() | seed: bad})
      refute changeset.valid?
    end
  end

  test "finish_changeset stores scores and the finish time", %{lobby: lobby} do
    {:ok, game} = Repo.insert(Game.create_changeset(%Game{lobby_id: lobby.id}, attrs()))
    finished = Repo.update!(Game.finish_changeset(game, %{"scores" => [%{"player" => "1"}]}))
    assert finished.status == "finished"
    assert %DateTime{} = finished.finished_at
    assert Repo.get!(Game, game.id).final_scores == %{"scores" => [%{"player" => "1"}]}
  end

  test "game_fixture seats players in order and deleting a game cascades", %{owner: owner} do
    users = [owner, player_fixture(), player_fixture()]
    game = game_fixture(users)

    seats = Repo.all(from p in GamePlayer, where: p.game_id == ^game.id, order_by: p.seat)
    assert Enum.map(seats, &{&1.seat, &1.user_id}) == Enum.with_index(users, fn u, i -> {i, u.id} end)
    assert game.wonders == %{"explicit" => [["Gizah", "a"], ["Rhódos", "a"], ["Éphesos", "a"]]}

    Repo.insert!(%GameAction{game_id: game.id, seq: 1, user_id: owner.id, action: %{"type" => "discard", "card" => "Altar"}})
    Repo.delete!(game)
    assert Repo.aggregate(from(p in GamePlayer, where: p.game_id == ^game.id), :count) == 0
    assert Repo.aggregate(from(a in GameAction, where: a.game_id == ^game.id), :count) == 0
  end
end
