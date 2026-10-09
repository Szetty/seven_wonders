defmodule HeliosWeb.PastGamesLiveTest do
  use HeliosWeb.ConnCase, async: false

  import Phoenix.LiveViewTest
  import Helios.GamesFixtures

  alias Helios.Repo

  test "lists my finished games with rank and links to them" do
    [{a, ta}, {b, _}, {c, _}] = players = for _ <- 1..3, do: user_with_token_fixture()
    game = game_fixture(Enum.map(players, &elem(&1, 0)))

    scores = %{
      "scores" => [
        %{"player" => to_string(b.id), "total" => 40, "rank" => 1},
        %{"player" => to_string(a.id), "total" => 31, "rank" => 2},
        %{"player" => to_string(c.id), "total" => 20, "rank" => 3}
      ]
    }

    game
    |> Ecto.Changeset.change(
      status: "finished",
      finished_at: ~U[2026-02-01 10:00:00Z],
      final_scores: scores
    )
    |> Repo.update!()

    {:ok, view, _html} = live(log_in_conn(build_conn(), ta), ~p"/games")
    assert has_element?(view, "#past-game-#{game.id} a[href='/game/#{game.id}']")
    assert has_element?(view, "#past-game-#{game.id} [data-my-rank]", "Rank 2")
    assert has_element?(view, "#past-game-#{game.id} [data-winner]", b.name)
    refute has_element?(view, "#past-games-empty")
  end

  test "shows an empty state" do
    {_user, token} = user_with_token_fixture()
    {:ok, view, _html} = live(log_in_conn(build_conn(), token), ~p"/games")
    assert has_element?(view, "#past-games-empty")
  end
end
