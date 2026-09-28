defmodule HeliosWeb.GameLiveTest do
  use HeliosWeb.ConnCase, async: false

  import Ecto.Query
  import Phoenix.LiveViewTest
  import Helios.GamesFixtures

  alias Helios.{Games, Repo}
  alias Helios.Games.{Game, GameAction}

  setup do
    on_exit(&stop_game_servers/0)
    players = for _ <- 1..3, do: user_with_token_fixture()
    game = game_fixture(Enum.map(players, &elem(&1, 0)))
    %{game: game, players: players}
  end

  defp open(token, game_id), do: live(log_in_conn(build_conn(), token), ~p"/game/#{game_id}")

  test "a seated player sees the table", %{game: game, players: [{_a, ta} | _]} do
    {:ok, view, _html} = open(ta, game.id)
    assert has_element?(view, "#top-bar[data-turn-key='1-1']")
    assert has_element?(view, "#my-board")
    assert has_element?(view, "#west-panel")
    assert has_element?(view, "#east-panel")
    refute has_element?(view, "#other-players")
    assert has_element?(view, "#hand-card-6")
    refute has_element?(view, "#hand-card-7")
    refute has_element?(view, "#action-panel")
  end

  test "players who are not seated are sent to their own lobby", %{game: game} do
    {outsider, token} = user_with_token_fixture()
    own = Helios.Lobbies.get_or_create_own_lobby(outsider)
    assert {:error, {:live_redirect, %{to: to, flash: flash}}} = open(token, game.id)
    assert to == ~p"/lobby/#{own.id}"
    assert flash["error"] == "You are not seated at this game"
  end

  test "unknown and malformed game ids redirect with a message", %{players: [{_a, ta} | _]} do
    for id <- [Ecto.UUID.generate(), "not-a-uuid"] do
      assert {:error, {:live_redirect, %{flash: flash}}} = open(ta, id)
      assert flash["error"] == "That game does not exist"
    end
  end

  test "selecting a card toggles its action panel", %{game: game, players: [{_a, ta} | _]} do
    {:ok, view, _html} = open(ta, game.id)
    view |> element("#hand-card-0") |> render_click()
    assert has_element?(view, "#action-panel")
    assert has_element?(view, "#build-options")
    assert has_element?(view, "#wonder-options")
    assert has_element?(view, "#discard-button")

    assert has_element?(view, "#build-options #build-unavailable") or
             has_element?(view, "#build-option-0")

    view |> element("#hand-card-0") |> render_click()
    refute has_element?(view, "#action-panel")
  end

  test "three players submitting advances the turn", %{game: game, players: players} do
    [{_a, _}, {b, _}, {c, _}] = players
    views = Enum.map(players, fn {_user, token} -> elem(open(token, game.id), 1) end)
    [view_a | _] = views

    view_a |> element("#hand-card-0") |> render_click()
    view_a |> element("#discard-button") |> render_click()
    assert has_element?(view_a, "#pending-choice")
    assert has_element?(view_a, "#waiting-for", b.name)
    assert has_element?(view_a, "#waiting-for", c.name)

    for view <- tl(views) do
      view |> element("#hand-card-0") |> render_click()
      view |> element("#discard-button") |> render_click()
    end

    for view <- views do
      assert has_element?(view, "#top-bar[data-turn-key='1-2']")
      refute has_element?(view, "#pending-choice")
      assert has_element?(view, "#hand-card-5")
      refute has_element?(view, "#hand-card-6")
    end
  end

  test "a player can change their choice before the turn resolves", %{
    game: game,
    players: [{a, ta} | _]
  } do
    {:ok, view, _html} = open(ta, game.id)
    {:ok, state} = Games.view(game.id, a.id)
    [first, second | _] = Enum.map(state.hand, & &1.name)

    view |> element("#hand-card-0") |> render_click()
    view |> element("#discard-button") |> render_click()
    assert has_element?(view, "#pending-choice", "Discard #{first}")

    view |> element("#change-choice") |> render_click()
    assert has_element?(view, "#action-panel")
    view |> element("#hand-card-1") |> render_click()
    view |> element("#discard-button") |> render_click()

    assert has_element?(view, "#pending-choice", "Discard #{second}")
    assert {:ok, %{my_pending: {:discard, ^second}}} = Games.view(game.id, a.id)
  end

  test "engine errors and forged choices are flashed and nothing is persisted", %{
    game: game,
    players: [{_a, ta} | _]
  } do
    {:ok, view, _html} = open(ta, game.id)

    render_click(view, "submit", %{"card" => "Not A Card", "kind" => "discard", "option" => "0"})
    assert has_element?(view, "#flash-error", "That card is no longer in your hand")

    render_click(view, "submit", %{"card" => "Not A Card", "kind" => "steal", "option" => "0"})
    assert has_element?(view, "#flash-error", "That choice is not available")

    render_click(view, "submit", %{"card" => "Not A Card", "kind" => "build", "option" => "99"})
    assert has_element?(view, "#flash-error", "That card is no longer in your hand")

    assert Repo.aggregate(from(a in GameAction, where: a.game_id == ^game.id), :count) == 0
    assert has_element?(view, "#hand-card-6")
  end

  test "the page follows the server through an idle stop", %{game: game, players: [{_a, ta} | _]} do
    {:ok, view, _html} = open(ta, game.id)
    stop_game_servers()

    view |> element("#hand-card-0") |> render_click()
    view |> element("#discard-button") |> render_click()
    assert has_element?(view, "#pending-choice")
  end

  test "shows who is connected", %{game: game, players: [{a, ta}, {b, tb}, {c, _}]} do
    {:ok, _view_b, _html} = open(tb, game.id)
    {:ok, view_a, _html} = open(ta, game.id)
    assert has_element?(view_a, "#connection-#{a.id}[data-connected='true']")
    assert has_element?(view_a, "#connection-#{b.id}[data-connected='true']")
    assert has_element?(view_a, "#connection-#{c.id}[data-connected='false']")
  end

  test "an aborted game sends players back to the lobby", %{game: game, players: [{_a, ta} | _]} do
    {:ok, view, _html} = open(ta, game.id)
    Phoenix.PubSub.broadcast(Helios.PubSub, Games.topic(game.id), {:game_aborted})
    flash = assert_redirect(view, ~p"/lobby/#{game.lobby_id}")
    assert flash["error"] == "This game was aborted"
  end

  test "unexpected messages do not crash the page", %{game: game, players: [{_a, ta} | _]} do
    {:ok, view, _html} = open(ta, game.id)
    send(view.pid, {:unexpected, :message})
    assert has_element?(view, "#hand")
  end

  test "a finished game shows its persisted scoreboard without starting the engine", %{
    game: game,
    players: [{a, ta}, {b, _}, {c, _}]
  } do
    final_scores = %{
      "scores" => [
        final_score(a.id, 1, 25),
        final_score(b.id, 2, 19),
        final_score(c.id, 3, 12)
      ]
    }

    game |> Game.finish_changeset(final_scores) |> Repo.update!()
    assert Registry.lookup(Helios.Games.Registry, game.id) == []

    {:ok, view, _html} = open(ta, game.id)

    assert has_element?(view, "#top-bar[data-phase='game_over'][data-turn-key='3-6']")
    assert has_element?(view, "#scoreboard")
    assert has_element?(view, "#score-row-#{a.id}[data-rank='1']", a.name)
    assert has_element?(view, "#score-row-#{b.id}[data-rank='2']", b.name)
    assert has_element?(view, "#score-row-#{c.id}[data-rank='3']", c.name)
    assert has_element?(view, "#back-to-lobby[href='/lobby/#{game.lobby_id}']")
    refute has_element?(view, "#hand")

    assert Registry.lookup(Helios.Games.Registry, game.id) == []
  end

  # The shape the finish path persists: JSON-decoded, string keys.
  defp final_score(user_id, rank, total) do
    %{
      "player" => to_string(user_id),
      "military" => 6,
      "treasury" => 1,
      "wonder" => 4,
      "civilian" => 3,
      "scientific" => 2,
      "commercial" => 1,
      "guild" => 0,
      "total" => total,
      "coins" => 5,
      "rank" => rank
    }
  end
end
