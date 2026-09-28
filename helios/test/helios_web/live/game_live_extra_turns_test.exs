defmodule HeliosWeb.GameLiveExtraTurnsTest do
  use HeliosWeb.ConnCase, async: false

  import Phoenix.LiveViewTest
  import Helios.GamesFixtures

  alias Helios.{GameDriver, Games}

  setup do
    on_exit(&stop_game_servers/0)
    players = for _ <- 1..3, do: user_with_token_fixture()
    %{players: players, ids: Enum.map(players, fn {user, _token} -> to_string(user.id) end)}
  end

  defp open(token, game), do: live(log_in_conn(build_conn(), token), ~p"/game/#{game.id}")

  # Finds a seed whose deterministic playthrough reaches `stop?`, persists a game
  # with that seed and seating, and drives it there through Helios.Games.
  defp prepare(players, ids, wonders, strategies, stop?) do
    seed = GameDriver.find_seed(ids, wonders, strategies, stop?)
    game = game_fixture(Enum.map(players, &elem(&1, 0)), seed: seed, wonders: wonders)
    {view_fun, submit_fun} = GameDriver.games_funs(game.id)
    assert {:reached, _view} = GameDriver.play_until(ids, strategies, view_fun, submit_fun, stop?)
    game
  end

  test "Halikarnassós builds from the discard pile while the others wait",
       %{players: [{a, ta}, {_b, tb}, _] = players, ids: [ia | rest] = ids} do
    wonders = [{"Halikarnassós", :b}, {"Gizah", :a}, {"Rhódos", :a}]
    strategies = Map.new([{ia, :wonder} | Enum.map(rest, &{&1, :build})])
    stop? = &match?(%{phase: %{kind: :extra_turn, extra_turn_kind: :build_from_discard}}, &1)
    game = prepare(players, ids, wonders, strategies, stop?)

    {:ok, view_a, _html} = open(ta, game)
    {:ok, view_b, _html} = open(tb, game)
    assert has_element?(view_a, "#discard-picker")
    assert has_element?(view_b, "#waiting-extra-turn", a.name)
    refute has_element?(view_b, "#hand")

    {:ok, state} = Games.view(game.id, a.id)
    built = GameDriver.built_names(state, state.me)
    index = Enum.find_index(state.discard_pile, &(&1 not in built))
    card = Enum.at(state.discard_pile, index)

    view_a |> element("#discard-pick-#{index}") |> render_click()

    refute has_element?(view_a, "#discard-picker")
    {:ok, after_pick} = Games.view(game.id, a.id)
    assert card in GameDriver.built_names(after_pick, after_pick.me)
  end

  test "Babylon B plays its last card while the others wait",
       %{players: [{a, ta}, {_b, tb}, _] = players, ids: [ia | rest] = ids} do
    wonders = [{"Babylon", :b}, {"Gizah", :a}, {"Rhódos", :a}]
    strategies = Map.new([{ia, :wonder} | Enum.map(rest, &{&1, :build})])
    stop? = &match?(%{phase: %{kind: :extra_turn, extra_turn_kind: :play_last_card}}, &1)
    game = prepare(players, ids, wonders, strategies, stop?)

    {:ok, view_a, _html} = open(ta, game)
    {:ok, view_b, _html} = open(tb, game)
    assert has_element?(view_a, "#play-last-card")
    assert has_element?(view_a, "#hand-card-0")
    refute has_element?(view_a, "#hand-card-1")
    assert has_element?(view_b, "#waiting-extra-turn", a.name)

    view_a |> element("#hand-card-0") |> render_click()
    view_a |> element("#discard-button") |> render_click()
    refute has_element?(view_a, "#play-last-card")
    assert {:ok, %{phase: %{kind: kind}}} = Games.view(game.id, a.id)
    assert kind in [:choosing_cards, :game_over]
  end

  test "the scoreboard replaces the table at game end, even with a card selected",
       %{players: [{_a, ta}, {_b, tb}, _] = players, ids: ids} do
    game = game_fixture(Enum.map(players, &elem(&1, 0)))
    {:ok, view_a, _html} = open(ta, game)
    view_a |> element("#hand-card-0") |> render_click()
    assert has_element?(view_a, "#action-panel")

    {view_fun, submit_fun} = GameDriver.games_funs(game.id)

    assert {:reached, final} =
             GameDriver.play_until(ids, %{}, view_fun, submit_fun, &(&1.phase.kind == :game_over))

    assert has_element?(view_a, "#scoreboard")
    refute has_element?(view_a, "#hand")
    refute has_element?(view_a, "#action-panel")
    for id <- ids, do: assert(has_element?(view_a, "#score-row-#{id}"))

    winner = Enum.find(final.scores, &(&1.rank == 1))
    assert has_element?(view_a, "#score-row-#{winner.player}[data-rank='1']")
    assert has_element?(view_a, "#back-to-lobby[href='/lobby/#{game.lobby_id}']")

    {:ok, view_b, _html} = open(tb, game)
    assert has_element?(view_b, "#scoreboard")
  end
end
