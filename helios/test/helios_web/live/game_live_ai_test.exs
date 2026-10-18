defmodule HeliosWeb.GameLiveAITest do
  use HeliosWeb.ConnCase, async: false

  import Phoenix.LiveViewTest
  import Helios.GamesFixtures

  alias Helios.{GameDriver, Games, Repo}
  alias Helios.Games.Game

  setup do
    on_exit(&stop_game_servers/0)
    put_ai_config(players: false)
    {user, token} = user_with_token_fixture()
    game = game_fixture([user, {:ai, "fake-first"}, {:ai, "fake-first"}])
    %{user: user, token: token, game: game}
  end

  defp open(token, game), do: live(log_in_conn(build_conn(), token), ~p"/game/#{game.id}")

  defp ai_discard(game, seat, source) do
    {:ok, view} = Games.view(game.id, {:ai, seat})
    :ok = Games.submit(game.id, {:ai, seat}, {:discard, hd(view.hand).name}, source)
  end

  test "AI seats show their model name, a bot icon and are always connected", %{
    token: token,
    game: game
  } do
    {:ok, lv, _html} = open(token, game)

    assert has_element?(lv, "[data-player='ai-1']", "Fake First")
    assert has_element?(lv, "[data-player='ai-2']", "Fake First #2")
    assert has_element?(lv, "[data-player='ai-1'] [data-ai-icon]")
    refute has_element?(lv, "#my-board [data-ai-icon]")
    assert has_element?(lv, "#connection-ai-1[data-connected='true'] [data-ai-icon]")
  end

  test "a random move is tagged in history and badged for the latest turn only", %{
    user: user,
    token: token,
    game: game
  } do
    {:ok, lv, _html} = open(token, game)

    ai_discard(game, 1, :random)
    ai_discard(game, 2, :ai)
    {:ok, view} = Games.view(game.id, user.id)
    :ok = Games.submit(game.id, user.id, {:discard, hd(view.hand).name})
    _ = render(lv)

    assert has_element?(lv, "#random-badge-ai-1[data-random-badge]")
    refute has_element?(lv, "#random-badge-ai-2")
    html = lv |> element("#history-drawer") |> render()

    assert html |> LazyHTML.from_fragment() |> LazyHTML.query("[data-random]") |> Enum.count() ==
             1

    ai_discard(game, 1, :ai)
    ai_discard(game, 2, :ai)
    {:ok, view} = Games.view(game.id, user.id)
    :ok = Games.submit(game.id, user.id, {:discard, hd(view.hand).name})
    _ = render(lv)

    refute has_element?(lv, "[data-random-badge]")
    assert has_element?(lv, "#history-drawer [data-random]")
  end

  test "finished games with AI seats replay and list in past games", %{
    user: user,
    token: token,
    game: game
  } do
    ids = [to_string(user.id), "ai-1", "ai-2"]
    {view_fun, _} = GameDriver.games_funs(game.id)

    submit = fn
      "ai-" <> seat, action ->
        Games.submit(game.id, {:ai, String.to_integer(seat)}, action, :random)

      _human, action ->
        Games.submit(game.id, user.id, action)
    end

    {:game_over, _} = GameDriver.play_until(ids, %{}, view_fun, submit, fn _ -> false end)
    stop_game_servers()
    assert %Game{status: "finished"} = Repo.get!(Game, game.id)

    {:ok, lv, _html} = open(token, game)
    assert has_element?(lv, "#history-drawer [data-random]")
    refute has_element?(lv, "[data-random-badge]")

    {:ok, _past, html} = live(log_in_conn(build_conn(), token), ~p"/games")
    assert html =~ "Fake First #2"
  end
end
