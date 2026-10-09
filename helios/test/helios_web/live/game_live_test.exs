defmodule HeliosWeb.GameLiveTest do
  use HeliosWeb.ConnCase, async: false

  import Ecto.Query
  import Phoenix.LiveViewTest
  import Helios.GamesFixtures

  alias Helios.{Games, Repo}
  alias Helios.Catalogue
  alias Helios.Games.{Game, GameAction}
  alias HeliosWeb.{CardText, GameFormat}

  setup do
    on_exit(&stop_game_servers/0)
    players = for _ <- 1..3, do: user_with_token_fixture()
    game = game_fixture(Enum.map(players, &elem(&1, 0)))
    %{game: game, players: players}
  end

  defp open(token, game_id), do: live(log_in_conn(build_conn(), token), ~p"/game/#{game_id}")

  defp table_of(count) do
    players = for _ <- 1..count, do: user_with_token_fixture()
    {players, game_fixture(Enum.map(players, &elem(&1, 0)))}
  end

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

  test "every board is on the table; others continue from the east neighbour" do
    {players, game} = table_of(5)
    [{_a, ta} | _] = players
    ids = Enum.map(players, fn {user, _} -> to_string(user.id) end)
    {:ok, view, _html} = open(ta, game.id)

    assert has_element?(view, "#my-board[data-player='#{Enum.at(ids, 0)}']")
    assert has_element?(view, "#east-panel[data-player='#{Enum.at(ids, 1)}']")
    assert has_element?(view, "#west-panel[data-player='#{Enum.at(ids, 4)}']")

    html = render(view)

    others =
      html
      |> LazyHTML.from_fragment()
      |> LazyHTML.query("#other-players article")
      |> LazyHTML.attribute("id")

    assert others == ["player-#{Enum.at(ids, 2)}", "player-#{Enum.at(ids, 3)}"]
    assert has_element?(view, "#player-#{Enum.at(ids, 2)} [id^='wonder-stages-']")
  end

  test "a neighbour's tile opens their card sheet with their terms relative to me", %{
    game: game,
    players: players
  } do
    [{a, ta}, {b, _}, {c, _}] = players
    {view_fun, submit_fun} = Helios.GameDriver.games_funs(game.id)
    ids = Enum.map([a, b, c], &to_string(&1.id))

    for _ <- 1..4 do
      Helios.GameDriver.step(ids, Map.new(ids, &{&1, :build}), view_fun, submit_fun)
    end

    {:ok, state} = Games.view(game.id, a.id)
    names = Map.new(players, fn {user, _} -> {to_string(user.id), user.name} end)
    owner = state.east
    owner_player = Enum.find(state.players, &(&1.name == owner))
    owner_rel = GameFormat.neighbours_of(state, owner, names)
    viewer_rel = GameFormat.neighbours_of(state, state.me, names)

    suitable =
      Enum.find(owner_player.built, fn card ->
        info = Catalogue.card(card.name)
        owner_line = info && trade_line(info, owner_rel)
        viewer_line = info && trade_line(info, viewer_rel)
        is_binary(owner_line) and owner_line != viewer_line
      end)

    assert suitable,
           "seed-1 deal must give the east neighbour a built trade card, got: " <>
             inspect(Enum.map(owner_player.built, & &1.name))

    {:ok, view, _html} = open(ta, game.id)
    tile = "#east-panel [data-card='#{suitable.name}']"
    assert has_element?(view, tile)

    [tile_name] =
      view
      |> render()
      |> LazyHTML.from_fragment()
      |> LazyHTML.query(tile)
      |> LazyHTML.attribute("data-card")

    view |> element(tile) |> render_click()
    assert has_element?(view, "#card-sheet #card-info")

    sheet_title =
      view
      |> render()
      |> LazyHTML.from_fragment()
      |> LazyHTML.query("#card-sheet-title")
      |> LazyHTML.text()

    assert String.trim(sheet_title) == tile_name

    info = Catalogue.card(suitable.name)
    owner_line = trade_line(info, owner_rel)
    viewer_line = trade_line(info, viewer_rel)

    seat_ids = Enum.map(state.players, & &1.name)
    owner_index = Enum.find_index(seat_ids, &(&1 == owner))
    owner_east = Enum.at(seat_ids, rem(owner_index + 1, length(seat_ids)))

    assert owner_line =~ "from #{names[owner_east]} for 1 coin"
    assert has_element?(view, "#card-sheet .effect-line", owner_line)
    refute has_element?(view, "#card-sheet .effect-line", viewer_line)

    view |> element("#close-card-sheet") |> render_click()
    refute has_element?(view, "#card-sheet")
  end

  test "the dock holds the hand and the pending choice", %{game: game, players: [{_a, ta} | _]} do
    {:ok, view, _html} = open(ta, game.id)
    assert has_element?(view, "#dock #hand #hand-card-0")

    view |> element("#hand-card-0") |> render_click()
    view |> element("#discard-button") |> render_click()
    assert has_element?(view, "#dock #pending-choice")
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
    assert has_element?(view, "#build-action")
    assert has_element?(view, "#wonder-action")
    assert has_element?(view, "#discard-button")
    assert has_element?(view, "#card-info")

    view |> element("#hand-card-0") |> render_click()
    refute has_element?(view, "#action-panel")
  end

  test "the action sheet closes from the scrim, the close button and Escape", %{
    game: game,
    players: [{_a, ta} | _]
  } do
    {:ok, view, _html} = open(ta, game.id)

    closers = [
      fn view -> view |> element("#action-scrim") |> render_click() end,
      fn view -> view |> element("#close-action-panel") |> render_click() end,
      fn view -> view |> element("#action-panel") |> render_keydown(%{"key" => "Escape"}) end
    ]

    for close <- closers do
      view |> element("#hand-card-0") |> render_click()
      assert has_element?(view, "#action-panel[role='dialog']")
      close.(view)
      refute has_element?(view, "#action-panel")
      refute has_element?(view, "#action-scrim")
      assert has_element?(view, "#hand-card-0[aria-pressed='false']")
    end
  end

  test "other keys and a deselect with nothing selected change nothing", %{
    game: game,
    players: [{_a, ta} | _]
  } do
    {:ok, view, _html} = open(ta, game.id)
    render_click(view, "deselect", %{})
    refute has_element?(view, "#action-panel")

    view |> element("#hand-card-0") |> render_click()
    view |> element("#action-panel") |> render_keydown(%{"key" => "a"})
    assert has_element?(view, "#action-panel")
  end

  test "three players submitting advances the turn", %{game: game, players: players} do
    [{_a, _}, {b, _}, {c, _}] = players
    views = Enum.map(players, fn {_user, token} -> elem(open(token, game.id), 1) end)
    [view_a | _] = views

    view_a |> element("#hand-card-0") |> render_click()
    view_a |> element("#discard-button") |> render_click()
    assert has_element?(view_a, "#pending-choice")
    assert has_element?(view_a, "#my-board [data-status='chosen']")
    assert has_element?(view_a, "#west-panel [data-status='thinking']")
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

  test "a forged show_card on a finished game does not crash the page", %{
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

    {:ok, view, _html} = open(ta, game.id)
    assert has_element?(view, "#scoreboard")

    render_click(view, "show_card", %{"card" => "Lumber Yard", "owner" => to_string(a.id)})

    assert has_element?(view, "#scoreboard")
    refute has_element?(view, "#card-sheet")
  end

  test "the paper background covers the whole page, header included", %{
    game: game,
    players: [{_a, ta} | _]
  } do
    {:ok, view, _html} = open(ta, game.id)
    assert has_element?(view, "[class*='paper.jpg'] #site-header")
    refute has_element?(view, "#game[class*='paper.jpg']")
  end

  # The trade_discount line of a card, rendered for a given neighbour map.
  defp trade_line(info, neighbours) do
    index = Enum.find_index(info.effects, &match?({:trade_discount, _}, &1))
    index && Enum.at(CardText.effects(info.effects, neighbours), index)
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
