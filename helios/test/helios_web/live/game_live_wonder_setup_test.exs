defmodule HeliosWeb.GameLiveWonderSetupTest do
  use HeliosWeb.ConnCase, async: false

  import Ecto.Query
  import Phoenix.LiveViewTest
  import Helios.GamesFixtures

  alias Helios.{Games, Repo}
  alias Helios.Games.GameAction

  setup do
    on_exit(&stop_game_servers/0)
    %{players: for(_ <- 1..3, do: user_with_token_fixture())}
  end

  defp open(token, game_id), do: live(log_in_conn(build_conn(), token), ~p"/game/#{game_id}")

  defp start(players, selection) do
    game = game_fixture(Enum.map(players, &elem(&1, 0)), selection: selection)
    views = Enum.map(players, fn {_user, token} -> elem(open(token, game.id), 1) end)
    {game, views}
  end

  defp action_count(game),
    do: Repo.aggregate(from(a in GameAction, where: a.game_id == ^game.id), :count)

  test "choose_board: claim, flip, switch and confirm; Age I starts once everyone confirmed",
       %{players: players} do
    [{a, _}, {b, _}, {c, _}] = players
    {game, [view_a, view_b, view_c] = views} = start(players, :choose_board)

    for view <- views do
      assert has_element?(view, "#wonder-setup[data-mode='choose_board']")
      assert has_element?(view, "#top-bar[data-phase='setup']")
      assert has_element?(view, "#age-label", "Choosing boards")
      refute has_element?(view, "#table")
      refute has_element?(view, "#dock")
      assert has_element?(view, "#confirm-wonder[disabled]")
      assert has_element?(view, "#history-empty")
    end

    tiles =
      view_a
      |> render()
      |> LazyHTML.from_fragment()
      |> LazyHTML.query("#setup-boards article[data-wonder]")

    assert Enum.count(tiles) == 7

    view_a |> element("#claim-gizah") |> render_click()

    assert has_element?(
             view_a,
             "#setup-wonder-gizah[data-holder='#{a.id}'][data-mine][data-side='a']"
           )

    refute has_element?(view_a, "#confirm-wonder[disabled]")
    assert has_element?(view_b, "#claim-gizah[disabled]")
    assert has_element?(view_b, "#setup-wonder-gizah [data-holder-name]", a.name)

    view_a |> element("#side-gizah-b") |> render_click()
    assert has_element?(view_b, "#setup-wonder-gizah[data-side='b']")

    view_a |> element("#claim-rhodos") |> render_click()
    refute has_element?(view_a, "#setup-wonder-gizah[data-holder]")
    assert has_element?(view_a, "#setup-wonder-rhodos[data-mine][data-side='a']")

    # A free board's side toggle only changes what this player sees.
    view_b |> element("#side-ephesos-b") |> render_click()
    assert has_element?(view_b, "#setup-wonder-ephesos[data-side='b']")
    refute has_element?(view_b, "#setup-wonder-ephesos[data-holder]")
    assert has_element?(view_a, "#setup-wonder-ephesos[data-side='a']")
    view_b |> element("#claim-ephesos") |> render_click()
    assert has_element?(view_a, "#setup-wonder-ephesos[data-holder='#{b.id}'][data-side='b']")

    view_c |> element("#claim-gizah") |> render_click()

    view_a |> element("#confirm-wonder") |> render_click()
    assert has_element?(view_a, "#confirm-wonder[disabled]")
    assert has_element?(view_a, "#setup-waiting", b.name)
    assert has_element?(view_a, "#setup-waiting", c.name)
    assert has_element?(view_a, "#waiting-for", c.name)
    assert has_element?(view_a, "#side-rhodos-b[disabled]")
    assert has_element?(view_b, "#setup-wonder-rhodos [data-confirmed]")
    assert has_element?(view_b, "#connection-#{a.id}[data-status='chosen']")

    view_b |> element("#confirm-wonder") |> render_click()
    view_c |> element("#confirm-wonder") |> render_click()

    for view <- views do
      assert has_element?(view, "#top-bar[data-phase='choosing_cards'][data-turn-key='1-1']")
      refute has_element?(view, "#wonder-setup")
      # Seven cards dealt: hand-card-0..6 exist, hand-card-7 does not (as in game_live_test.exs).
      assert has_element?(view, "#hand-card-6")
      refute has_element?(view, "#hand-card-7")
      assert has_element?(view, "#history-empty")
    end

    assert has_element?(view_a, "#my-board img[alt='Rhódos A']")
    assert has_element?(view_b, "#my-board img[alt='Éphesos B']")
    assert has_element?(view_c, "#my-board img[alt='Gizah A']")
    # gizah, flip, rhodos (A) + ephesos (B) + gizah (C) + three confirmations
    assert action_count(game) == 8
  end

  test "choose_side: each player picks a side of their drawn board", %{players: players} do
    [{a, _}, {b, _}, _] = players
    {game, [view_a, view_b, view_c]} = start(players, :choose_side)
    {:ok, %{setup: setup}} = Games.view(game.id, a.id)
    drawn = Map.new(setup.seats, &{&1.player, &1.wonder})
    mine = drawn[to_string(a.id)]

    assert has_element?(view_a, "#wonder-setup[data-mode='choose_side']")
    assert has_element?(view_a, "#setup-side-a img[alt='#{mine} A']")
    assert has_element?(view_a, "#setup-side-b img[alt='#{mine} B']")
    assert has_element?(view_a, "#confirm-wonder[disabled]")
    assert has_element?(view_a, "#setup-seat-#{b.id}", drawn[to_string(b.id)])
    refute has_element?(view_a, "#setup-seat-#{a.id}")

    view_a |> element("#setup-side-b") |> render_click()
    assert has_element?(view_a, "#setup-side-b[aria-pressed='true']")
    assert has_element?(view_b, "#setup-seat-#{a.id}", "#{mine} · B")
    refute has_element?(view_a, "#confirm-wonder[disabled]")

    render_click(view_a, "claim_wonder", %{"wonder" => drawn[to_string(b.id)], "side" => "a"})
    assert has_element?(view_a, "#flash-error", "That is not your board")

    view_b |> element("#setup-side-a") |> render_click()
    view_c |> element("#setup-side-a") |> render_click()
    for view <- [view_a, view_b, view_c], do: view |> element("#confirm-wonder") |> render_click()

    assert has_element?(view_a, "#top-bar[data-phase='choosing_cards'][data-turn-key='1-1']")
    assert has_element?(view_a, "#my-board img[alt='#{mine} B']")
    assert has_element?(view_b, "#my-board img[alt='#{drawn[to_string(b.id)]} A']")
  end

  test "two players racing for one board: the second gets a flash and nothing is logged",
       %{players: players} do
    [{a, _}, _, _] = players
    {game, [view_a, view_b, _]} = start(players, :choose_board)

    view_a |> element("#claim-babylon") |> render_click()
    # B tapped the board before A's claim reached their page.
    render_click(view_b, "claim_wonder", %{"wonder" => "Babylon", "side" => "a"})

    assert has_element?(view_b, "#flash-error", "Babylon was just taken")
    assert has_element?(view_b, "#setup-wonder-babylon[data-holder='#{a.id}']")
    refute has_element?(view_b, "#setup-boards [data-mine]")
    assert has_element?(view_b, "#confirm-wonder[disabled]")
    assert action_count(game) == 1
  end

  test "refreshing mid-setup, even after a server restart, keeps claims and confirmations",
       %{players: players} do
    [{a, ta}, {_b, tb}, _] = players
    {game, [view_a, view_b, _]} = start(players, :choose_board)

    view_a |> element("#side-olympia-b") |> render_click()
    view_a |> element("#claim-olympia") |> render_click()
    view_b |> element("#claim-alexandria") |> render_click()
    view_b |> element("#confirm-wonder") |> render_click()

    stop_game_servers()
    {:ok, again_a, _html} = open(ta, game.id)
    {:ok, again_b, _html} = open(tb, game.id)

    assert has_element?(again_a, "#setup-wonder-olympia[data-mine][data-side='b']")
    refute has_element?(again_a, "#confirm-wonder[disabled]")
    assert has_element?(again_a, "#setup-wonder-alexandria [data-confirmed]")
    assert has_element?(again_b, "#confirm-wonder[disabled]")
    assert has_element?(again_b, "#setup-waiting", a.name)
  end

  test "forged setup events are flashed and never crash the page", %{players: players} do
    {game, [view_a | _]} = start(players, :choose_board)

    render_click(view_a, "claim_wonder", %{"wonder" => "Atlantis", "side" => "a"})
    assert has_element?(view_a, "#flash-error", "That board does not exist")

    render_click(view_a, "claim_wonder", %{"wonder" => "Gizah", "side" => "c"})
    assert has_element?(view_a, "#flash-error", "That choice is not available")

    render_click(view_a, "claim_wonder", %{})
    render_click(view_a, "confirm_wonder", %{})
    assert has_element?(view_a, "#flash-error", "Pick a board first")

    render_click(view_a, "submit", %{"card" => "Altar", "kind" => "discard"})
    assert has_element?(view_a, "#flash-error", "You can't do that right now")

    render_click(view_a, "show_card", %{"card" => "Altar", "owner" => "1"})
    render_click(view_a, "select_card", %{"card" => "Altar"})
    render_click(view_a, "show_side", %{"wonder" => "Gizah", "side" => "z"})
    render_click(view_a, "show_side", %{"wonder" => "Atlantis", "side" => "b"})
    send(view_a.pid, {:game_updated, 0})

    assert has_element?(view_a, "#wonder-setup")
    assert has_element?(view_a, "#setup-wonder-gizah[data-side='a']")
    refute has_element?(view_a, "#card-sheet")
    refute has_element?(view_a, "#action-panel")
    assert action_count(game) == 0
  end
end
