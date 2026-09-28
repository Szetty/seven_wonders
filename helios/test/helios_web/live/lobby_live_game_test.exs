defmodule HeliosWeb.LobbyLiveGameTest do
  use HeliosWeb.ConnCase, async: false

  import Phoenix.LiveViewTest
  import Helios.GamesFixtures

  alias Helios.{Games, Lobbies, Repo}
  alias Helios.Accounts.Scope
  alias Helios.Games.Game

  setup do
    on_exit(&stop_game_servers/0)
    [{owner, _} | guests] = players = for _ <- 1..3, do: user_with_token_fixture()
    lobby = Lobbies.get_or_create_own_lobby(owner)
    :ok = invite_all(owner, lobby, Enum.map(guests, &elem(&1, 0)))
    %{players: players, lobby: lobby}
  end

  defp open(token, lobby), do: live(log_in_conn(build_conn(), token), ~p"/lobby/#{lobby.id}")

  test "Start is disabled with the reason until 3 players are connected",
       %{players: [{_, ta}, {_, tb}, _], lobby: lobby} do
    {:ok, _lv_b, _html} = open(tb, lobby)
    {:ok, lv_a, _html} = open(ta, lobby)
    assert has_element?(lv_a, "#start-game[disabled]")
    assert has_element?(lv_a, "#start-blocker", "Need at least 3 connected players")
  end

  test "guests never see the Start button", %{players: [_, {_, tb}, _], lobby: lobby} do
    {:ok, lv_b, _html} = open(tb, lobby)
    refute has_element?(lv_b, "#start-game")
  end

  test "starting sends every seated, connected player to the game",
       %{players: [{_, ta}, {_, tb}, {_, tc}], lobby: lobby} do
    {:ok, lv_b, _html} = open(tb, lobby)
    {:ok, lv_c, _html} = open(tc, lobby)
    {:ok, lv_a, _html} = open(ta, lobby)
    refute has_element?(lv_a, "#start-game[disabled]")
    refute has_element?(lv_a, "#start-blocker")

    lv_a |> element("#start-game") |> render_click()

    game = Games.active_game_for_lobby(lobby.id)
    path = ~p"/game/#{game.id}"
    for lv <- [lv_a, lv_b, lv_c], do: assert_redirect(lv, path)
  end

  test "while a game runs, seated players get Rejoin and Start is disabled",
       %{players: [{owner, ta}, {b, tb}, {c, _}], lobby: lobby} do
    connect_to_lobby(lobby, [b, c])
    {:ok, game} = Games.start_game(Scope.for_user(owner), lobby)

    {:ok, lv_b, _html} = open(tb, lobby)
    assert has_element?(lv_b, "#game-in-progress")
    assert has_element?(lv_b, "#rejoin-game[href='/game/#{game.id}']")

    {:ok, lv_a, _html} = open(ta, lobby)
    assert has_element?(lv_a, "#start-game[disabled]")
    assert has_element?(lv_a, "#start-blocker", "A game is already running at this table")
  end

  test "invitees who were not seated see the banner without Rejoin",
       %{players: [{owner, _}, {b, _}, {c, _}], lobby: lobby} do
    {late, late_token} = user_with_token_fixture()
    :ok = invite_all(owner, lobby, [late])
    connect_to_lobby(lobby, [b, c])
    {:ok, _game} = Games.start_game(Scope.for_user(owner), lobby)

    {:ok, lv_late, _html} = open(late_token, lobby)
    assert has_element?(lv_late, "#game-in-progress")
    refute has_element?(lv_late, "#rejoin-game")
  end

  test "Start is enabled again once the game has finished",
       %{players: [{owner, ta}, {b, _}, {c, _}], lobby: lobby} do
    connect_to_lobby(lobby, [b, c])
    {:ok, game} = Games.start_game(Scope.for_user(owner), lobby)
    {:ok, lv_a, _html} = open(ta, lobby)
    assert has_element?(lv_a, "#start-game[disabled]")

    stop_game_servers()
    game |> Game.finish_changeset(%{"scores" => []}) |> Repo.update!()
    Phoenix.PubSub.broadcast(Helios.PubSub, "lobby:#{lobby.id}", {:members_changed})

    refute has_element?(lv_a, "#start-game[disabled]")
    refute has_element?(lv_a, "#game-in-progress")
  end
end
