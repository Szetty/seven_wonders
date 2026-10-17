defmodule HeliosWeb.LobbyLiveAITest do
  use HeliosWeb.ConnCase, async: false

  import Phoenix.LiveViewTest
  import Helios.GamesFixtures

  alias Helios.{Games, Lobbies, Repo}
  alias Helios.Lobbies.AISeat

  setup do
    on_exit(&stop_game_servers/0)
    put_ai_config(players: false)
    [{owner, _} = o, {guest, _} = g] = for _ <- 1..2, do: user_with_token_fixture()
    lobby = Lobbies.get_or_create_own_lobby(owner)
    :ok = invite_all(owner, lobby, [guest])
    %{owner: o, guest: g, lobby: lobby}
  end

  defp open(token, lobby), do: live(log_in_conn(build_conn(), token), ~p"/lobby/#{lobby.id}")

  test "the owner picks from ready models only", %{owner: {_, token}, lobby: lobby} do
    {:ok, lv, _html} = open(token, lobby)
    assert has_element?(lv, "#ai-seat-form option[value='fake-first']", "Fake First")
    refute has_element?(lv, "#ai-seat-form option[value='fake-down']")
  end

  test "adding and removing AI players; they take free slots", %{owner: {_, token}, lobby: lobby} do
    {:ok, lv, _html} = open(token, lobby)

    free = fn ->
      lv
      |> render()
      |> LazyHTML.from_fragment()
      |> LazyHTML.query("[data-free-slot]")
      |> Enum.count()
    end

    before = free.()

    lv |> form("#ai-seat-form", ai: %{model_id: "fake-first"}) |> render_submit()
    lv |> form("#ai-seat-form", ai: %{model_id: "fake-first"}) |> render_submit()

    [first, second] = Lobbies.ai_seats(lobby)

    assert has_element?(
             lv,
             "#ai-seat-#{first.id}[data-ai-seat][data-model='fake-first']",
             "Fake First"
           )

    assert has_element?(lv, "#ai-seat-#{second.id}", "Fake First #2")
    assert free.() == before - 2

    lv |> element("#remove-ai-seat-#{first.id}") |> render_click()
    refute has_element?(lv, "#ai-seat-#{first.id}")
    assert has_element?(lv, "#ai-seat-#{second.id}", "Fake First")
  end

  test "guests see AI players but cannot change them", %{
    owner: {owner, _},
    guest: {_, token},
    lobby: lobby
  } do
    {:ok, seat} = Lobbies.add_ai_seat(Helios.Accounts.Scope.for_user(owner), lobby, "fake-first")
    {:ok, lv, _html} = open(token, lobby)

    assert has_element?(lv, "#ai-seat-#{seat.id}", "Fake First")
    refute has_element?(lv, "#ai-seat-form")
    refute has_element?(lv, "#remove-ai-seat-#{seat.id}")

    render_hook(lv, "add_ai_seat", %{"ai" => %{"model_id" => "fake-first"}})
    render_hook(lv, "remove_ai_seat", %{"id" => to_string(seat.id)})
    assert render(lv) =~ "Only the table leader can do that"
    assert length(Lobbies.ai_seats(lobby)) == 1
  end

  test "a seated model that is no longer ready is flagged", %{owner: {_, token}, lobby: lobby} do
    seat = Repo.insert!(%AISeat{lobby_id: lobby.id, model_id: "fake-down"})
    {:ok, lv, _html} = open(token, lobby)
    assert has_element?(lv, "#ai-seat-#{seat.id}[data-ready='false'] [data-unavailable]")
  end

  test "the owner alone plus two AI players can start", %{owner: {owner, token}, lobby: lobby} do
    {:ok, lv, _html} = open(token, lobby)
    assert has_element?(lv, "#start-game[disabled]")

    for _ <- 1..2,
        do: lv |> form("#ai-seat-form", ai: %{model_id: "fake-first"}) |> render_submit()

    refute has_element?(lv, "#start-game[disabled]")

    lv |> element("#start-game") |> render_click()
    game = Games.active_game_for_lobby(lobby.id)
    assert_redirect(lv, ~p"/game/#{game.id}")
    assert Enum.map(Games.players(game), & &1.user_id) == [owner.id, nil, nil]
  end
end
