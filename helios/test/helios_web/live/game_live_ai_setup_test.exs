defmodule HeliosWeb.GameLiveAISetupTest do
  use HeliosWeb.ConnCase, async: false

  import Phoenix.LiveViewTest
  import Helios.GamesFixtures

  alias Helios.Games

  setup do
    on_exit(&stop_game_servers/0)
    put_ai_config(players: false)
    [{owner, ot}, {guest, gt}] = for _ <- 1..2, do: user_with_token_fixture()
    game = game_fixture([owner, guest, {:ai, "fake-first"}], selection: :choose_board)
    %{owner: owner, ot: ot, guest: guest, gt: gt, game: game}
  end

  defp open(token, game), do: live(log_in_conn(build_conn(), token), ~p"/game/#{game.id}")

  test "the owner switches to the AI seat, claims and confirms for it",
       %{owner: owner, ot: ot, gt: gt, game: game} do
    {:ok, lv, _html} = open(ot, game)
    {:ok, guest_lv, _html} = open(gt, game)

    assert has_element?(lv, "#setup-as-#{owner.id}[aria-pressed='true']", "You")
    assert has_element?(lv, "#setup-as-ai-2", "Fake First")
    refute has_element?(guest_lv, "#setup-as")

    lv |> element("#setup-as-ai-2") |> render_click()
    lv |> element("#claim-gizah") |> render_click()
    assert has_element?(lv, "#setup-wonder-gizah[data-holder='ai-2'][data-mine]")
    assert has_element?(lv, "#setup-wonder-gizah [data-holder-name]", "Fake First")
    lv |> element("#confirm-wonder") |> render_click()

    lv |> element("#setup-as-#{owner.id}") |> render_click()
    refute has_element?(lv, "#setup-wonder-gizah[data-mine]")
    lv |> element("#claim-rhodos") |> render_click()

    assert has_element?(
             lv,
             "#setup-wonder-rhodos[data-holder='#{owner.id}'] [data-holder-name]",
             "You"
           )

    {:ok, %{setup: setup}} = Games.view(game.id, owner.id)
    assert %{wonder: "Gizah", confirmed: true} = Enum.find(setup.seats, &(&1.player == "ai-2"))

    assert %{wonder: "Rhódos", confirmed: false} =
             Enum.find(setup.seats, &(&1.player == to_string(owner.id)))
  end

  test "a guest cannot act for the AI seat, even with a forged event", %{gt: gt, game: game} do
    {:ok, guest_lv, _html} = open(gt, game)
    render_hook(guest_lv, "setup_as", %{"seat" => "ai-2"})
    guest_lv |> element("#claim-gizah") |> render_click()

    {:ok, %{setup: setup}} = Games.view(game.id, {:ai, 2})
    assert %{wonder: nil} = Enum.find(setup.seats, &(&1.player == "ai-2"))
  end
end
