defmodule Helios.AI.FullGameTest do
  use Helios.DataCase, async: false

  import Helios.GamesFixtures

  alias Helios.{GameDriver, Games}

  @moduletag timeout: 180_000

  setup do
    on_exit(&stop_game_servers/0)
    :ok
  end

  test "one human and two AI seats play a whole game" do
    human = player_fixture()
    wonders = [{"Gizah", :a}, {"Babylon", :b}, {"Halikarnassós", :b}]
    game = game_fixture([human, {:ai, "fake-first"}, {:ai, "fake-last"}], wonders: wonders)
    Phoenix.PubSub.subscribe(Helios.PubSub, Games.topic(game.id))
    {:ok, _pid} = Games.ensure_started(game.id)

    assert %{phase: %{kind: :game_over}} = drive(game.id, human.id, 600)
  end

  # Plays the human (discard, or a build from the discard pile) whenever they
  # must act; otherwise waits for the AI seats to move.
  defp drive(_game_id, _user_id, 0), do: flunk("the game did not finish")

  defp drive(game_id, user_id, steps) do
    {:ok, view} = Games.view(game_id, user_id)

    cond do
      view.phase.kind == :game_over ->
        view

      must_act?(view) ->
        :ok = Games.submit(game_id, user_id, GameDriver.choose(view, :discard))
        drive(game_id, user_id, steps - 1)

      true ->
        assert_receive {:game_updated, _}, 5_000
        drive(game_id, user_id, steps - 1)
    end
  end

  defp must_act?(%{phase: %{kind: :choosing_cards}, me: me, submitted: submitted}),
    do: List.keyfind(submitted, me, 0) == {me, false}

  defp must_act?(%{phase: %{kind: :extra_turn, extra_turn_player: me}, me: me}), do: true
  defp must_act?(_view), do: false
end
