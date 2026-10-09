defmodule HeliosWeb.HistoryTextTest do
  use ExUnit.Case, async: true

  alias Helios.SampleViews
  alias HeliosWeb.HistoryText

  # Seats 1..4: 1's west is 4, east is 2.
  defp ctx, do: %{me: "1", names: SampleViews.names(), view: SampleViews.view()}

  test "plays name the actor, the action and coins paid to neighbours" do
    assert HistoryText.play(
             %{player: "1", action: {:built, "Altar"}, coins_to_west: 0, coins_to_east: 0},
             ctx()
           ) ==
             "You built Altar"

    assert HistoryText.play(
             %{player: "2", action: {:built, "Stockade"}, coins_to_west: 2, coins_to_east: 1},
             ctx()
           ) ==
             "Bob built Stockade, paid 2 coins to you and 1 coin to Cid"

    assert HistoryText.play(
             %{player: "3", action: {:wonder_stage, 2}, coins_to_west: 0, coins_to_east: 0},
             ctx()
           ) ==
             "Cid built wonder stage 2"

    assert HistoryText.play(
             %{player: "4", action: :discarded, coins_to_west: 0, coins_to_east: 0},
             ctx()
           ) ==
             "Dee discarded a card"

    assert HistoryText.play(
             %{player: "1", action: {:built_free, "Palace"}, coins_to_west: 0, coins_to_east: 0},
             ctx()
           ) ==
             "You built Palace for free (Olympía)"

    assert HistoryText.play(
             %{
               player: "1",
               action: {:built_from_discard, "Gardens"},
               coins_to_west: 0,
               coins_to_east: 0
             },
             ctx()
           ) ==
             "You built Gardens from the discard pile"
  end

  test "coin changes, battles and turn titles" do
    assert HistoryText.coins([%{player: "2", delta: 5}, %{player: "1", delta: -2}], ctx()) ==
             "Bob +5 · You −2"

    assert HistoryText.battle(
             %{
               player: "1",
               rival: "2",
               player_strength: 2,
               rival_strength: 1,
               player_token: 1,
               rival_token: -1
             },
             ctx()
           ) == "You 2 vs Bob 1: You +1, Bob −1"

    assert HistoryText.battle(
             %{
               player: "2",
               rival: "3",
               player_strength: 0,
               rival_strength: 0,
               player_token: nil,
               rival_token: nil
             },
             ctx()
           ) == "Bob 0 vs Cid 0: tie"

    assert HistoryText.turn_title(%{turn: 3, kind: :regular}) == "Turn 3"
    assert HistoryText.turn_title(%{turn: 6, kind: :play_last_card}) == "Turn 6 · last card"

    assert HistoryText.turn_title(%{turn: 6, kind: :build_from_discard}) ==
             "Turn 6 · from the discard pile"
  end
end
