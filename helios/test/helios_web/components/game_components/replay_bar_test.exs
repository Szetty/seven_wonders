defmodule HeliosWeb.GameComponents.ReplayBarTest do
  use ExUnit.Case, async: true

  import Phoenix.LiveViewTest
  import HeliosWeb.HTMLHelpers

  alias HeliosWeb.GameComponents.ReplayBar

  defp frame(phase, choice \\ nil),
    do: %{phase: phase, hand: ["Altar", "Loom"], my_choice: choice, players: [], event_index: 0}

  test "position, controls, choice" do
    html =
      render_component(&ReplayBar.replay_bar/1,
        frame: frame(%{kind: :choosing_cards, age: 2, turn: 3}, {:discard, "Loom"}),
        index: 3,
        last: 20,
        playing: false
      )

    assert text(html, "#replay-position") == "Age II · Turn 3"
    assert text(html, "#replay-choice") =~ "You chose: Discard Loom"
    assert attrs(html, "#replay-next", "phx-click") == ["replay_next"]
    assert attrs(html, "#replay-bar", "data-playing") == ["false"]
    assert count(html, "#replay-prev[disabled]") == 0
  end

  test "the final frame says Final and disables forward controls" do
    html =
      render_component(&ReplayBar.replay_bar/1,
        frame: frame(%{kind: :game_over, age: 3, turn: 6}),
        index: 20,
        last: 20,
        playing: false
      )

    assert text(html, "#replay-position") == "Final"
    assert count(html, "#replay-next[disabled]") == 1
    assert count(html, "#replay-choice") == 0
  end
end
