defmodule HeliosWeb.GameComponents.TopBarTest do
  use ExUnit.Case, async: true

  import Phoenix.LiveViewTest
  import HeliosWeb.HTMLHelpers

  alias Helios.SampleViews
  alias HeliosWeb.GameComponents.TopBar

  defp render_bar(view, connected \\ MapSet.new(["1", "2"])) do
    render_component(&TopBar.top_bar/1, view: view, names: SampleViews.names(), connected: connected)
  end

  test "shows age, turn, direction, who we wait for and connection dots" do
    html = render_bar(SampleViews.view())
    assert text(html, "#age-label") == "Age II"
    assert text(html, "#turn-label") == "Turn 3/6"
    assert text(html, "#pass-direction") == "Pass east"
    assert text(html, "#waiting-for") == "Waiting for: Bob, Dee"
    assert attrs(html, "#top-bar", "data-turn-key") == ["2-3"]
    assert attrs(html, "#top-bar", "data-phase") == ["choosing_cards"]
    assert attrs(html, "#connection-2", "data-connected") == ["true"]
    assert attrs(html, "#connection-3", "data-connected") == ["false"]
    assert text(html, "#connection-4") == "Dee"
  end

  test "game over replaces the turn information" do
    view = SampleViews.view()
    html = render_bar(%{view | phase: %{view.phase | kind: :game_over}})
    assert text(html, "#turn-label") == "Game over"
    assert count(html, "#pass-direction") == 0
    assert count(html, "#waiting-for") == 0
  end
end
