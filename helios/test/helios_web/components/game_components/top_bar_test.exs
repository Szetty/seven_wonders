defmodule HeliosWeb.GameComponents.TopBarTest do
  use ExUnit.Case, async: true

  import Phoenix.LiveViewTest
  import HeliosWeb.HTMLHelpers

  alias Helios.SampleViews
  alias HeliosWeb.GameComponents.TopBar

  defp render_bar(view, opts \\ []) do
    render_component(&TopBar.top_bar/1,
      view: view,
      names: Keyword.get(opts, :names, SampleViews.names()),
      connected: Keyword.get(opts, :connected, MapSet.new(["1", "2"])),
      replay: Keyword.get(opts, :replay, false)
    )
  end

  test "shows age, turn, direction, who we wait for and connection dots" do
    html = render_bar(SampleViews.view())
    assert text(html, "#age-label") == "Age II"
    assert text(html, "#turn-label") == "Turn 3/6"
    assert text(html, "#pass-direction") == "Pass to Bob"
    assert text(html, "#waiting-for") == "Waiting for: Bob, Dee"
    assert text(html, "#waiting-count") == "Waiting for 2"
    assert text(html, "#waiting-list ul") == "BobDee"
    assert attrs(html, "#top-bar", "data-turn-key") == ["2-3"]
    assert attrs(html, "#top-bar", "data-phase") == ["choosing_cards"]
    assert attrs(html, "#connection-2", "data-connected") == ["true"]
    assert attrs(html, "#connection-3", "data-connected") == ["false"]
    assert text(html, "#connection-4") == "Dee"
  end

  test "game over replaces the turn information" do
    view = SampleViews.view()
    html = render_bar(%{view | phase: %{view.phase | kind: :game_over}, discard_count: 0})
    assert text(html, "#turn-label") == "Game over"
    assert count(html, "#pass-direction") == 0
    assert count(html, "#pass-row") == 0
    assert count(html, "#waiting-list") == 0
    assert count(html, "#waiting-for") == 0
    assert count(html, "#waiting-count") == 0
  end

  test "connected players who have chosen get a check mark" do
    html = render_bar(SampleViews.view())
    assert attrs(html, "#connection-1", "data-status") == ["chosen"]
    assert attrs(html, "#connection-2", "data-status") == ["thinking"]
    assert count(html, "#connection-1 .hero-check") == 1
    assert count(html, "#connection-2 .hero-check") == 0
  end

  test "passing west names the west neighbour" do
    view = SampleViews.view()
    html = render_bar(%{view | phase: %{view.phase | direction: :west}})
    assert text(html, "#pass-direction") == "Pass to Dee"
  end

  test "the history button toggles the drawer" do
    html = render_bar(SampleViews.view())
    assert [js] = attrs(html, "#history-button", "phx-click")
    assert js =~ "#history-drawer"
  end

  test "row one stays single-line so the phone bar fits 120px" do
    html = render_bar(SampleViews.view())
    assert hd(attrs(html, "#age-label", "class")) =~ "whitespace-nowrap"
    assert hd(attrs(html, "#turn-label", "class")) =~ "whitespace-nowrap"
    assert hd(attrs(html, "#waiting-count", "class")) =~ "whitespace-nowrap"
    assert html =~ "flex min-w-0 items-center gap-3 sm:gap-4"
    # The pass direction moved out of row one into its own row.
    assert count(html, "#age-label ~ #pass-direction") == 0
    assert count(html, "#pass-row > #pass-direction") == 1
  end

  test "the phone pass row shows the full name, wrapping instead of truncating" do
    long = "longplayername0abcdefghij"
    names = Map.put(SampleViews.names(), "2", long)
    html = render_bar(SampleViews.view(), names: names)

    assert text(html, "#pass-row #pass-direction") == "Pass to #{long}"
    assert count(html, "#pass-direction .sr-only") == 0
    assert [name_class] = attrs(html, "#pass-direction > span.break-words", "class")
    assert name_class =~ "break-words"
    refute name_class =~ ~r/(^|\s)(sm:)?truncate(\s|$)/
    refute name_class =~ "max-w-24"
  end

  test "the waiting button opens a hidden list of who we wait for" do
    html = render_bar(SampleViews.view())

    assert count(html, "button#waiting-count") == 1
    assert attrs(html, "#waiting-count", "type") == ["button"]
    assert attrs(html, "#waiting-count", "aria-haspopup") == ["dialog"]
    assert attrs(html, "#waiting-count", "aria-controls") == ["waiting-list"]
    assert hd(attrs(html, "#waiting-count", "class")) =~ "pointer-coarse:min-h-11"
    assert [js] = attrs(html, "#waiting-count", "phx-click")
    assert js =~ "#waiting-list"

    assert attrs(html, "#waiting-list", "role") == ["dialog"]
    assert hd(attrs(html, "#waiting-list", "class")) =~ "hidden"
    assert count(html, "#waiting-list li") == 2
    assert text(html, "#waiting-list li:first-child") == "Bob"

    # Escape and a tap outside close it; the toggle and the list share one wrapper.
    assert [wrapper_class] = attrs(html, "#waiting-menu", "class")
    assert wrapper_class =~ "sm:hidden"
    assert [away] = attrs(html, "#waiting-menu", "phx-click-away")
    assert away =~ "#waiting-list"
    assert attrs(html, "#waiting-menu", "phx-key") == ["Escape"]
  end

  test "a replay frame never shows who we wait for, even on an extra turn" do
    view = SampleViews.view()

    extra = %{
      view
      | phase: %{
          view.phase
          | kind: :extra_turn,
            extra_turn_player: "2",
            extra_turn_kind: :build_from_discard
        }
    }

    live = render_bar(extra)
    assert attrs(live, "#top-bar", "data-replay") == ["false"]
    assert text(live, "#waiting-count") == "Waiting for 1"
    assert text(live, "#waiting-list ul") == "Bob"

    replay = render_bar(extra, replay: true)
    assert attrs(replay, "#top-bar", "data-replay") == ["true"]
    assert count(replay, "#waiting-count") == 0
    assert count(replay, "#waiting-list") == 0
    assert count(replay, "#waiting-for") == 0
    assert text(replay, "#pass-direction") == "Pass to Bob"
  end

  test "the discard pile marker shows the count in the pass row and hides at zero" do
    html = render_bar(SampleViews.view())
    assert count(html, "#discard-pile") == 1
    assert count(html, "#pass-row > #discard-pile") == 1
    assert attrs(html, "#discard-pile", "data-count") == ["2"]
    assert text(html, "#discard-pile") == "Discard pile: 2"
    assert text(html, "#pass-row #pass-direction") == "Pass to Bob"

    empty = render_bar(%{SampleViews.view() | discard_count: 0})
    assert count(empty, "#discard-pile") == 0
    assert count(empty, "#pass-row > #pass-direction") == 1
  end

  test "on game over the pass row holds only the discard pile" do
    view = SampleViews.view()
    html = render_bar(%{view | phase: %{view.phase | kind: :game_over}, discard_count: 21})
    assert count(html, "#pass-row > #discard-pile") == 1
    assert count(html, "#pass-direction") == 0
    assert text(html, "#discard-pile") == "Discard pile: 21"
  end

  test "discard_pile/1 renders nothing for an empty pile" do
    assert render_component(&TopBar.discard_pile/1, count: 0) |> String.trim() == ""
    assert count(render_component(&TopBar.discard_pile/1, count: 5), "#discard-pile") == 1
  end
end
