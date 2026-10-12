defmodule HeliosWeb.GameComponents.ExtraTurnTest do
  use ExUnit.Case, async: true

  import Phoenix.LiveViewTest
  import HeliosWeb.HTMLHelpers

  alias Helios.SampleViews
  alias HeliosWeb.GameComponents.ExtraTurn

  defp extra(player, kind, overrides \\ %{}) do
    base = SampleViews.view()

    Map.merge(
      %{
        base
        | phase: %{
            base.phase
            | kind: :extra_turn,
              extra_turn_player: player,
              extra_turn_kind: kind
          }
      },
      overrides
    )
  end

  defp render_notice(view),
    do: render_component(&ExtraTurn.extra_turn_notice/1, view: view, names: SampleViews.names())

  defp render_picker(view),
    do: render_component(&ExtraTurn.discard_picker/1, view: view, minimised: false)

  test "my build-from-discard turn shows a picker over the discard pile and no notice" do
    view = extra("1", :build_from_discard, %{discard_pile: ["Altar", "Loom"], hand: []})
    html = render_picker(view)
    assert attrs(html, "#discard-picker [data-card]", "data-card") == ["Altar", "Loom"]
    assert attrs(html, "#discard-pick-1", "phx-value-kind") == ["build_from_discard"]
    assert attrs(html, "#discard-pick-1", "phx-value-card") == ["Loom"]
    assert count(render_notice(view), "#waiting-extra-turn, #play-last-card") == 0
  end

  test "the discard picker is a dialog that cannot be dismissed" do
    html = render_picker(extra("1", :build_from_discard, %{discard_pile: ["Altar"], hand: []}))
    assert attrs(html, "#discard-picker", "role") == ["dialog"]
    assert attrs(html, "#discard-picker", "aria-labelledby") == ["discard-picker-title"]
    assert count(html, "#discard-scrim") == 1
    assert count(html, "#discard-scrim[phx-click]") == 0
    assert count(html, "#discard-picker[phx-window-keydown]") == 0
    assert count(html, "#close-action-panel") == 0
  end

  test "my play-last-card turn shows the notice and no picker" do
    view = extra("1", :play_last_card)
    assert text(render_notice(view), "#play-last-card") =~ "Play your last card"
    assert count(render_picker(view), "#discard-picker") == 0
  end

  test "other players wait for the extra-turn player" do
    view = extra("2", :build_from_discard)
    assert text(render_notice(view), "#waiting-extra-turn") =~ "Waiting for Bob"
    assert count(render_picker(view), "#discard-picker") == 0
  end

  test "the picker can be minimised to a bar that restores it" do
    view = extra("1", :build_from_discard, %{discard_pile: ["Altar"], hand: []})
    open = render_component(&ExtraTurn.discard_picker/1, view: view, minimised: false)
    assert attrs(open, "#discard-minimise", "phx-click") == ["minimise_discard"]

    closed = render_component(&ExtraTurn.discard_picker/1, view: view, minimised: true)
    assert count(closed, "#discard-picker") == 0
    assert count(closed, "#discard-scrim") == 0

    bar = render_component(&ExtraTurn.discard_restore/1, view: view, minimised: true)
    assert attrs(bar, "#discard-restore", "phx-click") == ["restore_discard"]

    assert render_component(&ExtraTurn.discard_restore/1, view: view, minimised: false) =~
             ~r/^\s*$/
  end

  test "renders nothing outside extra turns" do
    view = SampleViews.view()
    assert count(render_notice(view), "#waiting-extra-turn, #play-last-card") == 0
    assert count(render_picker(view), "#discard-picker") == 0
  end

  test "the discard picker snapshots its card rects for the fly animation" do
    view = extra("1", :build_from_discard, %{discard_pile: ["Altar", "Lumber Yard"], hand: []})
    html = render_picker(view)
    assert [hook] = attrs(html, "#discard-picker", "phx-hook")
    assert hook =~ "DiscardRects"

    assert attrs(html, "#discard-picker [data-fx-id]", "data-fx-id") ==
             ["discard-card-altar", "discard-card-lumberyard"]

    assert attrs(html, "#discard-pick-1", "data-fx-id") == ["discard-card-lumberyard"]
  end

  test "replay_discard shows the options read-only with the chosen card marked" do
    html =
      render_component(&ExtraTurn.replay_discard/1,
        cards: ["Altar", "Lumber Yard"],
        chosen: "Lumber Yard"
      )

    assert attrs(html, "#replay-discard [data-card]", "data-card") == ["Altar", "Lumber Yard"]
    assert attrs(html, "#replay-discard-lumberyard", "data-chosen") == ["true"]
    assert attrs(html, "#replay-discard-altar", "data-chosen") == ["false"]
    assert text(html, "#replay-discard-lumberyard") == "Chosen"
    assert attrs(html, "#replay-discard-altar", "data-fx-id") == ["discard-card-altar"]
    assert [hook] = attrs(html, "#replay-discard", "phx-hook")
    assert hook =~ "DiscardRects"
    assert count(html, "#replay-discard button, #replay-discard [phx-click]") == 0
  end
end
