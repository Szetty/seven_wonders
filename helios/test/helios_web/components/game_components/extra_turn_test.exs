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

  defp render_extra(view),
    do: render_component(&ExtraTurn.extra_turn/1, view: view, names: SampleViews.names())

  test "my build-from-discard turn shows a picker over the discard pile" do
    html =
      render_extra(extra("1", :build_from_discard, %{discard_pile: ["Altar", "Loom"], hand: []}))

    assert attrs(html, "#discard-picker [data-card]", "data-card") == ["Altar", "Loom"]
    assert attrs(html, "#discard-pick-1", "phx-value-kind") == ["build_from_discard"]
    assert attrs(html, "#discard-pick-1", "phx-value-card") == ["Loom"]
  end

  test "my play-last-card turn shows the notice" do
    html = render_extra(extra("1", :play_last_card))
    assert text(html, "#play-last-card") =~ "Play your last card"
  end

  test "other players wait for the extra-turn player" do
    html = render_extra(extra("2", :build_from_discard))
    assert text(html, "#waiting-extra-turn") =~ "Waiting for Bob"
    assert count(html, "#discard-picker") == 0
  end

  test "renders nothing outside extra turns" do
    html = render_extra(SampleViews.view())
    assert count(html, "#waiting-extra-turn, #play-last-card, #discard-picker") == 0
  end
end
