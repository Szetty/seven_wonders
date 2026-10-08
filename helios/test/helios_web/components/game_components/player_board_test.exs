defmodule HeliosWeb.GameComponents.PlayerBoardTest do
  use ExUnit.Case, async: true

  import Phoenix.LiveViewTest
  import HeliosWeb.HTMLHelpers

  alias Helios.SampleViews
  alias HeliosWeb.GameComponents.PlayerBoard

  defp board(player, opts) do
    render_component(
      &PlayerBoard.player_board/1,
      Keyword.merge(
        [
          player: player,
          names: SampleViews.names(),
          status: :thinking,
          label: "East",
          id: "east-panel"
        ],
        opts
      )
    )
  end

  test "a neighbour's full board: wonder, stages, stats, tiles, latest" do
    html = board(SampleViews.player("2", %{wonder: "Rhódos", side: :b}), [])
    assert attrs(html, "#east-panel", "data-player") == ["2"]

    assert attrs(html, "#east-panel-details img[alt='Rhódos B']", "src") == [
             "/images/wonders/rhodosB.png"
           ]

    assert count(html, "#wonder-stages-2 [data-built]") == 3
    assert text(html, "#east-panel-details [data-stat='coins']") == "4"
    assert text(html, "#east-panel-details [data-stat='military']") == "1"

    assert attrs(html, "#built-cards-2 [data-card]", "data-card") == [
             "Lumber Yard",
             "Altar",
             "Stockade"
           ]

    assert attrs(html, "#built-2-stockade", "phx-click") == ["show_card"]
    assert attrs(html, "#built-2-stockade", "phx-value-owner") == ["2"]
    assert attrs(html, "#east-panel [data-latest]", "data-card") == ["Stockade"]
    assert attrs(html, "#status-2", "data-status") == ["thinking"]
    assert text(html, "#east-panel-details header") =~ "Bob"
  end

  test "non-own boards fold into a summary toggle below md" do
    html = board(SampleViews.player("2"), [])
    assert attrs(html, "#east-panel-toggle", "aria-controls") == ["east-panel-details"]
    assert [js] = attrs(html, "#east-panel-toggle", "phx-click")
    assert js =~ "is-open"
    assert count(html, "#east-panel-toggle [data-stat], #east-panel-toggle [data-card]") == 0
  end

  test "my board keeps its ids, has no toggle, and shows no name" do
    html = board(SampleViews.player("1"), id: "my-board", label: "You", mine: true)
    assert count(html, "#my-board-toggle") == 0
    assert count(html, "#wonder-stages") == 1

    assert attrs(html, "#my-built [data-card]", "data-card") == [
             "Lumber Yard",
             "Altar",
             "Stockade"
           ]

    assert count(html, "#built-1-altar") == 1
  end

  test "an empty city renders without a latest tile" do
    html = board(SampleViews.player("1", %{built: []}), id: "my-board", label: "You", mine: true)
    assert count(html, "#my-built [data-card]") == 0
    assert count(html, "[data-latest]") == 0
  end
end
