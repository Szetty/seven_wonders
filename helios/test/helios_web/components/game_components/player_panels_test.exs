defmodule HeliosWeb.GameComponents.PlayerPanelsTest do
  use ExUnit.Case, async: true

  import Phoenix.LiveViewTest
  import HeliosWeb.HTMLHelpers

  alias Helios.SampleViews
  alias HeliosWeb.GameComponents.PlayerPanels

  test "neighbour panel shows name, wonder art, stats and colour chips" do
    player = SampleViews.player("2", %{wonder: "Rhódos", side: :b})

    html =
      render_component(&PlayerPanels.neighbour_panel/1,
        id: "east-panel",
        label: "East",
        player: player,
        names: SampleViews.names()
      )

    assert text(html, "#east-panel header") =~ "East"
    assert text(html, "#east-panel header") =~ "Bob"

    assert attrs(html, "#east-panel img[alt='Rhódos B']", "src") == [
             "/images/wonders/rhodosB.png"
           ]

    assert text(html, "#east-panel [data-stat='coins']") == "4"
    assert text(html, "#east-panel [data-stat='shields']") == "1"
    assert text(html, "#east-panel [data-stat='stages']") == "1/3"

    assert attrs(html, "#east-panel [data-token]", "src") == [
             "/images/tokens/victory1.png",
             "/images/tokens/victoryminus1.png"
           ]

    assert attrs(html, "#east-panel [data-card]", "title") == ["Lumber Yard", "Altar", "Stockade"]
  end

  test "neighbour panel folds into a one-line summary toggle on phones" do
    html =
      render_component(&PlayerPanels.neighbour_panel/1,
        id: "east-panel",
        label: "East",
        player: SampleViews.player("2", %{wonder: "Rhódos", side: :b}),
        names: SampleViews.names()
      )

    assert attrs(html, "#east-panel-toggle", "aria-controls") == ["east-panel-details"]
    assert attrs(html, "#east-panel-toggle", "aria-expanded") == ["false"]
    assert text(html, "#east-panel-toggle") =~ "East"
    assert text(html, "#east-panel-toggle") =~ "Bob"
    assert attrs(html, "#east-panel-toggle img", "alt") |> Enum.all?(&(&1 == ""))
    assert [js] = attrs(html, "#east-panel-toggle", "phx-click")
    assert js =~ "toggle_class" and js =~ "is-open"
    assert count(html, "#east-panel-details header") == 1
    # The summary must not duplicate what the existing test counts.
    assert count(html, "#east-panel-toggle [data-stat], #east-panel-toggle [data-card]") == 0
  end

  test "others strip summarises each remaining player with colour counts" do
    html =
      render_component(&PlayerPanels.others_strip/1,
        players: [SampleViews.player("3", %{wonder: "Éphesos"})],
        names: SampleViews.names()
      )

    assert text(html, "#player-3 header") =~ "Cid"
    assert text(html, "#player-3 header") =~ "Éphesos A"

    assert attrs(html, "#player-3 [data-category]", "data-category") == [
             "raw_material",
             "civilian",
             "military"
           ]
  end
end
