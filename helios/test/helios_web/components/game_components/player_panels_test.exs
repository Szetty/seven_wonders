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
    assert attrs(html, "#east-panel img[alt='Rhódos B']", "src") == ["/images/wonders/rhodosB.png"]
    assert text(html, "#east-panel [data-stat='coins']") == "4"
    assert text(html, "#east-panel [data-stat='shields']") == "1"
    assert text(html, "#east-panel [data-stat='stages']") == "1/3"
    assert attrs(html, "#east-panel [data-token]", "src") == ["/images/tokens/victory1.png", "/images/tokens/victoryminus1.png"]
    assert attrs(html, "#east-panel [data-card]", "title") == ["Lumber Yard", "Altar", "Stockade"]
  end

  test "others strip summarises each remaining player with colour counts" do
    html =
      render_component(&PlayerPanels.others_strip/1,
        players: [SampleViews.player("3", %{wonder: "Éphesos"})],
        names: SampleViews.names()
      )

    assert text(html, "#player-3 header") =~ "Cid"
    assert text(html, "#player-3 header") =~ "Éphesos A"
    assert attrs(html, "#player-3 [data-category]", "data-category") == ["raw_material", "civilian", "military"]
  end
end
