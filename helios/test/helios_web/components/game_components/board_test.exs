defmodule HeliosWeb.GameComponents.BoardTest do
  use ExUnit.Case, async: true

  import Phoenix.LiveViewTest
  import HeliosWeb.HTMLHelpers

  alias Helios.SampleViews
  alias HeliosWeb.GameComponents.Board

  test "renders the wonder with stage slots and built cards in colour columns" do
    html = render_component(&Board.my_board/1, player: SampleViews.player("1"))

    assert attrs(html, "#my-board img[alt='Gizah A']", "src") == ["/images/wonders/gizahA.png"]
    assert count(html, "#wonder-stages [data-built='true']") == 1
    assert count(html, "#wonder-stages [data-built='false']") == 2
    assert attrs(html, "#stage-1 img", "src") == ["/images/cards/age1.png"]
    assert count(html, "#stage-2 img") == 0

    assert attrs(html, "#my-built [data-category]", "data-category") == [
             "raw_material",
             "civilian",
             "military"
           ]

    assert attrs(html, "#built-lumberyard img", "src") == ["/images/cards/lumberyard.png"]
    assert text(html, "#my-board [data-stat='coins']") == "4"
  end
end
