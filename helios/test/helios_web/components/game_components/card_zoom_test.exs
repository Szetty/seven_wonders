defmodule HeliosWeb.GameComponents.CardZoomTest do
  use ExUnit.Case, async: true

  import Phoenix.LiveViewTest
  import HeliosWeb.HTMLHelpers

  alias HeliosWeb.GameComponents.CardZoom

  test "the zoom starts hidden and closes on click, Escape and the close button" do
    html = render_component(&CardZoom.card_zoom/1, name: "Altar")
    assert [class] = attrs(html, "#card-zoom", "class")
    assert class =~ "hidden"
    assert attrs(html, "#card-zoom", "role") == ["dialog"]
    assert attrs(html, "#card-zoom", "phx-key") == ["Escape"]
    assert [click] = attrs(html, "#card-zoom", "phx-click")
    assert click =~ "hide"
    assert attrs(html, "#close-card-zoom", "aria-label") == ["Close"]
    assert attrs(html, "#card-zoom img", "alt") == ["Altar"]
  end
end
