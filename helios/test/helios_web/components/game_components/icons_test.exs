defmodule HeliosWeb.GameComponents.IconsTest do
  use ExUnit.Case, async: true

  import Phoenix.LiveViewTest
  import HeliosWeb.HTMLHelpers

  alias HeliosWeb.GameComponents.Icons

  test "the military icon is a decorative inline svg" do
    html = render_component(&Icons.military_icon/1, class: "size-4")
    assert attrs(html, "svg[data-icon='military']", "aria-hidden") == ["true"]
    assert attrs(html, "svg[data-icon='military']", "class") == ["size-4"]
  end

  test "the military icon is the card symbol: round red shield, key border, crossed swords" do
    html = render_component(&Icons.military_icon/1, class: "size-3.5")

    assert count(html, "svg[data-icon='military'] circle") == 2
    assert attrs(html, "circle[data-part='rim']", "fill") == ["#b91c1c"]
    assert attrs(html, "circle[data-part='rim']", "stroke") == ["#1c1917"]
    assert attrs(html, "circle[data-part='border']", "fill") == ["none"]
    assert [dashes] = attrs(html, "circle[data-part='border']", "stroke-dasharray")
    assert dashes != ""

    assert count(html, "g[data-part='sword']") == 2
    assert count(html, "g[data-part='sword'] path[data-part='blade']") == 2
    assert count(html, "g[data-part='sword'] path[data-part='hilt']") == 2
    assert attrs(html, "path[data-part='blade']", "stroke") == ["#e7e5e4", "#e7e5e4"]
  end

  test "no stroke of the military icon is thinner than 1px at size-3.5" do
    html = render_component(&Icons.military_icon/1, class: "size-3.5")
    widths = attrs(html, "svg [stroke-width]", "stroke-width")

    assert widths != []
    # 24-unit viewBox drawn at 14px: 1.7 units ≈ 1px.
    assert Enum.all?(widths, fn width -> String.to_float(width) >= 1.7 end)
  end
end
