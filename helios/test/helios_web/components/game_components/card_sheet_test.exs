defmodule HeliosWeb.GameComponents.CardSheetTest do
  use ExUnit.Case, async: true

  import Phoenix.LiveViewTest
  import HeliosWeb.HTMLHelpers

  alias HeliosWeb.GameComponents.CardSheet

  test "a read-only sheet: art with zoom, explanation, no actions, closable" do
    html = render_component(&CardSheet.card_sheet/1, name: "Temple", neighbours: %{}, built: [])
    assert attrs(html, "#card-sheet", "role") == ["dialog"]
    assert text(html, "#card-sheet-title") == "Temple"
    assert count(html, "#card-sheet #card-info") == 1
    assert count(html, "#card-sheet #zoom-card") == 1
    assert count(html, "#card-sheet [phx-click='submit']") == 0
    assert attrs(html, "#card-sheet-scrim", "phx-click") == ["hide_card"]
    assert attrs(html, "#close-card-sheet", "phx-click") == ["hide_card"]
    assert attrs(html, "#card-sheet", "phx-window-keydown") == ["hide_card"]
  end
end
