defmodule HeliosWeb.GameComponents.CardInfoTest do
  use ExUnit.Case, async: true

  import Phoenix.LiveViewTest
  import HeliosWeb.HTMLHelpers

  alias HeliosWeb.GameComponents.CardInfo

  test "explains effects, cost and chains" do
    html =
      render_component(&CardInfo.card_info/1, name: "Temple", neighbours: %{}, built: ["Altar"])

    assert text(html, "#card-info .effect-line") == "Worth 3 victory points"
    assert count(html, "#card-cost img") == 3
    assert text(html, "#card-chains-from") =~ "Altar"
    assert count(html, "#card-chains-from .hero-check") == 1
    assert text(html, "#card-chains-to") =~ "Pantheon"
  end

  test "a free card says so; unknown cards render nothing" do
    html =
      render_component(&CardInfo.card_info/1, name: "Lumber Yard", neighbours: %{}, built: [])

    assert text(html, "#card-cost") =~ "Free"
    assert count(html, "#card-chains-from") == 0

    assert render_component(&CardInfo.card_info/1, name: "Nope", neighbours: %{}, built: []) =~
             ~r/^\s*$/
  end
end
