defmodule HeliosWeb.GameComponents.AgeSummaryTest do
  use ExUnit.Case, async: true

  import Phoenix.LiveViewTest
  import HeliosWeb.HTMLHelpers

  alias Helios.SampleViews
  alias HeliosWeb.GameComponents.AgeSummary

  @battles [
    %{
      player: "2",
      rival: "3",
      player_strength: 0,
      rival_strength: 0,
      player_token: nil,
      rival_token: nil
    },
    %{
      player: "1",
      rival: "2",
      player_strength: 2,
      rival_strength: 1,
      player_token: 1,
      rival_token: -1
    },
    %{
      player: "4",
      rival: "1",
      player_strength: 3,
      rival_strength: 2,
      player_token: 1,
      rival_token: -1
    }
  ]

  test "my battles come first; tokens and ties are shown; it closes" do
    html =
      render_component(&AgeSummary.age_summary/1,
        event: {:battles, %{age: 1, battles: @battles}},
        ctx: %{me: "1", names: SampleViews.names(), view: SampleViews.view()}
      )

    # Rows read "<player> <strength> [token] vs [token] <strength> <rival>".
    assert text(html, "#age-summary-title") == "End of Age I — Military conflicts"
    assert text(html, "#battle-0") =~ "You 2"
    assert text(html, "#battle-0") =~ "1 Bob"
    assert text(html, "#battle-1") =~ "Dee 3"
    assert text(html, "#battle-1") =~ "2 You"
    assert text(html, "#battle-2") =~ "tie"

    assert attrs(html, "#battle-0 img[data-token-for]", "src") ==
             ["/images/tokens/victory1.png", "/images/tokens/victoryminus1.png"]

    assert [hook] = attrs(html, "#age-summary", "phx-hook")
    assert hook =~ "AgeSummary"
    assert count(html, "#close-age-summary") == 1
  end
end
