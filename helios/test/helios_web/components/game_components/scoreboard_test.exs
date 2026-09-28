defmodule HeliosWeb.GameComponents.ScoreboardTest do
  use ExUnit.Case, async: true

  import Phoenix.LiveViewTest
  import HeliosWeb.HTMLHelpers

  alias Helios.SampleViews
  alias HeliosWeb.GameComponents.Scoreboard

  test "ranks rows, highlights the winner and links back to the lobby" do
    html =
      render_component(&Scoreboard.scoreboard/1,
        scores: SampleViews.scores(),
        names: SampleViews.names(),
        me: "1",
        lobby_id: "abc"
      )

    assert attrs(html, "#scoreboard tbody tr", "id") == [
             "score-row-2",
             "score-row-1",
             "score-row-3"
           ]

    assert attrs(html, "#score-row-2", "data-rank") == ["1"]
    assert hd(attrs(html, "#score-row-2", "class")) =~ "bg-amber-200"
    assert text(html, "#score-row-2 td:last-child") == "1"
    assert text(html, "#score-row-2") =~ "Bob"
    assert text(html, "#score-row-2") =~ "25"
    assert count(html, "#scoreboard thead th") == 10
    assert attrs(html, "#back-to-lobby", "href") == ["/lobby/abc"]
  end
end
