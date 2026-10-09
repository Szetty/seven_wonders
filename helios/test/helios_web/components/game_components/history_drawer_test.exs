defmodule HeliosWeb.GameComponents.HistoryDrawerTest do
  use ExUnit.Case, async: true

  import Phoenix.LiveViewTest
  import HeliosWeb.HTMLHelpers

  alias Helios.SampleViews
  alias HeliosWeb.GameComponents.HistoryDrawer

  @history [
    {:turn,
     %{
       age: 1,
       turn: 1,
       kind: :regular,
       plays: [%{player: "1", action: {:built, "Altar"}, coins_to_west: 0, coins_to_east: 0}],
       coins: [%{player: "2", delta: 3}]
     }},
    {:battles,
     %{
       age: 1,
       battles: [
         %{
           player: "1",
           rival: "2",
           player_strength: 1,
           rival_strength: 0,
           player_token: 1,
           rival_token: -1
         }
       ]
     }},
    {:turn,
     %{
       age: 2,
       turn: 1,
       kind: :regular,
       plays: [%{player: "2", action: :discarded, coins_to_west: 0, coins_to_east: 0}],
       coins: []
     }}
  ]

  defp ctx, do: %{me: "1", names: SampleViews.names(), view: SampleViews.view()}

  test "newest first, grouped by age, battles as a block, cards open the card sheet" do
    html = render_component(&HistoryDrawer.history_drawer/1, history: @history, ctx: ctx())
    assert attrs(html, "#history-drawer [data-age]", "data-age") == ["2", "1"]

    assert attrs(html, "#history-drawer [data-history-index]", "data-history-index") == [
             "2",
             "1",
             "0"
           ]

    assert text(html, "#history-1") =~ "You 1 vs Bob 0: You +1, Bob −1"
    assert text(html, "#history-0") =~ "You built Altar"
    assert text(html, "#history-0") =~ "Bob +3"
    assert attrs(html, "#history-0 [phx-click='show_card']", "phx-value-card") == ["Altar"]
    assert attrs(html, "#history-0 [phx-click='show_card']", "phx-value-owner") == ["1"]
    assert [class] = attrs(html, "#history-drawer", "class")
    assert class =~ "hidden"
  end

  test "an empty history says so" do
    html = render_component(&HistoryDrawer.history_drawer/1, history: [], ctx: ctx())
    assert count(html, "#history-empty") == 1
  end
end
