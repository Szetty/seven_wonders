defmodule HeliosWeb.GameComponents.HandTest do
  use ExUnit.Case, async: true

  import Phoenix.LiveViewTest
  import HeliosWeb.HTMLHelpers

  alias Helios.SampleViews
  alias HeliosWeb.GameComponents.Hand

  defp card(name), do: Enum.find(SampleViews.hand(), &(&1.name == name))

  test "hand cards expose name and buildability and highlight selection and pending choice" do
    html =
      render_component(&Hand.hand/1,
        hand: SampleViews.hand(),
        selected: "Tavern",
        pending: {:discard, "Baths"}
      )

    assert attrs(html, "#hand [data-card]", "data-card") == ["Baths", "Tavern", "Tree Farm"]
    assert attrs(html, "#hand [data-card]", "data-buildable") == ["false", "true", "true"]
    assert attrs(html, "#hand-card-1", "aria-pressed") == ["true"]
    assert text(html, "#hand-card-0") =~ "Chosen"
    assert attrs(html, "#hand-card-2", "phx-value-card") == ["Tree Farm"]
    assert attrs(html, "#hand-card-0 img", "src") == ["/images/cards/baths.png"]
  end

  test "each hand card shows the cheapest way to build it" do
    html = render_component(&Hand.hand/1, hand: SampleViews.hand(), selected: nil, pending: nil)
    assert attrs(html, "#hand-card-0 .cost-badge", "data-cost") == ["unavailable"]
    assert attrs(html, "#hand-card-0 .cost-badge", "title") == ["You can't afford that"]
    assert attrs(html, "#hand-card-1 .cost-badge", "data-cost") == ["free"]
    assert attrs(html, "#hand-card-1 .cost-badge img", "src") == ["/images/tokens/check.png"]
    assert attrs(html, "#hand-card-2 .cost-badge", "data-cost") == ["1"]
    assert text(html, "#hand-card-2 .cost-badge") == "1"
    assert [class] = attrs(html, "#hand-card-0 img[alt='Baths']", "class")
    assert class =~ "grayscale"
  end

  test "unavailable builds show the reason; trade options list who gets paid" do
    html =
      render_component(&Hand.action_panel/1,
        card: card("Baths"),
        neighbours: %{west: "Dee", east: "Bob"}
      )

    assert text(html, "#build-unavailable") == "You can't afford that"
    assert attrs(html, "#build-unavailable", "disabled") == [""]
    assert text(html, "#wonder-option-0") == "Dee 2"
    assert text(html, "#wonder-option-1") == "Bob 2"
    assert count(html, "#wonder-card-cost") == 0
    assert attrs(html, "#wonder-option-1", "phx-value-option") == ["1"]
    assert attrs(html, "#wonder-option-1", "phx-value-kind") == ["wonder_stage"]
    assert count(html, "#build-free-button") == 0
    assert attrs(html, "#discard-button", "phx-value-kind") == ["discard"]
  end

  test "a coin-cost card shows its cost once above the trade options" do
    card = %{
      name: "Tree Farm",
      category: :raw_material,
      age: 1,
      build:
        {:trade,
         [
           %{
             payment: %{west: [{:wood, 1}], east: []},
             west_coins: 2,
             east_coins: 0,
             bank_coins: 1
           }
         ]},
      wonder_stage: {:unavailable, :cannot_afford},
      free_build: false
    }

    html =
      render_component(&Hand.action_panel/1, card: card, neighbours: %{west: "Dee", east: "Bob"})

    assert text(html, "#build-card-cost") == "+1 coin card cost"
    assert text(html, "#build-option-0") == "Dee 2"
  end

  test "the card art opens a zoom" do
    html =
      render_component(&Hand.action_panel/1,
        card: card("Tavern"),
        neighbours: %{west: "Dee", east: "Bob"}
      )

    assert [js] = attrs(html, "#zoom-card", "phx-click")
    assert js =~ "#card-zoom"
    assert attrs(html, "#card-zoom img", "src") == ["/images/cards/tavern.png"]
  end

  test "free, coin and Olympía free-build options" do
    tavern =
      render_component(&Hand.action_panel/1,
        card: card("Tavern"),
        neighbours: %{west: "Dee", east: "Bob"}
      )

    assert text(tavern, "#build-option-0") == "Free"
    assert text(tavern, "#wonder-unavailable") == "Your wonder is complete"
    assert attrs(tavern, "#build-free-button", "phx-value-kind") == ["build_free"]

    farm =
      render_component(&Hand.action_panel/1,
        card: card("Tree Farm"),
        neighbours: %{west: "Dee", east: "Bob"}
      )

    assert text(farm, "#build-option-0") == "Pay 1 coin"
  end

  test "the action panel is a dismissable dialog" do
    html =
      render_component(&Hand.action_panel/1,
        card: card("Tavern"),
        neighbours: %{west: "Dee", east: "Bob"}
      )

    assert attrs(html, "#action-panel", "role") == ["dialog"]
    assert attrs(html, "#action-panel", "aria-modal") == ["true"]
    assert attrs(html, "#action-panel", "aria-labelledby") == ["action-panel-title"]
    assert text(html, "#action-panel-title") == "Tavern"
    assert attrs(html, "#action-panel", "phx-window-keydown") == ["deselect"]
    assert attrs(html, "#action-panel", "phx-key") == ["Escape"]
    assert attrs(html, "#close-action-panel", "phx-click") == ["deselect"]
    assert attrs(html, "#close-action-panel", "aria-label") == ["Close"]
    assert attrs(html, "#action-scrim", "phx-click") == ["deselect"]
  end

  test "pending choice banner describes the choice and offers Change" do
    html = render_component(&Hand.pending_choice/1, action: {:discard, "Baths"})
    assert text(html, "#pending-choice") =~ "You chose: Discard Baths"
    assert attrs(html, "#change-choice", "phx-value-card") == ["Baths"]
  end

  test "the hand row centres its cards without clipping the first one" do
    html = render_component(&Hand.hand/1, hand: SampleViews.hand(), selected: nil, pending: nil)
    assert [class] = attrs(html, "#hand > div", "class")
    assert class =~ "justify-center-safe"
  end
end
