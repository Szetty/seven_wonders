defmodule HeliosWeb.GameComponents.HandTest do
  use ExUnit.Case, async: true

  import Phoenix.LiveViewTest
  import HeliosWeb.HTMLHelpers

  alias Helios.SampleViews
  alias HeliosWeb.GameComponents.Hand

  @neighbours %{west: "Dee", east: "Bob"}

  defp card(name), do: Enum.find(SampleViews.hand(), &(&1.name == name))

  defp panel(card, opts \\ []) do
    render_component(&Hand.action_panel/1,
      card: card,
      neighbours: @neighbours,
      built: Keyword.get(opts, :built, []),
      next_stage: Keyword.get(opts, :next_stage, 2)
    )
  end

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

  test "a row with one way to pay submits directly" do
    html = panel(card("Tree Farm"))
    assert attrs(html, "#build-action", "phx-click") == ["submit"]
    assert attrs(html, "#build-action", "phx-value-kind") == ["build"]
    assert attrs(html, "#build-action", "phx-value-option") == ["0"]
    assert text(html, "#build-action") =~ "1 coin"
    assert count(html, "#build-payments") == 0
  end

  test "a row with several trades reveals them sorted by cost, keeping the engine index" do
    html = panel(card("Baths"))
    assert text(html, "#wonder-action") =~ "Build wonder stage 2"
    assert text(html, "#wonder-action") =~ "from 2 coins"
    assert [js] = attrs(html, "#wonder-action", "phx-click")
    assert js =~ "#wonder-payments"
    assert [class] = attrs(html, "#wonder-payments", "class")
    assert class =~ "hidden"
    assert attrs(html, "#wonder-payments button", "id") == ["wonder-option-0", "wonder-option-1"]
    assert text(html, "#wonder-option-0") =~ "Buy 1 stone from ← Dee"
    assert text(html, "#wonder-option-1") =~ "Buy 1 stone from Bob →"
    assert attrs(html, "#wonder-option-1", "phx-value-option") == ["1"]
  end

  test "sorted display never changes the index sent" do
    expensive = %{
      payment: %{west: [{:ore, 2}], east: []},
      west_coins: 4,
      east_coins: 0,
      bank_coins: 0
    }

    cheap = %{
      payment: %{west: [], east: [{:ore, 2}]},
      west_coins: 0,
      east_coins: 2,
      bank_coins: 0
    }

    card = %{card("Baths") | build: {:trade, [expensive, cheap]}}
    html = panel(card)
    assert attrs(html, "#build-payments button", "id") == ["build-option-1", "build-option-0"]
    assert attrs(html, "#build-option-1", "phx-value-option") == ["1"]
  end

  test "unavailable rows are disabled with the reason; chain rows name the prerequisite" do
    html = panel(card("Baths"))
    assert attrs(html, "#build-action", "disabled") == [""]
    assert text(html, "#build-action") =~ "You can't afford that"

    chain = %{card("Tavern") | build: {:chain, "Altar"}}
    assert text(panel(chain), "#build-action") =~ "Free (you have Altar)"
  end

  test "Olympía free build and discard stay; the explanation box is present" do
    html = panel(card("Tavern"))
    assert attrs(html, "#build-free-button", "phx-value-kind") == ["build_free"]
    assert text(html, "#discard-button") =~ "+3"
    assert count(html, "#card-info") == 1
    assert text(html, "#wonder-action") =~ "Your wonder is complete"
  end

  test "discard button carries one coherent style with no conflicting duplicate utilities" do
    classes = panel(card("Tavern")) |> attrs("#discard-button", "class") |> hd() |> String.split()

    assert "bg-white" in classes
    assert "text-zinc-900" in classes
    assert "hover:bg-zinc-100" in classes
    refute "bg-zinc-900" in classes
    refute "text-white" in classes
    refute "hover:bg-zinc-700" in classes
    assert "pointer-coarse:min-h-11" in classes
  end

  test "a coin cost on top of trading is stated once in the payments list" do
    two = %{payment: %{west: [{:wood, 1}], east: []}, west_coins: 2, east_coins: 0, bank_coins: 1}

    other = %{
      payment: %{west: [], east: [{:wood, 1}]},
      west_coins: 0,
      east_coins: 2,
      bank_coins: 1
    }

    html = panel(%{card("Tree Farm") | build: {:trade, [two, other]}})
    assert text(html, "#build-card-cost") == "includes 1 coin card cost"
  end

  test "the action panel is a dismissable dialog" do
    html = panel(card("Tavern"))

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

  test "the card art opens a zoom" do
    html = panel(card("Tavern"))

    assert [js] = attrs(html, "#zoom-card", "phx-click")
    assert js =~ "#card-zoom"
    assert attrs(html, "#card-zoom img", "src") == ["/images/cards/tavern.png"]
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
