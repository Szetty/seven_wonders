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

  test "unavailable builds show the reason; trade options list who gets paid" do
    html = render_component(&Hand.action_panel/1, card: card("Baths"))

    assert text(html, "#build-unavailable") == "You can't afford that"
    assert attrs(html, "#build-unavailable", "disabled") == [""]
    assert text(html, "#wonder-option-0") == "West 2 · East 0 · Bank 0"
    assert text(html, "#wonder-option-1") == "West 0 · East 2 · Bank 0"
    assert attrs(html, "#wonder-option-1", "phx-value-option") == ["1"]
    assert attrs(html, "#wonder-option-1", "phx-value-kind") == ["wonder_stage"]
    assert count(html, "#build-free-button") == 0
    assert attrs(html, "#discard-button", "phx-value-kind") == ["discard"]
  end

  test "free, coin and Olympía free-build options" do
    tavern = render_component(&Hand.action_panel/1, card: card("Tavern"))
    assert text(tavern, "#build-option-0") == "Free"
    assert text(tavern, "#wonder-unavailable") == "Your wonder is complete"
    assert attrs(tavern, "#build-free-button", "phx-value-kind") == ["build_free"]

    farm = render_component(&Hand.action_panel/1, card: card("Tree Farm"))
    assert text(farm, "#build-option-0") == "Pay 1 coin"
  end

  test "pending choice banner describes the choice and offers Change" do
    html = render_component(&Hand.pending_choice/1, action: {:discard, "Baths"})
    assert text(html, "#pending-choice") =~ "You chose: Discard Baths"
    assert attrs(html, "#change-choice", "phx-value-card") == ["Baths"]
  end
end
