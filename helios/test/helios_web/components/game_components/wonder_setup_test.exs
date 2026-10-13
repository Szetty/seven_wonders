defmodule HeliosWeb.GameComponents.WonderSetupTest do
  use ExUnit.Case, async: true

  import Phoenix.LiveViewTest
  import HeliosWeb.HTMLHelpers

  alias Helios.SampleViews
  alias HeliosWeb.GameComponents.WonderSetup

  defp render_setup(setup, opts \\ []) do
    render_component(
      &WonderSetup.wonder_setup/1,
      Keyword.merge(
        [
          setup: setup,
          me: "1",
          names: SampleViews.names(),
          wonders: SampleViews.wonder_names(),
          previews: %{}
        ],
        opts
      )
    )
  end

  defp with_seat(setup, player, changes) do
    seats =
      Enum.map(setup.seats, fn seat ->
        if seat.player == player, do: Map.merge(seat, changes), else: seat
      end)

    %{setup | seats: seats}
  end

  defp board_setup, do: SampleViews.setup_view(:choose_board).setup

  test "choose_board: one tile per wonder with its holder, side and actions" do
    html = render_setup(with_seat(board_setup(), "1", %{confirmed: false}))

    assert attrs(html, "#wonder-setup", "data-mode") == ["choose_board"]
    assert count(html, "#setup-boards article[data-wonder]") == 7

    # My board: flipping a side re-claims it.
    assert attrs(html, "#setup-wonder-gizah", "data-holder") == ["1"]
    assert count(html, "#setup-wonder-gizah[data-mine]") == 1
    assert attrs(html, "#setup-wonder-gizah img", "src") == ["/images/wonders/gizahB.png"]
    assert attrs(html, "#side-gizah-b", "aria-pressed") == ["true"]
    assert attrs(html, "#side-gizah-a", "phx-click") == ["claim_wonder"]
    assert attrs(html, "#side-gizah-a", "phx-value-side") == ["a"]
    assert text(html, "#setup-wonder-gizah [data-holder-name]") == "You"

    # Someone else's board: disabled, named, confirmed mark when confirmed.
    assert count(html, "#claim-rhodos[disabled]") == 1
    assert count(html, "#side-rhodos-b[disabled]") == 1
    assert text(html, "#setup-wonder-rhodos [data-holder-name]") == "Bob"
    assert count(html, "#setup-wonder-rhodos [data-confirmed]") == 0
    assert count(html, "#setup-wonder-babylon [data-confirmed]") == 1

    # A free board: claim with the shown side, toggles only preview.
    assert count(html, "#setup-wonder-alexandria[data-holder]") == 0
    assert count(html, "#claim-alexandria[disabled]") == 0
    assert attrs(html, "#claim-alexandria", "phx-click") == ["claim_wonder"]
    assert attrs(html, "#claim-alexandria", "phx-value-wonder") == ["Alexandria"]
    assert attrs(html, "#claim-alexandria", "phx-value-side") == ["a"]
    assert attrs(html, "#side-alexandria-b", "phx-click") == ["show_side"]

    assert attrs(html, "#setup-wonder-halikarnassus img", "src") ==
             ["/images/wonders/halikarnassusA.png"]

    assert count(html, "#confirm-wonder[disabled]") == 0
    assert attrs(html, "#confirm-wonder", "phx-click") == ["confirm_wonder"]
    assert count(html, "#setup-waiting") == 0
  end

  test "a previewed side is shown and claimed on a free board" do
    html =
      render_setup(with_seat(board_setup(), "1", %{confirmed: false}),
        previews: %{"Alexandria" => :b}
      )

    assert attrs(html, "#setup-wonder-alexandria", "data-side") == ["b"]

    assert attrs(html, "#setup-wonder-alexandria img", "src") == [
             "/images/wonders/alexandriaB.png"
           ]

    assert attrs(html, "#claim-alexandria", "phx-value-side") == ["b"]
    assert attrs(html, "#side-alexandria-b", "aria-pressed") == ["true"]
  end

  test "without a claim Confirm is disabled; after confirming everything is locked" do
    unclaimed =
      render_setup(with_seat(board_setup(), "1", %{wonder: nil, side: nil, confirmed: false}))

    assert count(unclaimed, "#confirm-wonder[disabled]") == 1
    assert count(unclaimed, "[data-mine]") == 0
    assert count(unclaimed, "#claim-gizah[disabled]") == 0

    confirmed = render_setup(board_setup())
    assert count(confirmed, "#confirm-wonder[disabled]") == 1
    assert text(confirmed, "#setup-waiting") == "Waiting for Bob, Cid"
    assert count(confirmed, "#claim-alexandria[disabled]") == 1
    assert count(confirmed, "#side-gizah-a[disabled]") == 1
    assert count(confirmed, "#setup-wonder-gizah [data-confirmed]") == 1
  end

  test "choose_side: both sides of my drawn board and the other seats" do
    setup = SampleViews.setup_view(:choose_side).setup
    html = render_setup(setup)

    assert attrs(html, "#wonder-setup", "data-mode") == ["choose_side"]
    assert count(html, "#setup-boards") == 0
    assert attrs(html, "#setup-side-a img", "src") == ["/images/wonders/gizahA.png"]
    assert attrs(html, "#setup-side-b", "phx-click") == ["claim_wonder"]
    assert attrs(html, "#setup-side-b", "phx-value-wonder") == ["Gizah"]
    assert attrs(html, "#setup-side-b", "phx-value-side") == ["b"]
    assert attrs(html, "#setup-side-a", "aria-pressed") == ["false"]
    assert count(html, "#confirm-wonder[disabled]") == 1

    assert count(html, "#setup-seat-1") == 0
    assert text(html, "#setup-seat-2") =~ "Bob"
    assert text(html, "#setup-seat-2") =~ "Rhódos · B"
    assert count(html, "#setup-seat-2 [data-confirmed]") == 1
    assert text(html, "#setup-seat-3") =~ "Éphesos · choosing"

    picked = render_setup(with_seat(setup, "1", %{side: :a}))
    assert attrs(picked, "#setup-side-a", "aria-pressed") == ["true"]
    assert count(picked, "#confirm-wonder[disabled]") == 0
  end
end
