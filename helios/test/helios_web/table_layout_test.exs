defmodule HeliosWeb.TableLayoutTest do
  use ExUnit.Case, async: true

  alias HeliosWeb.TableLayout

  defp players(n), do: Enum.map(1..n, &%{name: to_string(&1)})

  defp build(players, me), do: TableLayout.build(players, me)

  defp roles(n) do
    n |> players() |> build("1") |> Map.fetch!(:seats) |> Enum.map(& &1.role)
  end

  defp ring_arrows(n) do
    n |> players() |> build("1") |> Map.fetch!(:seats) |> Enum.map(& &1.ring_arrows)
  end

  defp list_arrows(n) do
    n |> players() |> build("1") |> Map.fetch!(:seats) |> Enum.map(& &1.list_arrows)
  end

  test "narrow order is W, me, E, then on around the table" do
    for n <- 3..7 do
      names = build(players(n), "1").seats |> Enum.map(& &1.player.name)
      ids = Enum.map(1..n, &to_string/1)
      assert names == [List.last(ids) | Enum.take(ids, n - 1)]
    end
  end

  test "roles: you, west, east, and opposite only for even counts" do
    assert roles(4) == [:west, :me, :east, :opposite]
    assert roles(5) == [:west, :me, :east, nil, nil]
    assert roles(6) == [:west, :me, :east, nil, :opposite, nil]

    assert Enum.map(build(players(4), "1").seats, &TableLayout.label/1) ==
             ["West", "You", "East", "Opposite"]
  end

  test "every seat's neighbours follow seat order (east = i+1)" do
    seat = Enum.find(build(players(7), "1").seats, &(&1.player.name == "4"))
    assert {seat.west, seat.east} == {"3", "5"}
  end

  test "seven-player ring arrows point where the neighbours sit" do
    # narrow order W(7), me(1), E(2), E2(3), E3(4), W3(5), W2(6)
    assert ring_arrows(7) == [
             {"↓", "→"},
             {"←", "→"},
             {"←", "↓"},
             {"↑", "↙"},
             {"↗", "←"},
             {"→", "↖"},
             {"↘", "↑"}
           ]
  end

  test "list arrows wrap at both ends" do
    assert list_arrows(4) == [{"↧", "↓"}, {"↑", "↓"}, {"↑", "↓"}, {"↑", "↥"}]
  end

  test "slots, heights and panel alignment" do
    %{seats: seats, ring_height: h} = build(players(7), "1")
    assert h == 64

    assert Enum.map(seats, &{&1.ring.left, &1.ring.top}) ==
             [{0, 6}, {30, 0}, {78, 6}, {78, 27}, {51, 49}, {27, 49}, {0, 27}]

    assert Enum.map(seats, & &1.panel_align) ==
             [:left, :left, :right, :right, :right, :left, :left]

    assert build(players(3), "1").ring_height == 19
  end

  test "ring path is a closed curve through every seat starting at you" do
    path = build(players(5), "1").ring_path
    assert String.starts_with?(path, "M 50.00 7.00 C ")
    assert path |> String.split(" C ") |> length() == 6
    assert String.ends_with?(path, " Z")
  end

  test "the viewer may sit anywhere in the seat list" do
    names = build(players(5), "3").seats |> Enum.map(& &1.player.name)
    assert names == ["2", "3", "4", "5", "1"]
  end

  test "neighbours/3 resolves display names, You for the viewer" do
    # West = "3"
    seat = hd(build(players(3), "1").seats)

    assert TableLayout.neighbours(seat, %{"2" => "Bob", "3" => "Cid"}, "1") ==
             %{west: "Bob", east: "You", ring: seat.ring_arrows, list: seat.list_arrows}
  end
end
