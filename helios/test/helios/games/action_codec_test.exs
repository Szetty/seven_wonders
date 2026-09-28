defmodule Helios.Games.ActionCodecTest do
  use ExUnit.Case, async: true

  alias Helios.Games.ActionCodec

  @actions [
    {:build, %{card: "Altar", payment: %{west: [], east: []}}},
    {:build,
     %{card: "Aqueduct", payment: %{west: [{:wood, 1}, {:papyrus, 2}], east: [{:loom, 1}]}}},
    {:build_wonder_stage,
     %{card: "Baths", payment: %{west: [{:stone, 2}], east: [{:ore, 1}, {:clay, 1}, {:glass, 1}]}}},
    {:discard, "Altar"},
    {:build_free, "Palace"},
    {:build_from_discard, "Library"}
  ]

  defp through_json(map), do: map |> Jason.encode!() |> Jason.decode!()

  test "encodes to the documented JSON-safe maps" do
    assert ActionCodec.encode(
             {:build, %{card: "Altar", payment: %{west: [{:wood, 1}], east: []}}}
           ) ==
             %{
               "type" => "build",
               "card" => "Altar",
               "payment" => %{"west" => [["wood", 1]], "east" => []}
             }

    assert ActionCodec.encode({:discard, "Altar"}) == %{"type" => "discard", "card" => "Altar"}
  end

  test "round-trips every action variant through JSON" do
    for action <- @actions do
      assert action |> ActionCodec.encode() |> through_json() |> ActionCodec.decode() ==
               {:ok, action}
    end
  end

  test "rejects unknown types, unknown resources and malformed input" do
    for bad <- [
          %{"type" => "steal", "card" => "Altar"},
          %{
            "type" => "build",
            "card" => "Altar",
            "payment" => %{"west" => [["gold", 1]], "east" => []}
          },
          %{
            "type" => "build",
            "card" => "Altar",
            "payment" => %{"west" => [["wood", 0]], "east" => []}
          },
          %{
            "type" => "build",
            "card" => "Altar",
            "payment" => %{"west" => [["wood", "1"]], "east" => []}
          },
          %{"type" => "build", "card" => "Altar", "payment" => %{"west" => []}},
          %{"type" => "build", "card" => "Altar"},
          %{"type" => "discard"},
          %{"type" => "discard", "card" => 5},
          %{},
          "discard"
        ] do
      assert ActionCodec.decode(bad) == {:error, :invalid_action}, "accepted #{inspect(bad)}"
    end
  end

  test "never creates atoms from unknown strings" do
    name = "never_an_atom_#{System.unique_integer([:positive])}"
    assert {:error, :invalid_action} = ActionCodec.decode(%{"type" => name, "card" => "x"})
    assert_raise ArgumentError, fn -> String.to_existing_atom(name) end
  end

  test "wonder selections round-trip" do
    assert ActionCodec.encode_wonders(:random) == nil
    assert ActionCodec.decode_wonders(nil) == {:ok, :random}

    selection = {:explicit, [{"Gizah", :a}, {"Rhódos", :b}]}
    encoded = ActionCodec.encode_wonders(selection)
    assert encoded == %{"explicit" => [["Gizah", "a"], ["Rhódos", "b"]]}
    assert encoded |> through_json() |> ActionCodec.decode_wonders() == {:ok, selection}

    assert ActionCodec.decode_wonders(%{"explicit" => [["Gizah", "c"]]}) ==
             {:error, :invalid_wonders}

    assert ActionCodec.decode_wonders(%{"other" => []}) == {:error, :invalid_wonders}
  end
end
