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
    {:build_from_discard, "Library"},
    {:claim_wonder, %{wonder: "Rhódos", side: :b}},
    :confirm_wonder
  ]

  defp through_json(map), do: map |> Jason.encode!() |> Jason.decode!()

  test "setup actions encode with the existing type key" do
    assert ActionCodec.encode({:claim_wonder, %{wonder: "Gizah", side: :a}}) ==
             %{"type" => "claim_wonder", "wonder" => "Gizah", "side" => "a"}

    assert ActionCodec.encode(:confirm_wonder) == %{"type" => "confirm_wonder"}

    for bad <- [
          %{"type" => "claim_wonder", "wonder" => "Gizah", "side" => "c"},
          %{"type" => "claim_wonder", "wonder" => 5, "side" => "a"},
          %{"type" => "claim_wonder", "side" => "a"}
        ] do
      assert ActionCodec.decode(bad) == {:error, :invalid_action}, "accepted #{inspect(bad)}"
    end
  end

  test "setup wonder modes round-trip; unknown modes are rejected" do
    for mode <- [:choose_board, :choose_side] do
      encoded = ActionCodec.encode_wonders(mode)
      assert encoded == %{"mode" => Atom.to_string(mode)}
      assert encoded |> through_json() |> ActionCodec.decode_wonders() == {:ok, mode}
    end

    assert ActionCodec.decode_wonders(%{"mode" => "random"}) == {:error, :invalid_wonders}
    assert ActionCodec.decode_wonders(%{"mode" => "chaos"}) == {:error, :invalid_wonders}
  end

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
