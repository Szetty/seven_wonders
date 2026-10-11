defmodule HeliosWeb.TurnFxTest do
  use ExUnit.Case, async: true

  alias HeliosWeb.TurnFx

  test "plays point at the DOM ids they animate to" do
    turn =
      {:turn,
       %{
         age: 1,
         turn: 1,
         kind: :regular,
         plays: [
           %{player: "1", action: {:built, "Lumber Yard"}, coins_to_west: 0, coins_to_east: 0},
           %{player: "2", action: {:wonder_stage, 1}, coins_to_west: 0, coins_to_east: 0},
           %{player: "3", action: :discarded, coins_to_west: 0, coins_to_east: 0}
         ],
         coins: [%{player: "3", delta: 3}]
       }}

    assert TurnFx.payload(turn, %{me: "1", my_card: "Lumber Yard", pass: :west}) == %{
             plays: [
               %{player: "1", kind: "built", target: "built-1-lumberyard"},
               %{player: "2", kind: "wonder_stage", target: "stage-2-1"},
               %{player: "3", kind: "discarded", target: nil}
             ],
             coins: [%{player: "3", delta: 3}],
             me: "1",
             my_card: "Lumber Yard",
             pass: "west"
           }
  end

  test "a card built from the discard pile flies from the picker for the builder, from the pile for others" do
    turn =
      {:turn,
       %{
         age: 1,
         turn: 3,
         kind: :build_from_discard,
         plays: [
           %{
             player: "1",
             action: {:built_from_discard, "Lumber Yard"},
             coins_to_west: 0,
             coins_to_east: 0
           }
         ],
         coins: []
       }}

    assert %{plays: [builder]} =
             TurnFx.payload(turn, %{me: "1", my_card: "Lumber Yard", pass: nil})

    assert builder == %{
             player: "1",
             kind: "built",
             target: "built-1-lumberyard",
             from: "discard-card-lumberyard"
           }

    assert %{plays: [observer]} = TurnFx.payload(turn, %{me: "2", my_card: nil, pass: nil})
    assert observer.from == "discard-pile"
    assert observer.target == "built-1-lumberyard"
  end
end
