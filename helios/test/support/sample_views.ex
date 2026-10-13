defmodule Helios.SampleViews do
  @moduledoc "Constructed PlayerView maps (assumed Phase 3 shape) for pure and component tests."

  def names, do: %{"1" => "Ann", "2" => "Bob", "3" => "Cid", "4" => "Dee"}

  def player(id, overrides \\ %{}) do
    Map.merge(
      %{
        name: id,
        wonder: "Gizah",
        side: :a,
        stages_built: 1,
        stages_total: 3,
        built: [
          %{name: "Lumber Yard", category: :raw_material, age: 1},
          %{name: "Altar", category: :civilian, age: 1},
          %{name: "Stockade", category: :military, age: 1}
        ],
        coins: 4,
        shields: 1,
        military_tokens: [1, -1],
        free_build_available: false
      },
      overrides
    )
  end

  def hand do
    [
      %{
        name: "Baths",
        category: :civilian,
        age: 1,
        build: {:unavailable, :cannot_afford},
        wonder_stage:
          {:trade,
           [
             %{
               payment: %{west: [{:stone, 1}], east: []},
               west_coins: 2,
               east_coins: 0,
               bank_coins: 0
             },
             %{
               payment: %{west: [], east: [{:stone, 1}]},
               west_coins: 0,
               east_coins: 2,
               bank_coins: 0
             }
           ]},
        free_build: false
      },
      %{
        name: "Tavern",
        category: :commercial,
        age: 1,
        build: :free,
        wonder_stage: {:unavailable, :no_wonder_stage_left},
        free_build: true
      },
      %{
        name: "Tree Farm",
        category: :raw_material,
        age: 1,
        build: {:coins, 1},
        wonder_stage: {:unavailable, :cannot_afford},
        free_build: false
      }
    ]
  end

  def view(overrides \\ %{}) do
    Map.merge(
      %{
        me: "1",
        phase: %{
          kind: :choosing_cards,
          age: 2,
          turn: 3,
          direction: :east,
          extra_turn_player: nil,
          extra_turn_kind: nil
        },
        players: [
          player("1"),
          player("2", %{wonder: "Rhódos", side: :b}),
          player("3", %{wonder: "Éphesos"}),
          player("4", %{wonder: "Halikarnassós", side: :b})
        ],
        west: "4",
        east: "2",
        hand: hand(),
        discard_pile: nil,
        discard_count: 2,
        submitted: [{"1", true}, {"2", false}, {"3", true}, {"4", false}],
        my_pending: {:discard, "Baths"},
        scores: nil,
        setup: nil
      },
      overrides
    )
  end

  def wonder_names,
    do: ["Alexandria", "Babylon", "Éphesos", "Gizah", "Halikarnassós", "Olympía", "Rhódos"]

  @doc "A setup-phase view (engine shape: no boards, no hand). Me = \"1\"."
  def setup_view(mode \\ :choose_board) do
    seats =
      case mode do
        :choose_board ->
          [
            %{player: "1", wonder: "Gizah", side: :b, confirmed: true},
            %{player: "2", wonder: "Rhódos", side: :a, confirmed: false},
            %{player: "3", wonder: nil, side: nil, confirmed: false},
            %{player: "4", wonder: "Babylon", side: :a, confirmed: true}
          ]

        :choose_side ->
          [
            %{player: "1", wonder: "Gizah", side: nil, confirmed: false},
            %{player: "2", wonder: "Rhódos", side: :b, confirmed: true},
            %{player: "3", wonder: "Éphesos", side: nil, confirmed: false},
            %{player: "4", wonder: "Babylon", side: :a, confirmed: false}
          ]
      end

    held = for %{wonder: wonder} <- seats, wonder != nil, do: wonder
    available = if mode == :choose_board, do: wonder_names() -- held, else: []

    view(%{
      phase: %{
        kind: :setup,
        age: 1,
        turn: 1,
        direction: :west,
        extra_turn_player: nil,
        extra_turn_kind: nil
      },
      players: [],
      hand: [],
      discard_pile: nil,
      discard_count: 0,
      my_pending: nil,
      scores: nil,
      submitted: Enum.map(seats, &{&1.player, &1.confirmed}),
      setup: %{mode: mode, seats: seats, available: available}
    })
  end

  def scores do
    [
      %{
        player: "1",
        military: 3,
        treasury: 2,
        wonder: 3,
        civilian: 5,
        scientific: 4,
        commercial: 2,
        guild: 0,
        total: 19,
        coins: 6,
        rank: 2
      },
      %{
        player: "2",
        military: 6,
        treasury: 1,
        wonder: 10,
        civilian: 6,
        scientific: 0,
        commercial: 2,
        guild: 0,
        total: 25,
        coins: 4,
        rank: 1
      },
      %{
        player: "3",
        military: -2,
        treasury: 3,
        wonder: 0,
        civilian: 8,
        scientific: 1,
        commercial: 2,
        guild: 0,
        total: 12,
        coins: 9,
        rank: 3
      }
    ]
  end
end
