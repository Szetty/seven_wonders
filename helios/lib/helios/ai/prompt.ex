defmodule Helios.AI.Prompt do
  @moduledoc """
  The token-light payload of one AI decision, built from the AI seat's own
  view plus the static catalogue:

    * `state` — compact JSON-able position (boards for me and my neighbours,
      one-line summaries for everyone else, my hand with effect text);
    * `moves` — the legal moves as short ids (`b<n>` build, `w<n>` wonder
      stage, `f<n>` Olympia free build, `d<n>` discard, `x<n>` build from the
      discard pile) with cost labels; trades show only their cheapest total;
    * `primer` — a fixed persona line that relies on the model knowing the game.

  `user_message/2` renders the state for chat models (TOON or JSON).

  Production and science totals are presentation only (summed from catalogue
  effect specs); legality and costs come from the view's build options.
  """

  alias Helios.Catalogue
  alias Helios.Games.Choice
  alias HeliosWeb.{CardText, GameFormat}

  @primer "You are an expert player of the 7 Wonders board game (base game). " <>
            "You see your current position and your legal moves. " <>
            "Choose the move that maximises your final score."

  @colours %{
    raw_material: "brown",
    manufactured_good: "grey",
    civilian: "blue",
    commercial: "yellow",
    military: "red",
    scientific: "green",
    guild: "purple"
  }

  @type move :: %{
          id: String.t(),
          label: String.t(),
          card: String.t(),
          kind: String.t(),
          trade_options: non_neg_integer()
        }
  @type t :: %{state: map(), moves: [move()], primer: String.t()}

  @spec primer() :: String.t()
  def primer, do: @primer

  @spec build(map()) :: t()
  def build(view), do: %{state: state(view), moves: moves(view), primer: @primer}

  @doc "The browser-style choice for a move; trades get a random payment index."
  @spec to_choice(move()) :: Choice.t()
  def to_choice(%{card: card, kind: kind, trade_options: n}) do
    option = if n > 0, do: :rand.uniform(n) - 1, else: 0
    %{card: card, kind: kind, option: option}
  end

  @doc """
  The chat-model user message: the state as TOON (tables for uniform lists)
  or compact JSON, then one `id: label` line per move.
  """
  @spec user_message(t(), :toon | :json) :: String.t()
  def user_message(prompt, format) do
    render(format, prompt.state) <>
      "\nMoves:\n" <> Enum.map_join(prompt.moves, "", &"#{&1.id}: #{&1.label}\n")
  end

  defp render(:json, state), do: "State (JSON):\n" <> Jason.encode!(state)

  # String keys (via a JSON round-trip) so the encoder sees plain data.
  defp render(:toon, state),
    do: "State (TOON):\n" <> Toon.encode!(state |> Jason.encode!() |> Jason.decode!())

  ## Moves

  defp moves(%{phase: %{kind: :extra_turn, extra_turn_kind: :build_from_discard}} = view)
       when is_list(view.discard_pile) do
    view.discard_pile
    |> Enum.with_index()
    |> Enum.map(fn {card, n} ->
      move("x#{n}", "build #{card} from the discard pile, free", card, "build_from_discard", 0)
    end)
  end

  defp moves(view) do
    me = Enum.find(view.players, &(&1.name == view.me))

    view.hand
    |> Enum.with_index()
    |> Enum.flat_map(fn {card, n} -> card_moves(card, n, me.stages_built + 1) end)
  end

  defp card_moves(card, n, stage) do
    [
      Choice.available?(card.build) &&
        move(
          "b#{n}",
          "build #{card.name}: #{cost(card.build)}",
          card.name,
          "build",
          trades(card.build)
        ),
      Choice.available?(card.wonder_stage) &&
        move(
          "w#{n}",
          "build wonder stage #{stage} with #{card.name}: #{cost(card.wonder_stage)}",
          card.name,
          "wonder_stage",
          trades(card.wonder_stage)
        ),
      card.free_build &&
        move("f#{n}", "build #{card.name} free (once per age)", card.name, "build_free", 0),
      move("d#{n}", "discard #{card.name}: +3 coins", card.name, "discard", 0)
    ]
    |> Enum.filter(& &1)
  end

  defp move(id, label, card, kind, trade_options),
    do: %{id: id, label: label, card: card, kind: kind, trade_options: trade_options}

  defp cost(:free), do: "free"
  defp cost({:chain, from}), do: "free (chain from #{from})"
  defp cost({:coins, 1}), do: "1 coin"
  defp cost({:coins, n}), do: "#{n} coins"

  defp cost({:trade, options}),
    do: "trade, from #{options |> Enum.map(&total/1) |> Enum.min()} coins"

  defp total(%{west_coins: west, east_coins: east, bank_coins: bank}), do: west + east + bank

  defp trades({:trade, options}), do: length(options)
  defp trades(_option), do: 0

  ## State

  defp state(view) do
    by_name = Map.new(view.players, &{&1.name, &1})

    %{
      age: view.phase.age,
      turn: view.phase.turn,
      pass: Atom.to_string(view.phase.direction),
      me: board(Map.fetch!(by_name, view.me)),
      west: board(Map.fetch!(by_name, view.west)),
      east: board(Map.fetch!(by_name, view.east)),
      others: Enum.map(GameFormat.others(view), &summary/1),
      hand: Enum.map(view.hand, &card_entry(&1.name)),
      discard_count: view.discard_count
    }
    |> put_if(
      :extra_turn,
      view.phase.extra_turn_kind && Atom.to_string(view.phase.extra_turn_kind)
    )
    |> put_if(
      :discard_pile,
      is_list(view.discard_pile) && Enum.map(view.discard_pile, &card_entry/1)
    )
  end

  defp board(player) do
    effects = city_effects(player)

    %{
      wonder: "#{player.wonder} #{GameFormat.side_label(player.side)}",
      coins: player.coins,
      stages: "#{player.stages_built}/#{player.stages_total}",
      built: Enum.map(player.built, & &1.name),
      produces: produces(effects),
      science: science(effects),
      shields: player.shields,
      military_tokens: player.military_tokens
    }
    |> put_if(:next_stage, next_stage(player))
  end

  defp summary(player) do
    %{
      wonder: "#{player.wonder} #{GameFormat.side_label(player.side)}",
      stages: "#{player.stages_built}/#{player.stages_total}",
      coins: player.coins,
      shields: player.shields,
      cards:
        player.built
        |> Enum.frequencies_by(&Map.fetch!(@colours, &1.category))
        |> Enum.sort()
        |> Enum.map_join(", ", fn {colour, n} -> "#{n} #{colour}" end)
    }
  end

  # Flat rows of scalars with identical keys, so TOON writes them as a table.
  defp card_entry(name) do
    info = Catalogue.card(name)

    %{
      name: name,
      colour: Map.fetch!(@colours, info.category),
      effects: Enum.join(CardText.effects(info.effects), "; "),
      chains_to: Enum.join(info.chains_to, ", ")
    }
  end

  defp city_effects(player) do
    board = Catalogue.wonder_board(player.wonder, player.side)
    stages = board.stages |> Enum.take(player.stages_built) |> Enum.flat_map(& &1.effects)

    cards =
      Enum.flat_map(player.built, fn card ->
        case Catalogue.card(card.name) do
          nil -> []
          info -> info.effects
        end
      end)

    board.effects ++ stages ++ cards
  end

  defp next_stage(%{stages_built: built, stages_total: total} = player) when built < total do
    case Catalogue.stage(player.wonder, player.side, built + 1) do
      nil ->
        nil

      stage ->
        "#{CardText.cost(stage.cost)} -> #{Enum.join(CardText.effects(stage.effects), "; ")}"
    end
  end

  defp next_stage(_player), do: nil

  defp produces(effects) do
    tally(effects, fn
      {:produces, %{resources: resources, choice: choice}} -> {resources, choice}
      _other -> nil
    end)
  end

  defp science(effects) do
    tally(effects, fn
      {:science, %{symbols: symbols, choice: choice}} -> {symbols, choice}
      _other -> nil
    end)
  end

  # Fixed items counted ("wood x2"), alternatives listed ("clay/ore").
  defp tally(effects, pick) do
    {fixed, choices} =
      Enum.reduce(effects, {%{}, []}, fn effect, {fixed, choices} ->
        case pick.(effect) do
          {items, false} ->
            {Enum.reduce(items, fixed, &Map.update(&2, &1, 1, fn n -> n + 1 end)), choices}

          {items, true} ->
            {fixed, [Enum.map_join(items, "/", &to_string/1) | choices]}

          nil ->
            {fixed, choices}
        end
      end)

    counted =
      fixed
      |> Enum.sort()
      |> Enum.map(fn
        {item, 1} -> to_string(item)
        {item, n} -> "#{item} x#{n}"
      end)

    counted ++ Enum.reverse(choices)
  end

  defp put_if(map, _key, value) when value in [nil, false], do: map
  defp put_if(map, key, value), do: Map.put(map, key, value)
end
