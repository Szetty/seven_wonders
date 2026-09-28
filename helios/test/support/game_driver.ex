defmodule Helios.GameDriver do
  @moduledoc """
  Plays games automatically in tests, choosing only among the options the
  engine's own view advertises. Works against a raw engine ref (`core_funs/1`)
  or a persisted game (`games_funs/1`). Player ids are engine ids (strings).
  """

  alias Helios.{Core, Games}
  alias Helios.Games.Choice

  @empty_payment %{west: [], east: []}

  def choose(%{discard_pile: pile} = view, _strategy) when is_list(pile) do
    built = built_names(view, view.me)

    case Enum.find(pile, &(&1 not in built)) do
      nil -> nil
      card -> {:build_from_discard, card}
    end
  end

  def choose(%{hand: []}, _strategy), do: nil
  def choose(%{hand: [first | _]}, :discard), do: {:discard, first.name}

  def choose(%{hand: hand} = view, :wonder) do
    case Enum.find(hand, &Choice.available?(&1.wonder_stage)) do
      nil -> choose(view, :build)
      card -> {:build_wonder_stage, %{card: card.name, payment: first_payment(card.wonder_stage)}}
    end
  end

  def choose(%{hand: [first | _] = hand}, :build) do
    case Enum.find(hand, &Choice.available?(&1.build)) do
      nil -> {:discard, first.name}
      card -> {:build, %{card: card.name, payment: first_payment(card.build)}}
    end
  end

  def built_names(view, player) do
    view.players |> Enum.find(&(&1.name == player)) |> Map.fetch!(:built) |> Enum.map(& &1.name)
  end

  @doc "Every player who must act submits once (strategy defaults to :discard)."
  def step(players, strategies, view_fun, submit_fun) do
    acting =
      case view_fun.(hd(players)).phase do
        %{kind: :extra_turn, extra_turn_player: player} -> [player]
        _phase -> players
      end

    Enum.flat_map(acting, fn player ->
      case choose(view_fun.(player), Map.get(strategies, player, :discard)) do
        nil ->
          []

        action ->
          :ok = submit_fun.(player, action)
          [{player, action}]
      end
    end)
  end

  def play_until(players, strategies, view_fun, submit_fun, stop?, max_steps \\ 100) do
    Enum.reduce_while(1..max_steps, :exhausted, fn _, _ ->
      view = view_fun.(hd(players))

      cond do
        stop?.(view) ->
          {:halt, {:reached, view}}

        view.phase.kind == :game_over ->
          {:halt, {:game_over, view}}

        true ->
          step(players, strategies, view_fun, submit_fun)
          {:cont, :exhausted}
      end
    end)
  end

  def core_funs(ref) do
    {fn player ->
       {:ok, view} = Core.view(ref, player)
       view
     end, fn player, action -> Core.submit(ref, player, action) end}
  end

  def games_funs(game_id) do
    {fn player ->
       {:ok, view} = Games.view(game_id, String.to_integer(player))
       view
     end, fn player, action -> Games.submit(game_id, String.to_integer(player), action) end}
  end

  @doc "First seed whose deterministic playthrough reaches `stop?` (raises if none does)."
  def find_seed(players, wonders, strategies, stop?, seeds \\ 1..300) do
    Enum.find(seeds, fn seed ->
      {:ok, ref} = Core.new_game(players, {:explicit, wonders}, seed)
      {view_fun, submit_fun} = core_funs(ref)
      match?({:reached, _}, play_until(players, strategies, view_fun, submit_fun, stop?))
    end) || raise "no seed in #{inspect(seeds)} reaches the requested state"
  end

  defp first_payment(:free), do: @empty_payment
  defp first_payment({:coins, _}), do: @empty_payment
  defp first_payment({:trade, [option | _]}), do: option.payment
end
