defmodule Helios.Games.RandomMoves do
  @moduledoc """
  Which plays were AI random fallbacks: a set of `{player_id, age, turn,
  turn_kind}` keys — exactly the fields of a history `{:turn, event}`
  (`turn_kind` is `:regular`, `:play_last_card` or `:build_from_discard`). A
  key is taken from the phase just before the submit; setup has no key.
  """

  @type key :: {String.t(), pos_integer(), pos_integer(), atom()}
  @type t :: MapSet.t(key())

  @spec new() :: t()
  def new, do: MapSet.new()

  @spec record(t(), atom(), map(), String.t()) :: t()
  def record(set, :random, phase, player) do
    case key(phase, player) do
      nil -> set
      key -> MapSet.put(set, key)
    end
  end

  def record(set, _source, _phase, _player), do: set

  @spec key(map(), String.t()) :: key() | nil
  def key(%{kind: :choosing_cards, age: age, turn: turn}, player),
    do: {player, age, turn, :regular}

  def key(%{kind: :extra_turn, age: age, turn: turn, extra_turn_kind: kind}, player),
    do: {player, age, turn, kind}

  def key(_phase, _player), do: nil

  @doc "Whether `player`'s play in the history turn `event` was random."
  @spec random?(t(), map(), String.t()) :: boolean()
  def random?(set, %{age: age, turn: turn, kind: kind}, player),
    do: MapSet.member?(set, {player, age, turn, kind})
end
