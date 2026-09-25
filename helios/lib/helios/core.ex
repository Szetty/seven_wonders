defmodule Helios.Core do
  @moduledoc """
  Elixir entry point to the Rust 7 Wonders engine (`core/`, crate `seven_wonders_core`).

  Game handles are opaque NIF resources (they satisfy `is_reference/1`). Game rules
  live in Rust; this module only converts arguments and results.
  Malformed arguments (for example non-string player names) raise `ArgumentError`.
  """

  alias Helios.Core.Native

  @typedoc "Opaque handle to a running engine game."
  @type game :: reference()

  @typedoc "A wonder name as returned by `game_settings/0` and whether side B is used."
  @type wonder_side :: {String.t(), boolean()}

  @type start_error ::
          {:invalid_players_number, non_neg_integer()}
          | {:invalid_players_and_wonder_side_length, String.t()}
          | {:invalid_wonder, String.t()}

  @doc "Engine version and the names of the supported wonders."
  @spec game_settings() :: %{version: String.t(), wonders: [String.t()]}
  def game_settings, do: Native.game_settings()

  @doc """
  Starts a game for 3 to 7 players, in seat order.

  Pass `[]` as `wonder_sides` for random wonders and sides. Otherwise pass one
  `{wonder_name, side_b?}` per player, in the same order.
  """
  @spec start_game([String.t()], [wonder_side()]) :: {:ok, game()} | {:error, start_error()}
  def start_game(players, wonder_sides) do
    wonder_sides =
      Enum.map(wonder_sides, fn {wonder_name, side_b} ->
        %{wonder_name: wonder_name, side_b: side_b}
      end)

    Native.start_game(players, wonder_sides)
  end

  @doc "Full internal engine state as a decoded JSON map. Debug and test use only."
  @spec debug_game(game()) :: {:ok, map()} | {:error, atom()}
  def debug_game(game) do
    with {:ok, json} <- Native.debug_game(game) do
      {:ok, Jason.decode!(json)}
    end
  end
end
