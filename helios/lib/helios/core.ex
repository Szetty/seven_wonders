defmodule Helios.Core do
  @moduledoc """
  The 7 Wonders rules engine (Rust, loaded as a NIF). All game rules live in
  the engine; callers only orchestrate and persist.

  A game is an opaque reference. Replaying the same `new_game/3` arguments
  and the same accepted `submit/3` calls always rebuilds the same state.
  """

  alias Helios.Core.Native

  @max_seed 18_446_744_073_709_551_615

  @type game :: reference()
  @type resource :: :wood | :stone | :ore | :clay | :glass | :loom | :papyrus
  @type payment :: %{west: [{resource(), pos_integer()}], east: [{resource(), pos_integer()}]}
  @type action ::
          {:build, %{card: String.t(), payment: payment()}}
          | {:build_wonder_stage, %{card: String.t(), payment: payment()}}
          | {:discard, String.t()}
          | {:build_free, String.t()}
          | {:build_from_discard, String.t()}
  @type wonders :: :random | {:explicit, [{String.t(), :a | :b}]}
  @type setup_error ::
          :invalid_players_number
          | {:duplicate_player, String.t()}
          | {:invalid_wonder, String.t()}
          | {:wonders_length_mismatch, %{players: non_neg_integer(), wonders: non_neg_integer()}}
          | {:duplicate_wonder, String.t()}
  @type action_error ::
          :unknown_player
          | :not_your_turn
          | :game_over
          | :card_not_in_hand
          | :card_not_in_discard
          | :already_built
          | :cannot_afford
          | :invalid_payment
          | :no_wonder_stage_left
          | :free_build_unavailable
          | :action_not_allowed_now
          | :lock_fail

  @doc "Engine version, wonders (with sides) and every card (name, category, age)."
  @spec game_settings() :: %{engine_version: pos_integer(), wonders: [map()], cards: [map()]}
  def game_settings, do: Native.game_settings()

  @doc "Starts a game; `players` is the seat order. `seed` must fit in a u64."
  @spec new_game([String.t()], wonders(), non_neg_integer()) ::
          {:ok, game()} | {:error, setup_error()}
  def new_game(players, wonders, seed)
      when is_list(players) and is_integer(seed) and seed >= 0 and seed <= @max_seed do
    Native.new_game(players, wonders, seed)
  end

  @doc """
  Submits (or replaces) `player`'s choice for the current turn. The turn
  resolves inside the call once every required player has submitted.
  Raises `ArgumentError` for malformed action terms.
  """
  @spec submit(game(), String.t(), action()) :: :ok | {:error, action_error()}
  def submit(game, player, action), do: Native.submit(game, player, action)

  @doc "The table as seen by `player` (hidden information excluded)."
  @spec view(game(), String.t()) :: {:ok, map()} | {:error, :unknown_player | :lock_fail}
  def view(game, player), do: Native.view(game, player)

  @doc "Full internal state, decoded from JSON. Debugging only."
  @spec debug_game(game()) :: {:ok, map()} | {:error, :lock_fail}
  def debug_game(game) do
    with {:ok, json} <- Native.debug_game(game), do: {:ok, Jason.decode!(json)}
  end
end
