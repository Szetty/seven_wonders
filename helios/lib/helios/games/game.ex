defmodule Helios.Games.Game do
  @moduledoc "A game played at a lobby's table. `seed` + `wonders` + the action log fully determine its state."
  use Ecto.Schema
  import Ecto.Changeset

  @primary_key {:id, :binary_id, autogenerate: true}
  @foreign_key_type :binary_id
  @max_seed 9_223_372_036_854_775_807

  @type t :: %__MODULE__{}

  schema "games" do
    field :seed, :integer
    field :engine_version, :integer
    field :wonders, :map
    field :status, :string, default: "active"
    field :final_scores, :map
    field :finished_at, :utc_datetime

    belongs_to :lobby, Helios.Lobbies.Lobby
    has_many :players, Helios.Games.GamePlayer, preload_order: [asc: :seat]

    timestamps(type: :utc_datetime)
  end

  @doc "For new games. `lobby_id` must be set on the struct, never cast."
  def create_changeset(game, attrs) do
    game
    |> cast(attrs, [:seed, :engine_version, :wonders])
    |> validate_required([:seed, :engine_version])
    |> validate_number(:seed, greater_than_or_equal_to: 0, less_than_or_equal_to: @max_seed)
    # Postgres reports the partial index by name; SQLite only reports the column,
    # which ecto_sqlite3 turns into "games_lobby_id_index". Declare both.
    |> unique_constraint(:lobby_id, name: :games_one_active_per_lobby, message: "game in progress")
    |> unique_constraint(:lobby_id, name: :games_lobby_id_index, message: "game in progress")
  end

  def finish_changeset(game, final_scores) when is_map(final_scores) do
    change(game,
      status: "finished",
      final_scores: final_scores,
      finished_at: DateTime.utc_now() |> DateTime.truncate(:second)
    )
  end

  def abort_changeset(game), do: change(game, status: "aborted")
end
