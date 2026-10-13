defmodule Helios.Lobbies.Lobby do
  @moduledoc """
  A table. Its `id` is the public `game_id`; its owner is its leader forever.
  `wonder_mode` says how the next game hands out wonder boards.
  """
  use Ecto.Schema
  import Ecto.Changeset

  alias Helios.Accounts.User

  @wonder_modes ~w(random choose_side choose_board)

  @type t :: %__MODULE__{}

  @primary_key {:id, :binary_id, autogenerate: true}
  schema "lobbies" do
    field :wonder_mode, :string, default: "random"
    belongs_to :owner, User

    timestamps(type: :utc_datetime)
  end

  @doc "Every valid `wonder_mode`, default first."
  def wonder_modes, do: @wonder_modes

  def wonder_mode_changeset(lobby, attrs) do
    lobby
    |> cast(attrs, [:wonder_mode], empty_values: [])
    |> validate_required([:wonder_mode])
    |> validate_inclusion(:wonder_mode, @wonder_modes)
  end
end
