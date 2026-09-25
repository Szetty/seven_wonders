defmodule Helios.Lobbies.Lobby do
  @moduledoc """
  A table. Its `id` is the public `game_id`; its owner is its leader forever.
  """
  use Ecto.Schema

  alias Helios.Accounts.User

  @type t :: %__MODULE__{}

  @primary_key {:id, :binary_id, autogenerate: true}
  schema "lobbies" do
    belongs_to :owner, User

    timestamps(type: :utc_datetime)
  end
end
