defmodule Helios.Lobbies.AISeat do
  @moduledoc "An AI player seated at a lobby's table (persists across games until removed)."
  use Ecto.Schema

  @type t :: %__MODULE__{}

  schema "lobby_ai_seats" do
    field :model_id, :string
    belongs_to :lobby, Helios.Lobbies.Lobby, type: :binary_id

    timestamps(type: :utc_datetime, updated_at: false)
  end
end
