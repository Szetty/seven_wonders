defmodule Helios.Games.GamePlayer do
  @moduledoc "A seat at a game. Seat order is the engine's player order."
  use Ecto.Schema

  @type t :: %__MODULE__{}

  schema "game_players" do
    field :seat, :integer
    belongs_to :game, Helios.Games.Game, type: :binary_id
    belongs_to :user, Helios.Accounts.User
  end
end
