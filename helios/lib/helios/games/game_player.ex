defmodule Helios.Games.GamePlayer do
  @moduledoc """
  A seat at a game. Seat order is the engine's player order. A human seat has
  `user_id`; an AI seat has `ai_model` (a `Helios.AI.Models` id) and no user.
  """
  use Ecto.Schema

  @type t :: %__MODULE__{}

  schema "game_players" do
    field :seat, :integer
    field :ai_model, :string
    belongs_to :game, Helios.Games.Game, type: :binary_id
    belongs_to :user, Helios.Accounts.User
  end
end
