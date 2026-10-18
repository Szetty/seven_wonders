defmodule Helios.Games.GameAction do
  @moduledoc """
  One accepted engine action, in submission order (`seq` starts at 1). The
  actor is `user_id` (a human) or `seat` (an AI seat; owner setup choices for
  AI seats too). `source` is `"human"`, `"ai"` or `"random"` (an AI fallback).
  """
  use Ecto.Schema

  @type t :: %__MODULE__{}

  schema "game_actions" do
    field :seq, :integer
    field :action, :map
    field :seat, :integer
    field :source, :string, default: "human"
    belongs_to :game, Helios.Games.Game, type: :binary_id
    belongs_to :user, Helios.Accounts.User

    timestamps(type: :utc_datetime, updated_at: false)
  end
end
