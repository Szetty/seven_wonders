defmodule Helios.Lobbies.Invite do
  @moduledoc """
  An invitation to a table. Any row (whatever its status) authorizes the
  invitee to open the table; `"pending"` rows drive invite notifications.
  """
  use Ecto.Schema

  import Ecto.Changeset

  alias Helios.Accounts.User
  alias Helios.Lobbies.Lobby

  @type t :: %__MODULE__{}

  schema "lobby_invites" do
    belongs_to :lobby, Lobby, type: :binary_id
    belongs_to :user, User
    field :status, :string, default: "pending"

    timestamps(type: :utc_datetime)
  end

  @doc """
  Changeset for inserting an invite whose `lobby_id` and `user_id` were set
  programmatically. Maps the unique index violation to a changeset error.
  """
  def create_changeset(%__MODULE__{} = invite) do
    invite
    |> change()
    |> unique_constraint([:lobby_id, :user_id])
  end
end
