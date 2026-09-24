defmodule Helios.Accounts.User do
  @moduledoc """
  A player identity. Users have no password: they are identified by a unique,
  case-sensitive, permanent name (see `Helios.Accounts`).
  """
  use Ecto.Schema

  import Ecto.Changeset

  @max_name_length 24

  @type t :: %__MODULE__{}

  schema "users" do
    field :name, :string

    timestamps(type: :utc_datetime)
  end

  @doc """
  Normalises and validates a name: trims surrounding whitespace and requires a
  non-empty value of at most #{@max_name_length} characters (graphemes).
  """
  @spec name_changeset(t(), map()) :: Ecto.Changeset.t()
  def name_changeset(user, attrs) do
    user
    |> cast(attrs, [:name])
    |> update_change(:name, &String.trim/1)
    |> validate_required([:name], message: "Name can't be empty!")
    |> validate_length(:name,
      max: @max_name_length,
      message: "Name must be at most #{@max_name_length} characters"
    )
    |> unique_constraint(:name)
  end
end
