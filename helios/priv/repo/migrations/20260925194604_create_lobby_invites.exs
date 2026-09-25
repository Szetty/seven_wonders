defmodule Helios.Repo.Migrations.CreateLobbyInvites do
  use Ecto.Migration

  def change do
    create table(:lobby_invites) do
      add :lobby_id, references(:lobbies, type: :binary_id, on_delete: :delete_all), null: false
      add :user_id, references(:users, on_delete: :delete_all), null: false
      add :status, :string, null: false, default: "pending"

      timestamps(type: :utc_datetime)
    end

    create unique_index(:lobby_invites, [:lobby_id, :user_id])
    create index(:lobby_invites, [:user_id])
  end
end
