defmodule Helios.Repo.Migrations.AddLastVisitedAtToLobbyInvites do
  use Ecto.Migration

  def change do
    alter table(:lobby_invites) do
      add :last_visited_at, :utc_datetime
    end
  end
end
