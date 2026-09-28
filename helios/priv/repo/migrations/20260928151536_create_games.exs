defmodule Helios.Repo.Migrations.CreateGames do
  use Ecto.Migration

  def change do
    create table(:games, primary_key: false) do
      add :id, :binary_id, primary_key: true
      add :lobby_id, references(:lobbies, type: :binary_id, on_delete: :delete_all), null: false
      add :seed, :integer, null: false
      add :engine_version, :integer, null: false
      # nil = random wonders (derived from the seed); otherwise the explicit selection.
      add :wonders, :map
      add :status, :string, null: false, default: "active"
      add :final_scores, :map
      add :finished_at, :utc_datetime

      timestamps(type: :utc_datetime)
    end

    create unique_index(:games, [:lobby_id],
             where: "status = 'active'",
             name: :games_one_active_per_lobby
           )

    create table(:game_players) do
      add :game_id, references(:games, type: :binary_id, on_delete: :delete_all), null: false
      add :user_id, references(:users, on_delete: :nothing), null: false
      add :seat, :integer, null: false
    end

    create unique_index(:game_players, [:game_id, :seat])
    create unique_index(:game_players, [:game_id, :user_id])
    create index(:game_players, [:user_id])

    create table(:game_actions) do
      add :game_id, references(:games, type: :binary_id, on_delete: :delete_all), null: false
      add :seq, :integer, null: false
      add :user_id, references(:users, on_delete: :nothing), null: false
      add :action, :map, null: false

      timestamps(type: :utc_datetime, updated_at: false)
    end

    create unique_index(:game_actions, [:game_id, :seq])
  end
end
