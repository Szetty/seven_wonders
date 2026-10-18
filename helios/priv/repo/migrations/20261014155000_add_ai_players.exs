defmodule Helios.Repo.Migrations.AddAiPlayers do
  use Ecto.Migration

  # SQLite cannot change a column's nullability, so game_players and
  # game_actions are rebuilt (create / copy / drop / rename) in SQL that
  # Postgres accepts too.
  def up do
    create table(:lobby_ai_seats) do
      add :lobby_id, references(:lobbies, type: :binary_id, on_delete: :delete_all), null: false
      add :model_id, :string, null: false
      timestamps(type: :utc_datetime, updated_at: false)
    end

    create index(:lobby_ai_seats, [:lobby_id])

    create table(:game_players_new) do
      add :game_id, references(:games, type: :binary_id, on_delete: :delete_all), null: false
      add :user_id, references(:users, on_delete: :nothing)
      add :seat, :integer, null: false
      add :ai_model, :string
    end

    execute "INSERT INTO game_players_new (id, game_id, user_id, seat) SELECT id, game_id, user_id, seat FROM game_players"
    drop table(:game_players)
    rename table(:game_players_new), to: table(:game_players)
    create unique_index(:game_players, [:game_id, :seat])
    create unique_index(:game_players, [:game_id, :user_id])
    create index(:game_players, [:user_id])

    create table(:game_actions_new) do
      add :game_id, references(:games, type: :binary_id, on_delete: :delete_all), null: false
      add :seq, :integer, null: false
      add :user_id, references(:users, on_delete: :nothing)
      add :seat, :integer
      add :source, :string, null: false, default: "human"
      add :action, :map, null: false
      timestamps(type: :utc_datetime, updated_at: false)
    end

    execute "INSERT INTO game_actions_new (id, game_id, seq, user_id, action, inserted_at) SELECT id, game_id, seq, user_id, action, inserted_at FROM game_actions"
    drop table(:game_actions)
    rename table(:game_actions_new), to: table(:game_actions)
    create unique_index(:game_actions, [:game_id, :seq])
  end

  # Back to human-only tables: AI seats and their actions are dropped.
  def down do
    drop table(:lobby_ai_seats)

    create table(:game_players_old) do
      add :game_id, references(:games, type: :binary_id, on_delete: :delete_all), null: false
      add :user_id, references(:users, on_delete: :nothing), null: false
      add :seat, :integer, null: false
    end

    execute "INSERT INTO game_players_old (id, game_id, user_id, seat) SELECT id, game_id, user_id, seat FROM game_players WHERE user_id IS NOT NULL"
    drop table(:game_players)
    rename table(:game_players_old), to: table(:game_players)
    create unique_index(:game_players, [:game_id, :seat])
    create unique_index(:game_players, [:game_id, :user_id])
    create index(:game_players, [:user_id])

    create table(:game_actions_old) do
      add :game_id, references(:games, type: :binary_id, on_delete: :delete_all), null: false
      add :seq, :integer, null: false
      add :user_id, references(:users, on_delete: :nothing), null: false
      add :action, :map, null: false
      timestamps(type: :utc_datetime, updated_at: false)
    end

    execute "INSERT INTO game_actions_old (id, game_id, seq, user_id, action, inserted_at) SELECT id, game_id, seq, user_id, action, inserted_at FROM game_actions WHERE user_id IS NOT NULL"
    drop table(:game_actions)
    rename table(:game_actions_old), to: table(:game_actions)
    create unique_index(:game_actions, [:game_id, :seq])
  end
end
