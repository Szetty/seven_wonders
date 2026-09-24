defmodule Helios.Repo.Migrations.CreateUsers do
  use Ecto.Migration

  def change do
    create table(:users) do
      add :name, :string, null: false

      timestamps(type: :utc_datetime)
    end

    # Exact, case-sensitive uniqueness (same as the legacy Go backend).
    create unique_index(:users, [:name])
  end
end
