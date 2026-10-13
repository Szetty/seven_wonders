defmodule Helios.Repo.Migrations.AddWonderModeToLobbies do
  use Ecto.Migration

  def change do
    alter table(:lobbies) do
      add :wonder_mode, :string, null: false, default: "random"
    end
  end
end
