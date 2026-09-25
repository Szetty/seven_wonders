defmodule Helios.AccountsFixtures do
  @moduledoc """
  Test helpers for creating users.
  """
  alias Helios.Accounts.User
  alias Helios.Repo

  @doc "A name that is unique within the test run (always ≤ 24 characters)."
  def unique_user_name, do: "user#{System.unique_integer([:positive])}"

  @doc "The access token configured for the current environment."
  def valid_access_token, do: Application.fetch_env!(:helios, :access_token)

  @doc "Inserts a user directly (no session). Accepts `name:`."
  def user_fixture(attrs \\ %{}) do
    attrs = Map.new(attrs)

    %User{}
    |> User.name_changeset(%{name: Map.get(attrs, :name, unique_user_name())})
    |> Repo.insert!()
  end
end
