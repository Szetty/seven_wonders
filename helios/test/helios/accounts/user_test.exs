defmodule Helios.Accounts.UserTest do
  use Helios.DataCase

  alias Helios.Accounts.User

  defp changeset(name), do: User.name_changeset(%User{}, %{name: name})

  test "trims surrounding whitespace" do
    assert get_change(changeset("  Alice \t"), :name) == "Alice"
  end

  test "requires a non-blank name" do
    for name <- [nil, "", "   "] do
      assert %{name: ["Name can't be empty!"]} = errors_on(changeset(name))
    end
  end

  test "accepts 24 characters and rejects 25" do
    assert changeset(String.duplicate("a", 24)).valid?

    assert %{name: ["Name must be at most 24 characters"]} =
             errors_on(changeset(String.duplicate("a", 25)))
  end

  test "counts multi-byte characters, not bytes" do
    assert changeset(String.duplicate("é", 24)).valid?
    assert changeset(String.duplicate("日", 24)).valid?
    refute changeset(String.duplicate("é", 25)).valid?
  end

  test "names are unique and case-sensitive" do
    assert {:ok, _} = Repo.insert(changeset("Bob"))
    assert {:ok, _} = Repo.insert(changeset("bob"))
    assert {:error, changeset} = Repo.insert(changeset("Bob"))
    assert %{name: ["has already been taken"]} = errors_on(changeset)
  end
end
