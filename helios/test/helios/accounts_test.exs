defmodule Helios.AccountsTest do
  use Helios.DataCase

  import Helios.AccountsFixtures

  alias Helios.Accounts
  alias HeliosWeb.Presence

  describe "check_login/2" do
    test "accepts the configured token and a free name" do
      assert Accounts.check_login(valid_access_token(), unique_user_name()) == :ok
    end

    test "checks the access token before the name" do
      assert Accounts.check_login("", "") ==
               {:error, :invalid_access_token, "Access token can't be empty!"}

      assert Accounts.check_login("nope", "") ==
               {:error, :invalid_access_token, "Wrong access token"}
    end

    test "compares the access token exactly (no trimming)" do
      assert Accounts.check_login(" " <> valid_access_token(), unique_user_name()) ==
               {:error, :invalid_access_token, "Wrong access token"}
    end

    test "treats missing or non-string values as empty" do
      assert Accounts.check_login(nil, "alice") ==
               {:error, :invalid_access_token, "Access token can't be empty!"}

      assert Accounts.check_login(valid_access_token(), ["x"]) ==
               {:error, :invalid_name, "Name can't be empty!"}
    end

    test "rejects blank names" do
      for name <- ["", "   "] do
        assert Accounts.check_login(valid_access_token(), name) ==
                 {:error, :invalid_name, "Name can't be empty!"}
      end
    end

    test "rejects names longer than 24 characters" do
      assert Accounts.check_login(valid_access_token(), String.duplicate("a", 25)) ==
               {:error, :invalid_name, "Name must be at most 24 characters"}
    end

    test "accepts an existing user who is not online" do
      user = user_fixture()
      assert Accounts.check_login(valid_access_token(), user.name) == :ok
    end

    test "rejects a name held by an online user, after trimming, case-sensitively" do
      user = user_fixture()
      {:ok, _ref} = Presence.track_user(self(), user)

      assert Accounts.check_login(valid_access_token(), user.name) ==
               {:error, :invalid_name, "Name is already taken"}

      assert Accounts.check_login(valid_access_token(), "  #{user.name}  ") ==
               {:error, :invalid_name, "Name is already taken"}

      assert Accounts.check_login(valid_access_token(), String.upcase(user.name)) == :ok

      :ok = Presence.untrack(self(), Presence.online_topic(), to_string(user.id))
      assert Accounts.check_login(valid_access_token(), user.name) == :ok
    end
  end
end
