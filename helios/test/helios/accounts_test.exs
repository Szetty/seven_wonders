defmodule Helios.AccountsTest do
  use Helios.DataCase

  import Helios.AccountsFixtures

  alias Helios.Accounts
  alias Helios.Accounts.{User, UserToken}
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

  describe "login/2" do
    test "creates a user with the trimmed name and returns a session token" do
      name = unique_user_name()

      assert {:ok, %User{} = user, token} = Accounts.login(valid_access_token(), "  #{name} ")
      assert user.name == name
      assert byte_size(token) == 32
      assert Accounts.get_user_by_session_token(token).id == user.id
    end

    test "returns check_login errors without creating anything" do
      name = unique_user_name()

      assert Accounts.login("nope", name) ==
               {:error, :invalid_access_token, "Wrong access token"}

      assert Accounts.login(valid_access_token(), "") ==
               {:error, :invalid_name, "Name can't be empty!"}

      refute Repo.get_by(User, name: name)
    end

    test "re-enters an existing, un-held user and revokes and disconnects old sessions" do
      name = unique_user_name()
      {:ok, user, old_token} = Accounts.login(valid_access_token(), name)
      old_socket_id = Accounts.live_socket_id(old_token)
      HeliosWeb.Endpoint.subscribe(old_socket_id)

      assert {:ok, again, new_token} = Accounts.login(valid_access_token(), " #{name} ")
      assert again.id == user.id
      refute new_token == old_token
      assert Accounts.get_user_by_session_token(old_token) == nil
      assert Accounts.get_user_by_session_token(new_token).id == user.id
      assert_receive %Phoenix.Socket.Broadcast{topic: ^old_socket_id, event: "disconnect"}

      assert Repo.aggregate(from(t in UserToken, where: t.user_id == ^user.id), :count) == 1
    end

    test "rejects a name held by an online user" do
      user = user_fixture()
      {:ok, _ref} = Presence.track_user(self(), user)

      assert Accounts.login(valid_access_token(), user.name) ==
               {:error, :invalid_name, "Name is already taken"}

      :ok = Presence.untrack(self(), Presence.online_topic(), to_string(user.id))
    end
  end

  describe "get_user_by_session_token/1" do
    setup do
      {:ok, user, token} = Accounts.login(valid_access_token(), unique_user_name())
      %{user: user, token: token}
    end

    test "returns the user for a fresh token", %{user: user, token: token} do
      assert %User{id: id} = Accounts.get_user_by_session_token(token)
      assert id == user.id
    end

    test "returns nil for unknown or non-binary tokens" do
      assert Accounts.get_user_by_session_token("bogus") == nil
      assert Accounts.get_user_by_session_token(nil) == nil
    end

    test "accepts tokens younger than 60 days and rejects older ones", %{
      user: user,
      token: token
    } do
      set_token_age(token, 59)
      assert Accounts.get_user_by_session_token(token).id == user.id

      set_token_age(token, 61)
      assert Accounts.get_user_by_session_token(token) == nil
    end
  end

  describe "delete_session_token/1" do
    test "deletes the token and is idempotent" do
      {:ok, _user, token} = Accounts.login(valid_access_token(), unique_user_name())

      assert Accounts.delete_session_token(token) == :ok
      assert Accounts.get_user_by_session_token(token) == nil
      assert Accounts.delete_session_token(token) == :ok
    end
  end

  describe "live_socket_id/1" do
    test "is derived from the token hash" do
      assert Accounts.live_socket_id("raw") ==
               "users_sessions:" <> Base.url_encode64(:crypto.hash(:sha256, "raw"))
    end
  end

  defp set_token_age(token, days) do
    inserted_at = DateTime.utc_now() |> DateTime.add(-days, :day) |> DateTime.truncate(:second)
    hash = UserToken.hash_token(token)

    {1, _} =
      Repo.update_all(from(t in UserToken, where: t.token_hash == ^hash),
        set: [inserted_at: inserted_at]
      )
  end
end
