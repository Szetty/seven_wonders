defmodule HeliosWeb.PresenceTest do
  use ExUnit.Case

  alias Helios.Accounts.User
  alias HeliosWeb.Presence

  test "online_topic/0 is the global users topic" do
    assert Presence.online_topic() == "users:online"
  end

  test "track_user/2 marks the user online under a string key until untracked" do
    user = %User{id: 987_654, name: "presence-test"}
    refute Presence.user_online?(user.id)

    assert {:ok, _ref} = Presence.track_user(self(), user)
    assert Presence.user_online?(user.id)
    assert Presence.user_online?("987654")

    assert %{"987654" => %{metas: [%{name: "presence-test"}]}} =
             Presence.list("users:online")

    :ok = Presence.untrack(self(), "users:online", "987654")
    refute Presence.user_online?(user.id)
  end
end
