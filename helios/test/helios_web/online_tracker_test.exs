defmodule HeliosWeb.OnlineTrackerTest do
  use ExUnit.Case, async: true

  alias HeliosWeb.OnlineTracker

  defp presence(entries) do
    Map.new(entries, fn {id, name} -> {to_string(id), %{metas: [%{name: name, phx_ref: "r"}]}} end)
  end

  defp diff(joins, leaves), do: %{joins: presence(joins), leaves: presence(leaves)}

  test "topic/0 and to_user_id/1" do
    assert OnlineTracker.topic() == "users:online"
    assert OnlineTracker.to_user_id("42") == 42
    assert OnlineTracker.to_user_id(42) == 42
  end

  test "new/2 builds the online set from a presence list" do
    tracker = OnlineTracker.new(presence([{1, "ann"}, {2, "bob"}]), grace_ms: 10)

    assert Enum.sort(OnlineTracker.online_users(tracker)) == [{1, "ann"}, {2, "bob"}]
  end

  test "grace_ms defaults to the application config" do
    assert OnlineTracker.new(%{}).grace_ms == OnlineTracker.grace_ms()
  end

  test "a fresh join emits :came_online" do
    tracker = OnlineTracker.new(%{}, grace_ms: 10)

    {tracker, events} = OnlineTracker.handle_diff(tracker, diff([{3, "cid"}], []))

    assert events == [{:came_online, 3, "cid"}]
    assert OnlineTracker.online_users(tracker) == [{3, "cid"}]
  end

  test "a join for an already online user (second tab) emits nothing" do
    tracker = OnlineTracker.new(presence([{3, "cid"}]), grace_ms: 10)

    assert {_tracker, []} = OnlineTracker.handle_diff(tracker, diff([{3, "cid"}], []))
  end

  test "leave then confirm while absent emits :went_offline" do
    tracker = OnlineTracker.new(presence([{3, "cid"}]), grace_ms: 0)

    {tracker, []} = OnlineTracker.handle_diff(tracker, diff([], [{3, "cid"}]))
    assert_receive {:confirm_offline, 3, token}

    {tracker, events} = OnlineTracker.confirm_offline(tracker, 3, token, %{})

    assert events == [{:went_offline, 3, "cid"}]
    assert OnlineTracker.online_users(tracker) == []
  end

  test "leave then join within the grace period emits nothing and cancels the check" do
    tracker = OnlineTracker.new(presence([{3, "cid"}]), grace_ms: 50)

    {tracker, []} = OnlineTracker.handle_diff(tracker, diff([], [{3, "cid"}]))
    {tracker, events} = OnlineTracker.handle_diff(tracker, diff([{3, "cid"}], []))

    assert events == []
    assert tracker.pending == %{}
    assert OnlineTracker.online_users(tracker) == [{3, "cid"}]
    refute_receive {:confirm_offline, 3, _}, 100
  end

  test "leave and join in the same diff (live navigation) emits nothing" do
    tracker = OnlineTracker.new(presence([{3, "cid"}]), grace_ms: 50)

    assert {%OnlineTracker{pending: pending}, []} =
             OnlineTracker.handle_diff(tracker, diff([{3, "cid"}], [{3, "cid"}]))

    assert pending == %{}
  end

  # Review Focus 2: same user, two tabs, one closes.
  test "confirm while the user is still present (other tab) emits nothing" do
    tracker = OnlineTracker.new(presence([{3, "cid"}]), grace_ms: 0)

    {tracker, []} = OnlineTracker.handle_diff(tracker, diff([], [{3, "cid"}]))
    assert_receive {:confirm_offline, 3, token}

    {tracker, events} = OnlineTracker.confirm_offline(tracker, 3, token, presence([{3, "cid"}]))

    assert events == []
    assert OnlineTracker.online_users(tracker) == [{3, "cid"}]
    assert tracker.pending == %{}
  end

  # Review Focus 3: stale timer message after a re-join cancelled the check.
  test "a stale confirm message (token no longer pending) is ignored" do
    tracker = OnlineTracker.new(presence([{3, "cid"}]), grace_ms: 0)

    {tracker, []} = OnlineTracker.handle_diff(tracker, diff([], [{3, "cid"}]))
    assert_receive {:confirm_offline, 3, stale_token}
    {tracker, []} = OnlineTracker.handle_diff(tracker, diff([{3, "cid"}], []))

    assert {^tracker, []} = OnlineTracker.confirm_offline(tracker, 3, stale_token, %{})
  end

  test "a leave for a user that was never online is ignored" do
    tracker = OnlineTracker.new(%{}, grace_ms: 0)

    assert {%OnlineTracker{pending: pending}, []} =
             OnlineTracker.handle_diff(tracker, diff([], [{9, "zed"}]))

    assert pending == %{}
    refute_receive {:confirm_offline, 9, _}, 20
  end
end
