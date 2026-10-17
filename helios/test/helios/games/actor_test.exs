defmodule Helios.Games.ActorTest do
  use ExUnit.Case, async: true

  alias Helios.Games.{Actor, GameAction, GamePlayer}

  test "engine ids round-trip" do
    assert Actor.player_id(42) == "42"
    assert Actor.player_id({:ai, 3}) == "ai-3"
    assert Actor.from_player_id("42") == 42
    assert Actor.from_player_id("ai-3") == {:ai, 3}
  end

  test "seats and log rows name their actor" do
    assert Actor.of_seat(%GamePlayer{user_id: 7, seat: 0, ai_model: nil}) == 7
    assert Actor.of_seat(%GamePlayer{user_id: nil, seat: 2, ai_model: "fake-first"}) == {:ai, 2}
    assert Actor.of_row(%GameAction{user_id: 7, seat: nil}) == 7
    assert Actor.of_row(%GameAction{user_id: nil, seat: 2}) == {:ai, 2}
  end

  test "row fields set exactly one of user_id / seat" do
    assert Actor.row_fields(7) == %{user_id: 7, seat: nil}
    assert Actor.row_fields({:ai, 2}) == %{user_id: nil, seat: 2}
  end
end
