defmodule Helios.CoreTest do
  use ExUnit.Case, async: true

  alias Helios.Core

  @wonders ["Rhódos", "Alexandria", "Éphesos", "Babylon", "Olympía", "Halikarnassós", "Gizah"]

  describe "game_settings/0" do
    test "returns the crate version and the seven base-game wonders in data order" do
      settings = Core.game_settings()

      assert settings.version =~ ~r/^\d+\.\d+\.\d+/
      assert settings.wonders == @wonders
    end
  end

  describe "start_game/2" do
    test "starts a 3-player game with random wonders" do
      assert {:ok, game} = Core.start_game(["a", "b", "c"], [])
      assert is_reference(game)
    end

    test "starts a 7-player game with random wonders, using each wonder exactly once" do
      assert {:ok, game} = Core.start_game(~w(a b c d e f g), [])
      assert {:ok, %{"player_states" => player_states}} = Core.debug_game(game)

      wonders =
        for {_name, player_state} <- player_states do
          player_state["wonder"] |> hd() |> String.split(" - ") |> hd()
        end

      assert Enum.sort(wonders) == Enum.sort(@wonders)
    end

    test "starts a game with explicit wonder sides in seat order" do
      sides = [{"Gizah", false}, {"Alexandria", false}, {"Babylon", true}]

      assert {:ok, game} = Core.start_game(["a", "b", "c"], sides)
      assert {:ok, %{"player_states" => player_states}} = Core.debug_game(game)
      assert player_states["a"]["wonder"] == ["Gizah - A"]
      assert player_states["b"]["wonder"] == ["Alexandria - A"]
      assert player_states["c"]["wonder"] == ["Babylon - B"]
    end

    test "accepts accented wonder names exactly as game_settings/0 returns them" do
      %{wonders: wonders} = Core.game_settings()
      sides = Enum.map(wonders, &{&1, true})

      assert {:ok, game} = Core.start_game(~w(a b c d e f g), sides)
      assert {:ok, %{"player_states" => player_states}} = Core.debug_game(game)
      assert player_states["a"]["wonder"] == ["Rhódos - B"]
      assert player_states["f"]["wonder"] == ["Halikarnassós - B"]
    end

    test "rejects fewer than 3 players" do
      assert Core.start_game(["a", "b"], []) == {:error, {:invalid_players_number, 2}}
    end

    test "rejects more than 7 players" do
      assert Core.start_game(~w(a b c d e f g h), []) == {:error, {:invalid_players_number, 8}}
    end

    test "rejects a wonder list whose length differs from the player list" do
      assert Core.start_game(["a", "b", "c"], [{"Gizah", false}]) ==
               {:error, {:invalid_players_and_wonder_side_length, "3 != 1"}}
    end

    test "rejects an unknown wonder (the ASCII spelling of an accented name included)" do
      sides = [{"Gizah", false}, {"Alexandria", false}, {"Rhodos", false}]

      assert Core.start_game(["a", "b", "c"], sides) == {:error, {:invalid_wonder, "Rhodos"}}
    end

    test "raises ArgumentError instead of crashing the VM on malformed input" do
      assert_raise ArgumentError, fn -> Core.start_game([:a, :b, :c], []) end

      assert_raise ArgumentError, fn ->
        Core.start_game(["a", "b", "c"], [{"Gizah", :b}, {"Alexandria", false}, {"Babylon", true}])
      end
    end
  end

  describe "debug_game/1" do
    test "returns the decoded state with one player_states entry per player" do
      {:ok, game} = Core.start_game(["a", "b", "c"], [])

      assert {:ok, %{"player_states" => player_states}} = Core.debug_game(game)
      assert player_states |> Map.keys() |> Enum.sort() == ["a", "b", "c"]

      for {name, player_state} <- player_states do
        assert player_state["name"] == name
        assert player_state["coins"] == 3
      end
    end

    test "raises ArgumentError for a reference that is not a game" do
      assert_raise ArgumentError, fn -> Core.debug_game(make_ref()) end
    end
  end
end
