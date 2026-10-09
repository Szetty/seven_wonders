defmodule HeliosWeb.HistoryText do
  @moduledoc "English for history events. `ctx` = %{me: id, names: %{id => name}, view: view (seat order)}."

  alias HeliosWeb.GameFormat

  def play(%{player: player, action: action} = play, ctx) do
    "#{actor(player, ctx)} #{did(action)}" <> payments(play, ctx)
  end

  defp did({:built, card}), do: "built #{card}"
  defp did({:built_free, card}), do: "built #{card} for free (Olympía)"
  defp did({:built_from_discard, card}), do: "built #{card} from the discard pile"
  defp did({:wonder_stage, n}), do: "built wonder stage #{n}"
  defp did(:discarded), do: "discarded a card"

  defp payments(%{coins_to_west: 0, coins_to_east: 0}, _ctx), do: ""

  defp payments(%{player: player, coins_to_west: west, coins_to_east: east}, ctx) do
    neighbours = GameFormat.neighbours_of(ctx.view, player, Map.put(ctx.names, ctx.me, "you"))

    paid =
      [{west, neighbours.west}, {east, neighbours.east}]
      |> Enum.reject(fn {coins, _} -> coins == 0 end)
      |> Enum.map_join(" and ", fn {coins, name} -> "#{coins(coins)} to #{name}" end)

    ", paid " <> paid
  end

  def coins(changes, ctx) do
    Enum.map_join(changes, " · ", fn %{player: player, delta: delta} ->
      "#{actor(player, ctx)} #{signed(delta)}"
    end)
  end

  def battle(%{player_token: nil, rival_token: nil} = b, ctx), do: score(b, ctx) <> ": tie"

  def battle(b, ctx) do
    score(b, ctx) <>
      ": #{actor(b.player, ctx)} #{signed(b.player_token)}, #{actor(b.rival, ctx)} #{signed(b.rival_token)}"
  end

  def turn_title(%{turn: turn, kind: :regular}), do: "Turn #{turn}"
  def turn_title(%{turn: turn, kind: :play_last_card}), do: "Turn #{turn} · last card"

  def turn_title(%{turn: turn, kind: :build_from_discard}),
    do: "Turn #{turn} · from the discard pile"

  def actor(player, %{me: player}), do: "You"
  def actor(player, ctx), do: GameFormat.player_name(ctx.names, player)

  defp score(b, ctx),
    do:
      "#{actor(b.player, ctx)} #{b.player_strength} vs #{actor(b.rival, ctx)} #{b.rival_strength}"

  defp signed(n) when n > 0, do: "+#{n}"
  defp signed(n), do: "−#{abs(n)}"

  defp coins(1), do: "1 coin"
  defp coins(n), do: "#{n} coins"
end
