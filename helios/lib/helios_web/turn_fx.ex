defmodule HeliosWeb.TurnFx do
  @moduledoc "Builds the `turn_resolved` payload the `.TurnFx` hook animates (DOM ids computed here, not in JS)."

  alias HeliosWeb.GameAssets

  def payload({:turn, turn}, %{me: me, my_card: my_card, pass: pass}) do
    %{
      plays: Enum.map(turn.plays, &play(&1, me)),
      coins: Enum.map(turn.coins, &%{player: &1.player, delta: &1.delta}),
      me: me,
      my_card: my_card,
      pass: pass && Atom.to_string(pass)
    }
  end

  # Halikarnassós: the builder saw the card in the picker (or the replay grid),
  # everyone else sees it leave the discard-pile marker in the status bar.
  defp play(%{player: player, action: {:built_from_discard, card}}, me) do
    slug = GameAssets.slug(card)
    from = if player == me, do: "discard-card-#{slug}", else: "discard-pile"
    %{player: player, kind: "built", target: "built-#{player}-#{slug}", from: from}
  end

  defp play(%{player: player, action: {kind, card}}, _me) when kind in [:built, :built_free],
    do: %{player: player, kind: "built", target: "built-#{player}-#{GameAssets.slug(card)}"}

  defp play(%{player: player, action: {:wonder_stage, n}}, _me),
    do: %{player: player, kind: "wonder_stage", target: "stage-#{player}-#{n}"}

  defp play(%{player: player, action: :discarded}, _me),
    do: %{player: player, kind: "discarded", target: nil}
end
