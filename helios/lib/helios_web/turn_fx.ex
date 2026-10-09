defmodule HeliosWeb.TurnFx do
  @moduledoc "Builds the `turn_resolved` payload the `.TurnFx` hook animates (DOM ids computed here, not in JS)."

  alias HeliosWeb.GameAssets

  def payload({:turn, turn}, %{me: me, my_card: my_card, pass: pass}) do
    %{
      plays: Enum.map(turn.plays, &play/1),
      coins: Enum.map(turn.coins, &%{player: &1.player, delta: &1.delta}),
      me: me,
      my_card: my_card,
      pass: pass && Atom.to_string(pass)
    }
  end

  defp play(%{player: player, action: {kind, card}})
       when kind in [:built, :built_free, :built_from_discard],
       do: %{player: player, kind: "built", target: "built-#{player}-#{GameAssets.slug(card)}"}

  defp play(%{player: player, action: {:wonder_stage, n}}),
    do: %{player: player, kind: "wonder_stage", target: "stage-#{player}-#{n}"}

  defp play(%{player: player, action: :discarded}),
    do: %{player: player, kind: "discarded", target: nil}
end
