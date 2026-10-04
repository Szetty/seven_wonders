defmodule Helios.Catalogue do
  @moduledoc """
  Static card and wonder data from the engine (`Helios.Core.game_settings/0`),
  indexed once and cached in `:persistent_term`. Loom, Glassworks and Press
  appear in two ages with identical data; the first entry wins.
  """

  alias Helios.Core

  @key {__MODULE__, :index}

  def card(name), do: Map.get(index().cards, name)
  def cards, do: Map.values(index().cards)
  def wonder_board(wonder, side), do: Map.get(index().boards, {wonder, side})
  def boards, do: Map.to_list(index().boards)

  def stage(wonder, side, n) when is_integer(n) and n >= 1 do
    case wonder_board(wonder, side) do
      nil -> nil
      board -> Enum.at(board.stages, n - 1)
    end
  end

  defp index do
    case :persistent_term.get(@key, nil) do
      nil ->
        index = build(Core.game_settings())
        :persistent_term.put(@key, index)
        index

      index ->
        index
    end
  end

  defp build(settings) do
    %{
      cards: settings.cards |> Enum.reverse() |> Map.new(&{&1.name, &1}),
      boards:
        for(
          wonder <- settings.wonders,
          board <- wonder.boards,
          into: %{},
          do: {{wonder.name, board.side}, board}
        )
    }
  end
end
