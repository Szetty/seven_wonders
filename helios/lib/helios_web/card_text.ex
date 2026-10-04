defmodule HeliosWeb.CardText do
  @moduledoc """
  English for the engine's effect specs and costs (data from `Helios.Catalogue`).
  `neighbours` optionally names the card owner's neighbours
  (`%{west: "Bob", east: "Cid"}`); without it the text says "your west neighbour".
  """

  @colours %{
    raw_material: "brown",
    manufactured_good: "grey",
    civilian: "blue",
    commercial: "yellow",
    military: "red",
    scientific: "green",
    guild: "purple"
  }

  def effects(specs, neighbours \\ %{}), do: Enum.map(specs, &effect(&1, neighbours))

  def effect({:produces, %{resources: resources, choice: false}}, _n),
    do: "Produces " <> counted(resources)

  def effect({:produces, %{resources: resources, choice: true, tradable: true}}, _n),
    do: "Produces #{alternatives(resources, &to_string/1)} (one per turn)"

  def effect({:produces, %{resources: resources, choice: true, tradable: false}}, _n),
    do: "Produces #{alternatives(resources, &to_string/1)} (one per turn, only for you)"

  def effect({:points, %{points: points}}, _n), do: "Worth #{points(points)}"
  def effect({:military, n}, _n), do: "+#{n} military strength"

  def effect({:science, %{symbols: [symbol], choice: false}}, _n),
    do: "Science symbol: #{symbol}"

  def effect({:science, %{symbols: symbols, choice: true}}, _n),
    do: "One science symbol of your choice: #{alternatives(symbols, &to_string/1)}"

  def effect({:coins, n}, _n), do: "Gain #{coins(n)} (once, when built)"

  def effect({:trade_discount, %{directions: directions, resources: resources}}, n),
    do: "Buy #{alternatives(resources, &to_string/1)} from #{whom(directions, n)} for 1 coin"

  def effect({:coins_per_card, %{directions: d, categories: c, coins: coins}}, n),
    do: "Gain #{coins(coins)} per #{colours(c)} card #{where(d, n)} (once, when built)"

  def effect({:points_per_card, %{directions: d, categories: c, points: points}}, n),
    do: "#{points(points)} per #{colours(c)} card #{where(d, n)} (at game end)"

  def effect({:coins_per_wonder_stage, %{directions: d, coins: coins}}, n),
    do: "Gain #{coins(coins)} per wonder stage built #{where(d, n)} (once, when built)"

  def effect({:points_per_wonder_stage, %{directions: d, points: points}}, n),
    do: "#{points(points)} per wonder stage built #{where(d, n)} (at game end)"

  def effect({:points_per_defeat, %{directions: d, points: points}}, n),
    do: "#{points(points)} per defeat token #{where(d, n)} (at game end)"

  def effect(:play_last_card, _n), do: "Play the last card of each age instead of discarding it"
  def effect(:build_free_once_per_age, _n), do: "Once per age, build a structure for free"
  def effect(:copy_guild, _n), do: "At game end, copy a guild built by a neighbour"
  def effect(:build_from_discard, _n), do: "Build a card from the discard pile for free"

  def cost(%{coins: 0, resources: []}), do: "Free"

  def cost(%{coins: coins, resources: resources}) do
    parts = Enum.map(resources, fn {resource, count} -> "#{count} #{resource}" end)
    parts = if coins > 0, do: parts ++ [coins(coins)], else: parts
    Enum.join(parts, ", ")
  end

  defp counted(resources) do
    resources
    |> Enum.chunk_by(& &1)
    |> Enum.map_join(" and ", fn
      [one] -> to_string(one)
      [first | _] = many -> "#{length(many)} #{first}"
    end)
  end

  defp alternatives([only], fun), do: fun.(only)

  defp alternatives(items, fun) do
    {init, [last]} = Enum.split(items, -1)
    Enum.map_join(init, ", ", fun) <> " or " <> fun.(last)
  end

  defp colours(categories), do: alternatives(categories, &Map.fetch!(@colours, &1))

  defp points(1), do: "1 victory point"
  defp points(n), do: "#{n} victory points"
  defp coins(1), do: "1 coin"
  defp coins(n), do: "#{n} coins"

  defp neighbour(:west, n), do: Map.get(n, :west, "your west neighbour")
  defp neighbour(:east, n), do: Map.get(n, :east, "your east neighbour")

  defp whom(directions, n) do
    case Enum.sort(directions) do
      [:east, :west] -> "both neighbours"
      [one] -> neighbour(one, n)
    end
  end

  defp where(directions, n) do
    case Enum.sort(directions) do
      [:own] -> "in your city"
      [:east, :west] -> "in your neighbours' cities"
      [:east, :own, :west] -> "in your city and your neighbours' cities"
      [one] -> "in #{neighbour(one, n)}'s city"
    end
  end
end
