defmodule HeliosWeb.GameAssets do
  @moduledoc "Maps engine card, wonder and token names to static image paths under priv/static/images."

  @resource_tokens %{
    wood: "wood",
    stone: "stone",
    clay: "clay",
    ore: "ore",
    glass: "glass",
    loom: "linen",
    papyrus: "paper"
  }

  @spec slug(String.t()) :: String.t()
  def slug(name), do: name |> String.downcase() |> String.replace(" ", "")

  def card_path(name), do: static("/images/cards/#{slug(name)}.png")

  def card_back_path(age) when age in 1..3, do: static("/images/cards/age#{age}.png")

  @doc "ASCII city name of a wonder, as used by its image files and DOM ids."
  @spec wonder_slug(String.t()) :: String.t()
  def wonder_slug(wonder) do
    city =
      wonder
      |> :unicode.characters_to_nfd_binary()
      |> String.replace(~r/\p{Mn}/u, "")
      |> String.downcase()

    if city == "halikarnassos", do: "halikarnassus", else: city
  end

  def wonder_path(wonder, side) when side in [:a, :b] do
    side_letter = side |> Atom.to_string() |> String.upcase()
    static("/images/wonders/#{wonder_slug(wonder)}#{side_letter}.png")
  end

  def token_path(:coin), do: static("/images/tokens/coin.png")
  def token_path(:check), do: static("/images/tokens/check.png")
  def token_path(:pyramid), do: static("/images/tokens/pyramid.png")

  def token_path({:military, value}) when value in [1, 3, 5],
    do: static("/images/tokens/victory#{value}.png")

  def token_path({:military, -1}), do: static("/images/tokens/victoryminus1.png")

  def token_path(resource) when is_map_key(@resource_tokens, resource) do
    static("/images/tokens/#{Map.fetch!(@resource_tokens, resource)}.png")
  end

  defp static(path), do: HeliosWeb.Endpoint.static_path(path)
end
