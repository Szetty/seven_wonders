defmodule HeliosWeb.GameAssetsTest do
  use ExUnit.Case, async: true

  alias HeliosWeb.GameAssets

  defp exists?(path), do: File.exists?(Path.join(Application.app_dir(:helios, "priv/static"), path))

  test "card paths lowercase the engine name and drop spaces" do
    assert GameAssets.card_path("Chamber of commerce") == "/images/cards/chamberofcommerce.png"
    assert GameAssets.card_path("Lumber Yard") == "/images/cards/lumberyard.png"
    assert GameAssets.card_back_path(2) == "/images/cards/age2.png"
  end

  test "wonder paths strip diacritics and special-case Halikarnassós" do
    assert GameAssets.wonder_path("Halikarnassós", :b) == "/images/wonders/halikarnassusB.png"
    assert GameAssets.wonder_path("Éphesos", :a) == "/images/wonders/ephesosA.png"
    assert GameAssets.wonder_path("Olympía", :a) == "/images/wonders/olympiaA.png"
    assert GameAssets.wonder_path("Rhódos", :b) == "/images/wonders/rhodosB.png"
  end

  test "token paths map engine resources to the legacy file names" do
    assert GameAssets.token_path(:loom) == "/images/tokens/linen.png"
    assert GameAssets.token_path(:papyrus) == "/images/tokens/paper.png"
    assert GameAssets.token_path(:coin) == "/images/tokens/coin.png"
    assert GameAssets.token_path({:military, -1}) == "/images/tokens/victoryminus1.png"
    assert GameAssets.token_path({:military, 5}) == "/images/tokens/victory5.png"
  end

  test "every card, card back, wonder side and token exists under priv/static" do
    settings = Helios.Core.game_settings()

    for %{name: name} <- settings.cards do
      assert exists?(GameAssets.card_path(name)), "missing card art for #{name}"
    end

    for age <- 1..3, do: assert(exists?(GameAssets.card_back_path(age)))

    for %{name: name, sides: sides} <- settings.wonders, side <- sides do
      assert exists?(GameAssets.wonder_path(name, side)), "missing wonder art for #{name} #{side}"
    end

    tokens =
      [:coin, :pyramid, :wood, :stone, :clay, :ore, :glass, :loom, :papyrus] ++
        Enum.map([1, 3, 5, -1], &{:military, &1})

    for token <- tokens, do: assert(exists?(GameAssets.token_path(token)), "missing #{inspect(token)}")

    assert exists?("/images/paper.jpg")
  end
end
