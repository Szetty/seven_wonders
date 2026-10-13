defmodule Helios.CatalogueTest do
  use ExUnit.Case, async: true

  alias Helios.Catalogue

  test "cards are looked up by name; duplicate names return one entry" do
    assert %{name: "Altar", age: 1, chains_to: ["Temple"]} = Catalogue.card("Altar")
    assert %{name: "Loom", age: 1} = Catalogue.card("Loom")
    assert Catalogue.card("Not A Card") == nil
    assert length(Catalogue.cards()) == length(Enum.uniq_by(Catalogue.cards(), & &1.name))
  end

  test "wonder names in engine order" do
    names = Catalogue.wonder_names()
    assert length(names) == 7
    assert Enum.sort(names) == Enum.sort(Helios.SampleViews.wonder_names())
    assert names == Enum.map(Helios.Core.game_settings().wonders, & &1.name)
  end

  test "wonder boards and stages" do
    assert %{side: :b, stages: [_, _, _, _]} = Catalogue.wonder_board("Gizah", :b)
    assert %{effects: [{:points, %{points: 5}}]} = Catalogue.stage("Gizah", :a, 2)
    assert Catalogue.stage("Gizah", :a, 9) == nil
    assert Catalogue.wonder_board("Atlantis", :a) == nil
  end
end
