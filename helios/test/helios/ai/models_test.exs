defmodule Helios.AI.ModelsTest do
  use ExUnit.Case, async: true

  alias Helios.AI.Models

  test "all/0 lists the configured allowlist in config order" do
    assert Enum.map(Models.all(), & &1.id) ==
             ~w(fake-first fake-last fake-timeout fake-error fake-invalid fake-down)
  end

  test "fetch/1 finds a model by id; unknown ids are :error" do
    assert {:ok, %{id: "fake-first", provider: :fake, deadline_ms: 2_000}} =
             Models.fetch("fake-first")

    assert Models.fetch("gone") == :error
    assert Models.fetch(nil) == :error
  end

  test "label/1 falls back to the stored id for models removed from config" do
    assert Models.label("fake-first") == "Fake First"
    assert Models.label("removed-model") == "removed-model"
  end

  test "provider_module/1 maps whitelisted providers only" do
    assert Models.provider_module(%{provider: :fake}) == Helios.AI.Provider.Fake
    assert Models.provider_module(%{provider: :jev}) == Helios.AI.Provider.Jev
    assert Models.provider_module(%{provider: :openrouter}) == Helios.AI.Provider.OpenRouter
    assert_raise KeyError, fn -> Models.provider_module(%{provider: :nope}) end
  end

  test "seat_labels/1 numbers repeated models from #2, in order" do
    assert Models.seat_labels(["fake-first", "fake-last", "fake-first", "gone", "fake-first"]) ==
             ["Fake First", "Fake Last", "Fake First #2", "gone", "Fake First #3"]

    assert Models.seat_labels([]) == []
  end
end
