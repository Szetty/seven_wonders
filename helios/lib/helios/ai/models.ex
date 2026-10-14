defmodule Helios.AI.Models do
  @moduledoc """
  The allowlist of AI models table owners may seat
  (`config :helios, Helios.AI, models: [...]`). An entry is
  `%{id, label, provider, model, deadline_ms}`; OpenRouter entries may add
  `reasoning: true` and `max_tokens`. `id` is the stable key stored on seats:
  a model removed from config keeps its id as label and plays random moves.
  """

  @providers %{
    jev: Helios.AI.Provider.Jev,
    openrouter: Helios.AI.Provider.OpenRouter,
    fake: Helios.AI.Provider.Fake
  }

  @type provider :: :jev | :openrouter | :fake

  @type model :: %{
          required(:id) => String.t(),
          required(:label) => String.t(),
          required(:provider) => provider(),
          required(:model) => String.t(),
          required(:deadline_ms) => pos_integer(),
          optional(atom()) => term()
        }

  @spec config() :: keyword()
  def config, do: Application.get_env(:helios, Helios.AI, [])

  @spec all() :: [model()]
  def all, do: Keyword.get(config(), :models, [])

  @spec fetch(term()) :: {:ok, model()} | :error
  def fetch(id) do
    case Enum.find(all(), &(&1.id == id)) do
      nil -> :error
      model -> {:ok, model}
    end
  end

  @spec label(String.t()) :: String.t()
  def label(id) do
    case fetch(id) do
      {:ok, model} -> model.label
      :error -> id
    end
  end

  @doc "The adapter module for a model's provider (whitelisted; raises on unknown providers)."
  @spec provider_module(%{provider: provider()}) :: module()
  def provider_module(%{provider: provider}), do: Map.fetch!(@providers, provider)

  @doc "Display names for AI seats in seat order: the label, then `#2`, `#3`… for repeats."
  @spec seat_labels([String.t()]) :: [String.t()]
  def seat_labels(model_ids) do
    {labels, _seen} =
      Enum.map_reduce(model_ids, %{}, fn id, seen ->
        n = Map.get(seen, id, 0) + 1
        label = if n == 1, do: label(id), else: "#{label(id)} ##{n}"
        {label, Map.put(seen, id, n)}
      end)

    labels
  end
end
