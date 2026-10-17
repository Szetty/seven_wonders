defmodule Helios.AI.LiveProvidersTest do
  @moduledoc """
  Real calls to every model in config/ai_models.exs. Local only (excluded by
  default):

      JEV_API_KEY=… OPENROUTER_API_KEY=… mix test --only live_ai
      LIVE_AI_MODEL=jev-1.13 LIVE_AI_RUNS=10 mix test --only live_ai

  Per model: the readiness probe (key, credit, listing), then real engine
  positions decided with a #{30_000} ms cap (OpenRouter models in both TOON
  and JSON). Every answer must be an offered id. Prints latency, token and
  deadline figures per model; models without a key are skipped.
  """
  use ExUnit.Case, async: false

  alias Helios.AI.{Models, Prompt}
  alias Helios.{Core, GameDriver}

  @moduletag :live_ai
  @moduletag timeout: :infinity

  @cap_ms 30_000
  @keys %{jev: "JEV_API_KEY", openrouter: "OPENROUTER_API_KEY"}
  @modules %{jev: Helios.AI.Provider.Jev, openrouter: Helios.AI.Provider.OpenRouter}

  # Real keys, no Req.Test plug, for the duration of this module.
  setup_all do
    for {provider, var} <- @keys do
      module = Map.fetch!(@modules, provider)
      original = Application.get_env(:helios, module, [])
      Application.put_env(:helios, module, api_key: System.get_env(var))
      on_exit(fn -> Application.put_env(:helios, module, original) end)
    end

    %{positions: positions()}
  end

  test "allowlisted models answer legal moves; latency and token report", %{positions: positions} do
    models = live_models()
    assert models != [], "no model selected (check LIVE_AI_MODEL and config/ai_models.exs)"

    results = Enum.map(models, &run_model(&1, positions))
    Enum.each(results, &IO.puts(report(&1)))

    for %{model: model, failures: failures} <- results do
      assert failures == [], "#{model.id}: #{inspect(failures, limit: 5)}"
    end
  end

  defp live_models do
    models =
      "../../../config/ai_models.exs"
      |> Path.expand(__DIR__)
      |> Config.Reader.read!()
      |> get_in([:helios, Helios.AI, :models])

    case System.get_env("LIVE_AI_MODEL") do
      nil -> models
      id -> Enum.filter(models, &(&1.id == id))
    end
  end

  defp runs, do: String.to_integer(System.get_env("LIVE_AI_RUNS", "3"))

  defp run_model(model, positions) do
    var = Map.fetch!(@keys, model.provider)

    if System.get_env(var) in [nil, ""] do
      %{model: model, skipped: "#{var} not set", samples: [], failures: []}
    else
      module = Models.provider_module(model)

      case module.probe([model]) do
        {:ok, [_ready]} ->
          sample(model, module, positions)

        {:ok, []} ->
          %{model: model, skipped: nil, samples: [], failures: [:not_listed]}

        {:error, reason} ->
          %{model: model, skipped: nil, samples: [], failures: [{:probe, reason}]}
      end
    end
  end

  defp sample(model, module, positions) do
    formats = if model.provider == :openrouter, do: [:toon, :json], else: [:json]

    samples =
      for {name, view} <- positions, format <- formats, _run <- 1..runs() do
        prompt = Prompt.build(view)
        started = System.monotonic_time(:millisecond)
        result = module.decide(Map.put(model, :format, format), prompt, started + @cap_ms)

        %{
          position: name,
          format: format,
          ms: System.monotonic_time(:millisecond) - started,
          result: result,
          legal?: legal?(result, prompt)
        }
      end

    failures = for s <- samples, !s.legal?, do: {s.position, s.format, s.result}
    %{model: model, skipped: nil, samples: samples, failures: failures}
  end

  defp legal?({:ok, id, _usage}, prompt), do: Enum.any?(prompt.moves, &(&1.id == id))
  defp legal?(_result, _prompt), do: false

  defp report(%{model: model, skipped: reason}) when is_binary(reason),
    do: "#{model.id}: skipped (#{reason})"

  defp report(%{model: model, samples: [], failures: failures}),
    do: "#{model.id}: not ready #{inspect(failures)}"

  defp report(%{model: model, samples: samples, failures: failures}) do
    ms = samples |> Enum.map(& &1.ms) |> Enum.sort()
    p95 = percentile(ms, 95)
    late = Enum.count(ms, &(&1 > model.deadline_ms))

    tokens_in =
      samples
      |> Enum.group_by(& &1.format)
      |> Enum.map_join(" / ", fn {format, group} -> "#{avg(group, :input_tokens)} (#{format})" end)

    """
    #{model.id}  runs=#{length(samples)}  ok=#{length(samples) - length(failures)}  failed=#{length(failures)}
      latency ms: min #{hd(ms)}  p50 #{percentile(ms, 50)}  p95 #{p95}  max #{List.last(ms)}
      tokens in: avg #{tokens_in}  out: avg #{avg(samples, :output_tokens)}
      configured deadline #{model.deadline_ms} ms -> #{late} would have timed out · suggested deadline ≈ #{suggest(p95)} ms (p95 × 1.5)
    """
  end

  defp avg(samples, key) do
    values = for %{result: {:ok, _id, usage}} <- samples, do: Map.get(usage, key, 0)
    if values == [], do: 0, else: div(Enum.sum(values), length(values))
  end

  defp percentile(sorted, p),
    do: Enum.at(sorted, min(length(sorted) - 1, div(p * length(sorted), 100)))

  defp suggest(p95), do: trunc(Float.ceil(p95 * 1.5 / 250) * 250)

  ## Positions: real engine views, deterministic seeds

  defp positions do
    ids = ["p1", "p2", "p3", "p4"]
    {:ok, ref} = Core.new_game(ids, :random, 7)
    {:ok, opening} = Core.view(ref, "p1")

    [
      {"age1-turn1", opening},
      {"age2-turn3",
       reach(ids, &match?(%{phase: %{kind: :choosing_cards, age: 2, turn: 3}}, &1))},
      {"age3-turn5",
       reach(ids, &match?(%{phase: %{kind: :choosing_cards, age: 3, turn: 5}}, &1))},
      {"build-from-discard", discard_turn()}
    ]
  end

  defp reach(ids, stop?) do
    {:ok, ref} = Core.new_game(ids, :random, 7)
    {view_fun, submit_fun} = GameDriver.core_funs(ref)
    strategies = Map.new(ids, &{&1, :build})
    {:reached, _view} = GameDriver.play_until(ids, strategies, view_fun, submit_fun, stop?)
    view_fun.("p1")
  end

  defp discard_turn do
    ids = ["p1", "p2", "p3"]
    wonders = [{"Halikarnassós", :b}, {"Gizah", :a}, {"Rhódos", :a}]
    strategies = %{"p1" => :wonder, "p2" => :build, "p3" => :build}
    stop? = &match?(%{phase: %{kind: :extra_turn, extra_turn_kind: :build_from_discard}}, &1)
    seed = GameDriver.find_seed(ids, wonders, strategies, stop?)
    {:ok, ref} = Core.new_game(ids, {:explicit, wonders}, seed)
    {view_fun, submit_fun} = GameDriver.core_funs(ref)
    {:reached, _view} = GameDriver.play_until(ids, strategies, view_fun, submit_fun, stop?)
    view_fun.("p1")
  end
end
