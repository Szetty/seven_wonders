defmodule Mix.Tasks.Helios.Ai.Probe do
  @shortdoc "Checks which configured AI models are ready (keys, credit, model listing)"
  @moduledoc """
  Runs the readiness probes against the configured providers with the real
  API keys from the environment and prints each model's status and every
  provider problem. For real decisions, latency and token counts run the
  live tests: `mix test --only live_ai`.

      JEV_API_KEY=… OPENROUTER_API_KEY=… mix helios.ai.probe
  """
  use Mix.Task

  alias Helios.AI.{Models, Readiness}

  @impl true
  def run(_args) do
    Mix.Task.run("app.start")
    shell = Mix.shell()
    Enum.each(report(), &shell.info/1)
  end

  @doc "The lines `run/1` prints."
  @spec report() :: [String.t()]
  def report do
    %{ready: ready, reasons: reasons} = Readiness.probe_now()

    models =
      for model <- Models.all(),
          do: "#{model.id}: #{if MapSet.member?(ready, model.id), do: "ready", else: "not ready"}"

    problems = for {provider, reason} <- reasons, do: "#{provider}: #{inspect(reason)}"

    models ++
      problems ++ ["Run `mix test --only live_ai` for real decisions, latency and token counts."]
  end
end
