defmodule Helios.AI.Provider.Fake do
  @moduledoc """
  Test / e2e provider (never configured in prod). The model entry's `model`
  string selects the behaviour: `"first"` / `"last"` answer the first / last
  move, `"timeout"` never answers, `"error"` fails, `"invalid"` answers an id
  that was not offered, `"down"` fails its readiness probe. When
  `config :helios, :fake_ai_listener` holds a pid, it receives
  `{:fake_ai_decide, model_id, prompt}` and `{:fake_ai_probe, ready_ids}`.
  """
  @behaviour Helios.AI.Provider

  @usage %{input_tokens: 0, output_tokens: 0}

  @impl true
  def decide(model, prompt, _deadline) do
    notify({:fake_ai_decide, model.id, prompt})

    case model.model do
      "first" -> {:ok, hd(prompt.moves).id, @usage}
      "last" -> {:ok, List.last(prompt.moves).id, @usage}
      "invalid" -> {:ok, "zz", @usage}
      "error" -> {:error, :fake_error}
      "timeout" -> Process.sleep(:infinity)
    end
  end

  @impl true
  def probe(models) do
    ready = for model <- models, model.model != "down", do: model.id
    notify({:fake_ai_probe, ready})
    {:ok, ready}
  end

  defp notify(message) do
    case Application.get_env(:helios, :fake_ai_listener) do
      pid when is_pid(pid) -> send(pid, message)
      _ -> :ok
    end
  end
end
