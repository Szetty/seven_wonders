defmodule Helios.AI.Provider.OpenRouter do
  @moduledoc """
  OpenRouter chat completions. A decision is a system message (the primer)
  plus one user message (`Prompt.user_message/2`: the state as TOON or JSON,
  per the model's `format`, and `id: label` move lines); the
  `response_format` is a strict JSON schema whose `move` is an enum of the
  offered ids, `max_tokens` is tiny and `provider.require_parameters` keeps
  routing on backends that honour structured output. Reasoning is off unless
  the model entry sets `reasoning: true`.

  Response bodies are decoded here, not by Req (`decode_body: false`), so a
  malformed 200 is one `{:error, :invalid_move}` from `decide/3` instead of a
  decode failure `Helios.AI.HTTP` would class as a transport error and retry.

  Readiness: `GET /key` (valid key, credit left), then `GET /models`: a model
  is ready when it is listed with `structured_outputs` or `response_format`
  among its supported parameters.
  """
  @behaviour Helios.AI.Provider

  require Logger

  alias Helios.AI.{HTTP, Prompt}

  @default_base_url "https://openrouter.ai/api/v1"
  @probe_budget_ms 10_000

  @impl true
  def decide(model, prompt, deadline) do
    with {:ok, key} <- api_key() do
      case HTTP.request(
             req(key, method: :post, url: "/chat/completions", json: body(model, prompt)),
             deadline
           ) do
        {:ok, %Req.Response{status: 200} = response} ->
          case json(response) do
            {:ok, decoded} -> answer(decoded, prompt)
            {:error, _malformed} -> {:error, :invalid_move}
          end

        {:ok, %Req.Response{status: status}} ->
          {:error, {:http, status}}

        {:error, reason} ->
          {:error, reason}
      end
    end
  end

  @impl true
  def probe(models) do
    deadline = HTTP.now() + @probe_budget_ms

    with {:ok, key} <- api_key(),
         :ok <- check_key(key, deadline),
         {:ok, supported} <- structured_models(key, deadline) do
      {ready, missing} = Enum.split_with(models, &MapSet.member?(supported, &1.model))

      for model <- missing,
          do:
            Logger.warning(
              "OpenRouter model #{model.model} is unknown or lacks structured outputs"
            )

      {:ok, Enum.map(ready, & &1.id)}
    end
  end

  defp body(model, prompt) do
    ids = Enum.map(prompt.moves, & &1.id)

    %{
      model: model.model,
      messages: [
        %{role: "system", content: prompt.primer},
        %{role: "user", content: Prompt.user_message(prompt, Map.get(model, :format, :toon))}
      ],
      response_format: %{
        type: "json_schema",
        json_schema: %{
          name: "move",
          strict: true,
          schema: %{
            type: "object",
            properties: %{move: %{type: "string", enum: ids}},
            required: ["move"],
            additionalProperties: false
          }
        }
      },
      max_tokens: Map.get(model, :max_tokens, 16),
      temperature: 0.2,
      provider: %{require_parameters: true}
    }
    |> then(fn body ->
      if Map.get(model, :reasoning, false),
        do: Map.put(body, :reasoning, %{effort: "low", exclude: true}),
        else: body
    end)
  end

  defp answer(%{"choices" => [%{"message" => %{"content" => content}} | _]} = body, prompt)
       when is_binary(content) do
    with {:ok, %{"move" => id}} <- Jason.decode(content),
         true <- Enum.any?(prompt.moves, &(&1.id == id)) do
      {:ok, id, usage(body)}
    else
      _invalid -> {:error, :invalid_move}
    end
  end

  defp answer(_body, _prompt), do: {:error, :invalid_move}

  defp usage(body) do
    %{
      input_tokens: get_in(body, ["usage", "prompt_tokens"]) || 0,
      output_tokens: get_in(body, ["usage", "completion_tokens"]) || 0
    }
  end

  defp check_key(key, deadline) do
    case HTTP.request(req(key, method: :get, url: "/key"), deadline) do
      {:ok, %Req.Response{status: 200} = response} ->
        case json(response) do
          {:ok, %{"data" => %{"limit_remaining" => left}}}
          when is_number(left) and left <= 0 ->
            {:error, :out_of_credit}

          _ok_or_unexpected ->
            :ok
        end

      {:ok, %Req.Response{status: status}} ->
        {:error, {:http, status}}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp structured_models(key, deadline) do
    case HTTP.request(req(key, method: :get, url: "/models"), deadline) do
      {:ok, %Req.Response{status: 200} = response} ->
        case json(response) do
          {:ok, %{"data" => models}} when is_list(models) ->
            {:ok,
             for(
               %{"id" => id} = model <- models,
               params = model["supported_parameters"] || [],
               "structured_outputs" in params or "response_format" in params,
               into: MapSet.new(),
               do: id
             )}

          _unexpected ->
            {:error, {:http, 200}}
        end

      {:ok, %Req.Response{status: status}} ->
        {:error, {:http, status}}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp json(%Req.Response{body: body}) when is_binary(body), do: Jason.decode(body)
  defp json(_response), do: {:error, :invalid_body}

  defp req(key, options) do
    [
      base_url: Keyword.get(config(), :base_url, @default_base_url),
      auth: {:bearer, key},
      headers: [{"x-title", "Seven Wonders"}]
    ]
    |> Keyword.merge(decode_body: false)
    |> Keyword.merge(options)
    |> Keyword.merge(Keyword.get(config(), :req_options, []))
  end

  defp api_key do
    case Keyword.get(config(), :api_key) do
      key when is_binary(key) and key != "" -> {:ok, key}
      _missing -> {:error, :no_key}
    end
  end

  defp config, do: Application.get_env(:helios, __MODULE__, [])
end
