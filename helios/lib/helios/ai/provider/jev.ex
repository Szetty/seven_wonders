defmodule Helios.AI.Provider.Jev do
  @moduledoc """
  TypeSafe's Jev decision model (`POST /v1/systemone`). A decision is one
  `choice` question: the moves are its criteria (id => label), the payload
  state is the `state`, the primer the instructions. The answer can only be
  one of the offered ids. Priced per input token; output is free.

  The response body is decoded here, not by Req (`decode_body: false`), so a
  malformed 200 is one `{:error, :invalid_move}` instead of a decode failure
  Helios.AI.HTTP would class as a transport error and retry.

  Readiness: `GET /v1/models` with the key; if the API has no listing
  (404), a two-option decision (a few input tokens) proves the key works.
  """
  @behaviour Helios.AI.Provider

  alias Helios.AI.HTTP

  @default_base_url "https://api.typesafe.ai"
  @probe_budget_ms 10_000

  @impl true
  def decide(model, prompt, deadline) do
    with {:ok, key} <- api_key() do
      body = %{
        model: model.model,
        state: prompt.state,
        questions: %{
          move: %{
            type: "choice",
            instructions: prompt.primer,
            criteria: Map.new(prompt.moves, &{&1.id, &1.label})
          }
        }
      }

      case HTTP.request(req(key, method: :post, url: "/v1/systemone", json: body), deadline) do
        {:ok, %Req.Response{status: 200, body: body}} when is_binary(body) ->
          with {:ok, decoded} <- Jason.decode(body),
               {:ok, id} <- choice(decoded),
               true <- Enum.any?(prompt.moves, &(&1.id == id)) do
            {:ok, id, usage(decoded)}
          else
            _not_a_valid_answer -> {:error, :invalid_move}
          end

        {:ok, %Req.Response{status: 200}} ->
          {:error, :invalid_move}

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
    ids = Enum.map(models, & &1.id)

    with {:ok, key} <- api_key() do
      case HTTP.request(req(key, method: :get, url: "/v1/models"), deadline) do
        {:ok, %Req.Response{status: 200}} -> {:ok, ids}
        {:ok, %Req.Response{status: 404}} -> probe_decision(key, hd(models), ids, deadline)
        {:ok, %Req.Response{status: status}} -> {:error, {:http, status}}
        {:error, reason} -> {:error, reason}
      end
    end
  end

  defp probe_decision(key, model, ids, deadline) do
    body = %{
      model: model.model,
      state: "ping",
      questions: %{
        ok: %{type: "choice", instructions: "Answer yes.", criteria: %{yes: nil, no: nil}}
      }
    }

    case HTTP.request(req(key, method: :post, url: "/v1/systemone", json: body), deadline) do
      {:ok, %Req.Response{status: 200}} -> {:ok, ids}
      {:ok, %Req.Response{status: status}} -> {:error, {:http, status}}
      {:error, reason} -> {:error, reason}
    end
  end

  defp choice(%{"answers" => %{"move" => %{"choice" => id}}}) when is_binary(id), do: {:ok, id}
  defp choice(_other), do: :error

  defp usage(body) do
    %{
      input_tokens: get_in(body, ["usage", "input_tokens"]) || 0,
      output_tokens: get_in(body, ["usage", "output_tokens"]) || 0
    }
  end

  defp req(key, options) do
    [base_url: Keyword.get(config(), :base_url, @default_base_url), auth: {:bearer, key}]
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
