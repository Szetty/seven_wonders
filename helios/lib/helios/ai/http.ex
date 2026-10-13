defmodule Helios.AI.HTTP do
  @moduledoc """
  One provider request under an absolute deadline (monotonic milliseconds).
  429, 529, other 5xx and transport errors are retried with short backoff
  while at least #{300} ms remain; every attempt's `receive_timeout` is the
  remaining budget, so the call never outlives the deadline by more than
  connection teardown. Other responses are returned unchanged.
  """

  @backoff_ms [100, 250, 500]
  @min_attempt_ms 300

  @spec now() :: integer()
  def now, do: System.monotonic_time(:millisecond)

  @spec request(keyword(), integer()) ::
          {:ok, Req.Response.t()}
          | {:error, :deadline | {:http, pos_integer()} | {:transport, term()}}
  def request(options, deadline), do: attempt(options, deadline, @backoff_ms, :deadline)

  defp attempt(options, deadline, backoff, last_error) do
    remaining = deadline - now()

    if remaining < @min_attempt_ms do
      {:error, last_error}
    else
      options
      |> Keyword.merge(
        retry: false,
        receive_timeout: remaining,
        connect_options: [timeout: remaining]
      )
      |> Req.request()
      |> handle(options, deadline, backoff)
    end
  end

  defp handle({:ok, %Req.Response{status: status} = response}, options, deadline, backoff) do
    if status == 429 or status >= 500,
      do: retry(options, deadline, backoff, {:http, status}),
      else: {:ok, response}
  end

  defp handle({:error, exception}, options, deadline, backoff),
    do: retry(options, deadline, backoff, {:transport, exception})

  defp retry(_options, _deadline, [], reason), do: {:error, reason}

  defp retry(options, deadline, [delay | rest], reason) do
    if deadline - now() - delay < @min_attempt_ms do
      {:error, reason}
    else
      Process.sleep(delay)
      attempt(options, deadline, rest, reason)
    end
  end
end
