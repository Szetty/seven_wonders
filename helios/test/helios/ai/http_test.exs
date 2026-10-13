defmodule Helios.AI.HTTPTest do
  use ExUnit.Case, async: true

  alias Helios.AI.HTTP

  @options [method: :get, url: "http://ai.test/x", plug: {Req.Test, __MODULE__}]

  # Answers `statuses` in order (the last one repeats) and counts calls.
  defp stub(statuses) do
    counter = :counters.new(1, [])

    Req.Test.stub(__MODULE__, fn conn ->
      :counters.add(counter, 1, 1)
      n = :counters.get(counter, 1)

      case Enum.at(statuses, n - 1, List.last(statuses)) do
        :transport -> Req.Test.transport_error(conn, :econnrefused)
        status -> conn |> Plug.Conn.put_status(status) |> Req.Test.json(%{"n" => n})
      end
    end)

    fn -> :counters.get(counter, 1) end
  end

  test "a 429 is retried and the success is returned" do
    calls = stub([429, 200])

    assert {:ok, %Req.Response{status: 200, body: %{"n" => 2}}} =
             HTTP.request(@options, HTTP.now() + 2_000)

    assert calls.() == 2
  end

  test "transport errors and 5xx are retried" do
    calls = stub([:transport, 503, 200])
    assert {:ok, %Req.Response{status: 200}} = HTTP.request(@options, HTTP.now() + 3_000)
    assert calls.() == 3
  end

  test "401 is returned as is, never retried" do
    calls = stub([401])
    assert {:ok, %Req.Response{status: 401}} = HTTP.request(@options, HTTP.now() + 2_000)
    assert calls.() == 1
  end

  test "repeated 529s stop before the deadline" do
    stub([529])
    started = HTTP.now()
    assert {:error, {:http, 529}} = HTTP.request(@options, started + 700)
    assert HTTP.now() - started < 700
  end

  test "no attempt when the budget is already too small" do
    calls = stub([200])
    assert {:error, :deadline} = HTTP.request(@options, HTTP.now() + 100)
    assert calls.() == 0
  end
end
