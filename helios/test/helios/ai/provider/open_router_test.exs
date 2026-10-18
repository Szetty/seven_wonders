defmodule Helios.AI.Provider.OpenRouterTest do
  use ExUnit.Case, async: false

  alias Helios.AI.HTTP
  alias Helios.AI.Provider.OpenRouter

  @model %{
    id: "or-small",
    label: "Small",
    provider: :openrouter,
    model: "vendor/small",
    deadline_ms: 8_000
  }
  @prompt %{
    state: %{age: 1},
    primer: "You are an expert player.",
    moves: [
      %{id: "b0", label: "build Altar: free", card: "Altar", kind: "build", trade_options: 0},
      %{
        id: "d0",
        label: "discard Altar: +3 coins",
        card: "Altar",
        kind: "discard",
        trade_options: 0
      }
    ]
  }

  defp capture(response) do
    test = self()

    Req.Test.stub(OpenRouter, fn conn ->
      {:ok, body, conn} = Plug.Conn.read_body(conn)

      send(
        test,
        {:request, conn.method, conn.request_path,
         Plug.Conn.get_req_header(conn, "authorization"), body}
      )

      response.(conn)
    end)
  end

  defp completion(content) do
    fn conn ->
      Req.Test.json(conn, %{
        "choices" => [%{"message" => %{"role" => "assistant", "content" => content}}],
        "usage" => %{"prompt_tokens" => 640, "completion_tokens" => 6}
      })
    end
  end

  test "decide/3 asks for a strict enum of move ids with a tiny token cap" do
    capture(completion(~s({"move":"b0"})))

    assert {:ok, "b0", %{input_tokens: 640, output_tokens: 6}} =
             OpenRouter.decide(@model, @prompt, HTTP.now() + 2_000)

    assert_received {:request, "POST", "/api/v1/chat/completions", ["Bearer test-openrouter-key"],
                     body}

    body = Jason.decode!(body)

    assert body["model"] == "vendor/small"
    assert body["max_tokens"] == 16
    assert body["provider"] == %{"require_parameters" => true}
    refute Map.has_key?(body, "reasoning")

    assert [
             %{"role" => "system", "content" => "You are an expert player."},
             %{"role" => "user", "content" => user}
           ] =
             body["messages"]

    assert user =~ "State (TOON):\nage: 1"
    assert user =~ "b0: build Altar: free"
    assert user =~ "d0: discard Altar: +3 coins"

    assert %{
             "type" => "json_schema",
             "json_schema" => %{
               "strict" => true,
               "schema" => %{
                 "properties" => %{"move" => %{"enum" => ["b0", "d0"]}},
                 "required" => ["move"]
               }
             }
           } = body["response_format"]
  end

  test "format: :json sends compact JSON state" do
    capture(completion(~s({"move":"b0"})))

    assert {:ok, "b0", _} =
             OpenRouter.decide(Map.put(@model, :format, :json), @prompt, HTTP.now() + 2_000)

    assert_received {:request, _, _, _, body}
    [_system, %{"content" => user}] = Jason.decode!(body)["messages"]
    assert user =~ ~s|State (JSON):\n{"age":1}|
  end

  test "reasoning models opt in explicitly" do
    capture(completion(~s({"move":"d0"})))
    model = Map.merge(@model, %{reasoning: true, max_tokens: 400})
    assert {:ok, "d0", _} = OpenRouter.decide(model, @prompt, HTTP.now() + 2_000)
    assert_received {:request, _, _, _, body}

    assert %{"max_tokens" => 400, "reasoning" => %{"effort" => "low", "exclude" => true}} =
             Jason.decode!(body)
  end

  test "unparseable or unknown answers are :invalid_move" do
    for content <- ["b0", ~s({"move":"x9"}), ~s({"other":1}), ""] do
      capture(completion(content))

      assert {:error, :invalid_move} = OpenRouter.decide(@model, @prompt, HTTP.now() + 2_000),
             content
    end
  end

  test "HTTP errors are returned" do
    capture(fn conn -> conn |> Plug.Conn.put_status(402) |> Req.Test.json(%{}) end)
    assert {:error, {:http, 402}} = OpenRouter.decide(@model, @prompt, HTTP.now() + 2_000)
  end

  test "401 and 403 are single-request errors" do
    for status <- [401, 403] do
      capture(fn conn -> conn |> Plug.Conn.put_status(status) |> Req.Test.json(%{}) end)
      assert {:error, {:http, ^status}} = OpenRouter.decide(@model, @prompt, HTTP.now() + 2_000)
      assert_received {:request, _, _, _, _}
      refute_received {:request, _, _, _, _}
    end
  end

  test "an unparseable 200 body is :invalid_move without retry" do
    capture(fn conn ->
      conn
      |> Plug.Conn.put_resp_content_type("application/json")
      |> Plug.Conn.send_resp(200, "{not json")
    end)

    assert {:error, :invalid_move} = OpenRouter.decide(@model, @prompt, HTTP.now() + 2_000)
    assert_received {:request, _, _, _, _}
    refute_received {:request, _, _, _, _}
  end

  describe "probe/1" do
    defp catalogue(models, key \\ %{"limit_remaining" => nil}) do
      capture(fn
        %{request_path: "/api/v1/key"} = conn -> Req.Test.json(conn, %{"data" => key})
        %{request_path: "/api/v1/models"} = conn -> Req.Test.json(conn, %{"data" => models})
      end)
    end

    test "models that are listed and support structured outputs are ready" do
      catalogue([
        %{"id" => "vendor/small", "supported_parameters" => ["max_tokens", "structured_outputs"]},
        %{"id" => "vendor/plain", "supported_parameters" => ["max_tokens"]}
      ])

      plain = %{@model | id: "or-plain", model: "vendor/plain"}
      gone = %{@model | id: "or-gone", model: "vendor/gone"}

      ExUnit.CaptureLog.capture_log(fn ->
        assert {:ok, ["or-small"]} = OpenRouter.probe([@model, plain, gone])
      end)
    end

    test "an exhausted key is not ready" do
      catalogue([], %{"limit_remaining" => 0})
      assert {:error, :out_of_credit} = OpenRouter.probe([@model])
    end

    test "a rejected key is not ready" do
      capture(fn conn -> conn |> Plug.Conn.put_status(401) |> Req.Test.json(%{}) end)
      assert {:error, {:http, 401}} = OpenRouter.probe([@model])
    end
  end
end
