defmodule Helios.AI.Provider.JevTest do
  use ExUnit.Case, async: false

  alias Helios.AI.HTTP
  alias Helios.AI.Provider.Jev

  @model %{
    id: "jev-1.13",
    label: "Jev 1.13",
    provider: :jev,
    model: "jev-1.13.0",
    deadline_ms: 5_000
  }
  @prompt %{
    state: %{age: 1, turn: 1},
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

    Req.Test.stub(Jev, fn conn ->
      {:ok, body, conn} = Plug.Conn.read_body(conn)

      send(
        test,
        {:request, conn.method, conn.request_path,
         Plug.Conn.get_req_header(conn, "authorization"), body}
      )

      response.(conn)
    end)
  end

  defp answer(choice) do
    fn conn ->
      Req.Test.json(conn, %{
        "model" => "jev-1.13.0",
        "answers" => %{"move" => %{"type" => "choice", "choice" => choice, "confidence" => 0.9}},
        "usage" => %{"input_tokens" => 812, "output_tokens" => 3}
      })
    end
  end

  test "decide/3 sends one choice question with the moves as criteria" do
    capture(answer("d0"))

    assert {:ok, "d0", %{input_tokens: 812, output_tokens: 3}} =
             Jev.decide(@model, @prompt, HTTP.now() + 2_000)

    assert_received {:request, "POST", "/v1/systemone", ["Bearer test-jev-key"], body}

    assert Jason.decode!(body) == %{
             "model" => "jev-1.13.0",
             "state" => %{"age" => 1, "turn" => 1},
             "questions" => %{
               "move" => %{
                 "type" => "choice",
                 "instructions" => "You are an expert player.",
                 "criteria" => %{"b0" => "build Altar: free", "d0" => "discard Altar: +3 coins"}
               }
             }
           }
  end

  test "an id that was not offered is :invalid_move" do
    capture(answer("x9"))
    assert {:error, :invalid_move} = Jev.decide(@model, @prompt, HTTP.now() + 2_000)
  end

  test "errors are returned, 401 without retry" do
    capture(fn conn -> conn |> Plug.Conn.put_status(401) |> Req.Test.json(%{}) end)
    assert {:error, {:http, 401}} = Jev.decide(@model, @prompt, HTTP.now() + 2_000)
    assert_received {:request, _, _, _, _}
    refute_received {:request, _, _, _, _}
  end

  test "an unparseable 200 body is :invalid_move without retry" do
    capture(fn conn ->
      conn
      |> Plug.Conn.put_resp_content_type("application/json")
      |> Plug.Conn.send_resp(200, "{not json")
    end)

    assert {:error, :invalid_move} = Jev.decide(@model, @prompt, HTTP.now() + 2_000)
    assert_received {:request, _, _, _, _}
    refute_received {:request, _, _, _, _}
  end

  test "no key: not ready, no request" do
    original = Application.get_env(:helios, Jev)
    Application.put_env(:helios, Jev, Keyword.put(original, :api_key, nil))
    on_exit(fn -> Application.put_env(:helios, Jev, original) end)

    assert {:error, :no_key} = Jev.decide(@model, @prompt, HTTP.now() + 2_000)
    assert {:error, :no_key} = Jev.probe([@model])
  end

  describe "probe/1" do
    test "a listing answer makes every Jev model ready" do
      capture(fn conn -> Req.Test.json(conn, %{"data" => [%{"id" => "jev-1.13.0"}]}) end)
      assert {:ok, ["jev-1.13"]} = Jev.probe([@model])
      assert_received {:request, "GET", "/v1/models", ["Bearer test-jev-key"], _}
    end

    test "without a listing endpoint it falls back to a tiny decision" do
      capture(fn
        %{request_path: "/v1/models"} = conn ->
          conn |> Plug.Conn.put_status(404) |> Req.Test.json(%{})

        conn ->
          Req.Test.json(conn, %{"answers" => %{"ok" => %{"choice" => "yes"}}, "usage" => %{}})
      end)

      assert {:ok, ["jev-1.13"]} = Jev.probe([@model])
      assert_received {:request, "GET", "/v1/models", _, _}
      assert_received {:request, "POST", "/v1/systemone", _, _}
    end

    test "a rejected key is not ready" do
      capture(fn conn -> conn |> Plug.Conn.put_status(401) |> Req.Test.json(%{}) end)
      assert {:error, {:http, 401}} = Jev.probe([@model])
    end
  end
end
