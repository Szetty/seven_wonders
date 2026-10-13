defmodule Helios.AI.Provider.FakeTest do
  use ExUnit.Case, async: false

  alias Helios.AI.Models
  alias Helios.AI.Provider.Fake

  @prompt %{
    state: %{},
    primer: "p",
    moves: [
      %{id: "b0", label: "build A: free", card: "A", kind: "build", trade_options: 0},
      %{id: "d0", label: "discard A: +3 coins", card: "A", kind: "discard", trade_options: 0}
    ]
  }

  defp model(id), do: elem(Models.fetch(id), 1)
  defp deadline, do: System.monotonic_time(:millisecond) + 1_000

  test "first / last / error / invalid answer as named" do
    assert {:ok, "b0", %{input_tokens: 0, output_tokens: 0}} =
             Fake.decide(model("fake-first"), @prompt, deadline())

    assert {:ok, "d0", _usage} = Fake.decide(model("fake-last"), @prompt, deadline())
    assert {:error, :fake_error} = Fake.decide(model("fake-error"), @prompt, deadline())
    assert {:ok, "zz", _usage} = Fake.decide(model("fake-invalid"), @prompt, deadline())
  end

  test "timeout never answers" do
    task = Task.async(fn -> Fake.decide(model("fake-timeout"), @prompt, deadline()) end)
    assert Task.yield(task, 200) == nil
    Task.shutdown(task, :brutal_kill)
  end

  test "probe/1 reports every model except \"down\" ones" do
    assert Fake.probe(Models.all()) ==
             {:ok, ~w(fake-first fake-last fake-timeout fake-error fake-invalid)}
  end

  test "a configured listener receives decisions and probes" do
    Application.put_env(:helios, :fake_ai_listener, self())
    on_exit(fn -> Application.delete_env(:helios, :fake_ai_listener) end)

    Fake.decide(model("fake-first"), @prompt, deadline())
    assert_received {:fake_ai_decide, "fake-first", @prompt}

    Fake.probe([model("fake-first")])
    assert_received {:fake_ai_probe, ["fake-first"]}
  end
end
