defmodule Helios.AI.ReadinessTest do
  use ExUnit.Case, async: false

  alias Helios.AI.{Models, Readiness}

  setup do
    Application.put_env(:helios, :fake_ai_listener, self())
    on_exit(fn -> Application.delete_env(:helios, :fake_ai_listener) end)
    :ok
  end

  defp start(opts) do
    name = :"readiness_#{System.unique_integer([:positive])}"

    start_supervised!(
      {Readiness, Keyword.merge([name: name, interval_ms: :timer.hours(1)], opts)}
    )

    # The boot probe runs once; wait for it so tests start from a known state.
    assert_receive {:fake_ai_probe, _ready}
    _ = :sys.get_state(name)
    name
  end

  test "ready models are those the provider's probe returned, in config order" do
    name = start([])

    assert Enum.map(Readiness.ready_models(name), & &1.id) ==
             ~w(fake-first fake-last fake-timeout fake-error fake-invalid)

    assert Readiness.ready?("fake-first", name)
    refute Readiness.ready?("fake-down", name)
    refute Readiness.ready?("not-configured", name)
  end

  test "probe_now/1 probes, replies with the result and broadcasts" do
    name = start([])
    Phoenix.PubSub.subscribe(Helios.PubSub, Readiness.topic())

    assert %{ready: ready, reasons: reasons} = Readiness.probe_now(name)
    assert MapSet.member?(ready, "fake-first")
    assert reasons == %{}
    assert_receive {:fake_ai_probe, _}
    assert_receive {:ai_readiness_changed}
  end

  test "refresh/1 is rate limited by the refresh gap" do
    name = start(refresh_gap_ms: :timer.hours(1))
    Readiness.refresh(name)
    _ = :sys.get_state(name)
    refute_receive {:fake_ai_probe, _}, 100

    open = start(refresh_gap_ms: 0)
    Readiness.refresh(open)
    assert_receive {:fake_ai_probe, _}
  end

  test "an unknown provider in config fails the probe round but keeps the last ready set" do
    name = start([])
    broken = [%{id: "typo", label: "Typo", provider: :nope, model: "x", deadline_ms: 1_000}]
    :sys.replace_state(name, &%{&1 | models: Models.all() ++ broken})

    ExUnit.CaptureLog.capture_log(fn ->
      assert %{ready: ready} = Readiness.probe_now(name)
      assert MapSet.member?(ready, "fake-first")
    end)

    assert Readiness.ready?("fake-first", name)
    refute Readiness.ready?("typo", name)
  end

  test "reads do not wait for a running probe" do
    name = start([])

    :sys.replace_state(
      name,
      &%{&1 | task: %Task{ref: make_ref(), pid: self(), owner: self(), mfa: {:m, :f, 0}}}
    )

    assert Readiness.ready?("fake-first", name)
  end
end
