defmodule Mix.Tasks.Helios.Ai.ProbeTest do
  use ExUnit.Case, async: false

  alias Mix.Tasks.Helios.Ai.Probe

  test "reports which configured models are ready" do
    lines = Probe.report()
    assert "fake-first: ready" in lines
    assert "fake-down: not ready" in lines
    assert List.last(lines) =~ "mix test --only live_ai"
  end
end
