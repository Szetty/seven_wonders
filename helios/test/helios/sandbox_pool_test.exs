defmodule Helios.SandboxPoolTest do
  use ExUnit.Case, async: true

  # Regression tripwire for the deferred-sandbox SQLITE_BUSY flake (see the
  # root-cause note in config/test.exs): a pool larger than one connection
  # lets two sandbox transactions exist at once, and the second test's first
  # mid-transaction write then fails instantly because SQLite skips the busy
  # handler while a transaction is open.
  test "test repo runs with a single SQLite connection" do
    assert Helios.Repo.config()[:pool_size] == 1,
           "pool_size must stay 1 in the test env; raising it reintroduces " <>
             "the intermittent SQLITE_BUSY fixture failures (config/test.exs)"
  end
end
