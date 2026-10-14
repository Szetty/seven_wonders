import Config

# Configure your database
#
# The MIX_TEST_PARTITION environment variable can be used
# to provide built-in test partitioning in CI environment.
# Run `mix help test` for more information.
config :helios, Helios.Repo,
  database: Path.expand("../helios_test.db", __DIR__),
  pool_size: 5,
  pool: Ecto.Adapters.SQL.Sandbox,
  # SQLite WAL allows one writer at a time; sandbox checkouts and app
  # processes can hold the write lock while another connection tears down.
  # The exqlite default of 2000ms was too short for CI (SQLITE_BUSY).
  busy_timeout: 10_000

# We don't run a server during test. If one is required,
# you can enable the server option below.
config :helios, HeliosWeb.Endpoint,
  http: [ip: {127, 0, 0, 1}, port: 4002],
  secret_key_base: "Ek3fV3yJ6n69moyY9KT09gXlWKAjEIbqm9OwVfU0XRFwszJ5ZGawen4fP+j1UoUk",
  server: false

# Print only warnings and errors during test
config :logger, level: :warning

# Initialize plugs at runtime for faster test compilation
config :phoenix, :plug_init_mode, :runtime

# Enable helpful, but potentially expensive runtime checks
config :phoenix_live_view,
  enable_expensive_runtime_checks: true

# Sort query params output of verified routes for robust url comparisons
config :phoenix,
  sort_verified_routes_query_params: true

config :helios, access_token: "test"

# Fake AI models (Helios.AI.Provider.Fake); `model` selects the behaviour.
config :helios, Helios.AI,
  probe_interval_ms: :timer.hours(1),
  models: [
    %{id: "fake-first", label: "Fake First", provider: :fake, model: "first", deadline_ms: 2_000},
    %{id: "fake-last", label: "Fake Last", provider: :fake, model: "last", deadline_ms: 2_000},
    %{
      id: "fake-timeout",
      label: "Fake Slow",
      provider: :fake,
      model: "timeout",
      deadline_ms: 100
    },
    %{id: "fake-error", label: "Fake Error", provider: :fake, model: "error", deadline_ms: 2_000},
    %{
      id: "fake-invalid",
      label: "Fake Invalid",
      provider: :fake,
      model: "invalid",
      deadline_ms: 2_000
    },
    %{id: "fake-down", label: "Fake Down", provider: :fake, model: "down", deadline_ms: 2_000}
  ]

config :helios, Helios.AI.Provider.Jev,
  api_key: "test-jev-key",
  req_options: [plug: {Req.Test, Helios.AI.Provider.Jev}]
