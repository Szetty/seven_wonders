import Config

# Environment for the Playwright suite in ../e2e: a real server with
# production-like settings (no code reloading, watchers or live reload)
# against a throwaway database. Started by e2e/playwright.config.ts with
#
#     MIX_ENV=e2e mix do assets.build + ecto.reset + phx.server
#
# The HTTP port comes from PORT (default 4004, see config/runtime.exs).

config :helios, Helios.Repo,
  database: Path.expand("../helios_e2e.db", __DIR__),
  pool_size: 5

config :helios, HeliosWeb.Endpoint,
  http: [ip: {127, 0, 0, 1}],
  check_origin: false,
  code_reloader: false,
  debug_errors: false,
  server: true,
  secret_key_base:
    "e2e-only-secret-key-base-not-for-production-0123456789abcdefghijklmnopqrstuvwxyz"

config :helios, access_token: "e2e"

config :logger, level: :warning
