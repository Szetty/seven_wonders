# :live_ai tests call real providers with real keys; run them locally with
# `JEV_API_KEY=… OPENROUTER_API_KEY=… mix test --only live_ai`.
ExUnit.start(exclude: [:live_ai])
# The app's Readiness probes the fake models at boot; wait so every test sees them ready.
Helios.AI.Readiness.probe_now()
Ecto.Adapters.SQL.Sandbox.mode(Helios.Repo, :manual)
