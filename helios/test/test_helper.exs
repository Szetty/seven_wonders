ExUnit.start()
# The app's Readiness probes the fake models at boot; wait so every test sees them ready.
Helios.AI.Readiness.probe_now()
Ecto.Adapters.SQL.Sandbox.mode(Helios.Repo, :manual)
