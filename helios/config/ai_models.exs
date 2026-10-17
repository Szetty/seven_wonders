import Config

# The AI model allowlist table owners pick from (Helios.AI.Models). Kept in
# its own file so the live provider tests (`mix test --only live_ai`) read the
# real list even though config/test.exs overrides it with fake models.
config :helios, Helios.AI,
  models: [
    %{id: "jev-1.13", label: "Jev 1.13", provider: :jev, model: "jev-1.13.0", deadline_ms: 5_000}
    # OpenRouter entries agreed with the user, e.g.
    # %{id: "or-<slug>", label: "<label>", provider: :openrouter, model: "<vendor>/<model>",
    #   deadline_ms: 10_000, format: :toon}
  ]
