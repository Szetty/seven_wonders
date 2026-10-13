# AI Players — Phase A: AI Foundation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Helios knows the AI model allowlist, has a provider behaviour with a fake test provider, and keeps a cached, regularly refreshed set of *ready* models that later phases use to fill the lobby dropdown.

**Architecture:** `Helios.AI.Models` reads `config :helios, Helios.AI, models: [...]` and maps each provider atom to an adapter module. `Helios.AI.Provider` is the behaviour every adapter implements (`decide/3`, `probe/1`). `Helios.AI.Provider.Fake` is driven by the model entry's `model` string. `Helios.AI.Readiness` is a GenServer that probes every provider in a `Task.Supervisor` task, at boot, on a timer and on rate-limited `refresh/0`, and serves `ready?/1` / `ready_models/0` from its state. It broadcasts `{:ai_readiness_changed}` on `"ai:readiness"`.

**Tech Stack:** Elixir 1.18 / OTP, Phoenix PubSub, `Req` (added here, used from phase B), ExUnit.

**Spec:** `docs/superpowers/specs/2026-10-13-ai-players/2026-10-13-ai-players-design.md` (sections "Configuration" and "Provider readiness"). Cross-phase contract: `docs/superpowers/plans/2026-10-13-ai-players/README.md`.

## Global Constraints

- Read `helios/AGENTS.md` before touching `helios/`.
- No changes to `core/`. Elixir never re-implements rules.
- API keys are server-side only (env vars, read in `config/runtime.exs` in phase B). Nothing in this phase reads a key.
- The fake provider is configured only in `config/test.exs` and `config/e2e.exs`, never in `config/config.exs` or prod.
- Never create atoms from config strings or user input. Provider atoms come from config literals and map through a fixed whitelist.
- Tests: use `start_supervised!/1`; no `Process.sleep/1` in tests (use messages and `:sys.get_state/1`).
- Stage explicit paths only; never stage `helios/config/dev.exs`.
- Run `cd helios && mix precommit` before each commit. Commit messages end with `Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>`.

## Review Focus

- A model whose `provider` is not in the whitelist (a config typo) must not crash Readiness forever. The probe task fails, Readiness logs it and keeps its previous ready set. (Test in Task 2, "an unknown provider …".)
- Concurrent `ready?/1` calls during a slow probe must answer immediately from cached state. (Covered by the design, since probes run in a task. Test in Task 2, "reads do not wait for a running probe".)
- `refresh/0` spam (every lobby page load) must not trigger more than one probe per gap. (Test in Task 2.)
- `fake-down` and any model a probe did not return must never appear in `ready_models/0`. (Test in Task 2.)
- Removing a model from config while seats still reference it: `Models.label/1` falls back to the stored id and `Models.fetch/1` returns `:error`. (Test in Task 1.)

---

### Task 1: `Req`, model allowlist, provider behaviour, fake provider

**Files:**
- Modify: `helios/mix.exs` (deps)
- Modify: `helios/config/config.exs`, `helios/config/test.exs`, `helios/config/e2e.exs`
- Create: `helios/lib/helios/ai/models.ex`, `helios/lib/helios/ai/provider.ex`, `helios/lib/helios/ai/provider/fake.ex`
- Test: `helios/test/helios/ai/models_test.exs`, `helios/test/helios/ai/provider/fake_test.exs`

**Interfaces:**
- Produces: `Helios.AI.Models.all/0 :: [model]`, `fetch/1 :: {:ok, model} | :error`, `label/1 :: String.t()`, `provider_module/1 :: module()`, `seat_labels/1 :: [String.t()]`, `config/0 :: keyword()`.
- Produces: the `Helios.AI.Provider` behaviour (README).
- Produces: `Helios.AI.Provider.Fake` (README "Fake provider").

- [ ] **Step 1: Add `Req`**

In `helios/mix.exs` `deps/0`, after `{:jason, "~> 1.2"},` add:

```elixir
      {:req, "~> 0.5"},
```

Run: `cd helios && mix deps.get`
Expected: `req`, `finch` and `mint` (among others) are fetched and `mix.lock` changes.

- [ ] **Step 2: Config**

In `helios/config/config.exs`, after the `config :helios, Helios.Games, idle_timeout: …` line, add:

```elixir
# AI players. `models` is the allowlist table owners pick from; phase B adds
# the real Jev / OpenRouter entries. Keys are read in runtime.exs.
config :helios, Helios.AI,
  probe_interval_ms: :timer.minutes(10),
  models: []
```

Append to `helios/config/test.exs`:

```elixir
# Fake AI models (Helios.AI.Provider.Fake); `model` selects the behaviour.
config :helios, Helios.AI,
  probe_interval_ms: :timer.hours(1),
  models: [
    %{id: "fake-first", label: "Fake First", provider: :fake, model: "first", deadline_ms: 2_000},
    %{id: "fake-last", label: "Fake Last", provider: :fake, model: "last", deadline_ms: 2_000},
    %{id: "fake-timeout", label: "Fake Slow", provider: :fake, model: "timeout", deadline_ms: 100},
    %{id: "fake-error", label: "Fake Error", provider: :fake, model: "error", deadline_ms: 2_000},
    %{id: "fake-invalid", label: "Fake Invalid", provider: :fake, model: "invalid", deadline_ms: 2_000},
    %{id: "fake-down", label: "Fake Down", provider: :fake, model: "down", deadline_ms: 2_000}
  ]
```

Append to `helios/config/e2e.exs`:

```elixir
config :helios, Helios.AI,
  probe_interval_ms: :timer.hours(1),
  models: [
    %{id: "fake-first", label: "Fake AI", provider: :fake, model: "first", deadline_ms: 2_000},
    %{id: "fake-timeout", label: "Fake AI (slow)", provider: :fake, model: "timeout", deadline_ms: 500}
  ]
```

- [ ] **Step 3: Write the failing tests**

Create `helios/test/helios/ai/models_test.exs`:

```elixir
defmodule Helios.AI.ModelsTest do
  use ExUnit.Case, async: true

  alias Helios.AI.Models

  test "all/0 lists the configured allowlist in config order" do
    assert Enum.map(Models.all(), & &1.id) ==
             ~w(fake-first fake-last fake-timeout fake-error fake-invalid fake-down)
  end

  test "fetch/1 finds a model by id; unknown ids are :error" do
    assert {:ok, %{id: "fake-first", provider: :fake, deadline_ms: 2_000}} =
             Models.fetch("fake-first")

    assert Models.fetch("gone") == :error
    assert Models.fetch(nil) == :error
  end

  test "label/1 falls back to the stored id for models removed from config" do
    assert Models.label("fake-first") == "Fake First"
    assert Models.label("removed-model") == "removed-model"
  end

  test "provider_module/1 maps whitelisted providers only" do
    assert Models.provider_module(%{provider: :fake}) == Helios.AI.Provider.Fake
    assert Models.provider_module(%{provider: :jev}) == Helios.AI.Provider.Jev
    assert Models.provider_module(%{provider: :openrouter}) == Helios.AI.Provider.OpenRouter
    assert_raise KeyError, fn -> Models.provider_module(%{provider: :nope}) end
  end

  test "seat_labels/1 numbers repeated models from #2, in order" do
    assert Models.seat_labels(["fake-first", "fake-last", "fake-first", "gone", "fake-first"]) ==
             ["Fake First", "Fake Last", "Fake First #2", "gone", "Fake First #3"]

    assert Models.seat_labels([]) == []
  end
end
```

Create `helios/test/helios/ai/provider/fake_test.exs`:

```elixir
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
```

- [ ] **Step 4: Run to verify they fail**

Run: `cd helios && mix test test/helios/ai`
Expected: FAIL (`Helios.AI.Models` and `Helios.AI.Provider.Fake` are undefined).

- [ ] **Step 5: Implement**

Create `helios/lib/helios/ai/models.ex`:

```elixir
defmodule Helios.AI.Models do
  @moduledoc """
  The allowlist of AI models table owners may seat
  (`config :helios, Helios.AI, models: [...]`). An entry is
  `%{id, label, provider, model, deadline_ms}`; OpenRouter entries may add
  `reasoning: true` and `max_tokens`. `id` is the stable key stored on seats:
  a model removed from config keeps its id as label and plays random moves.
  """

  @providers %{
    jev: Helios.AI.Provider.Jev,
    openrouter: Helios.AI.Provider.OpenRouter,
    fake: Helios.AI.Provider.Fake
  }

  @type model :: %{
          required(:id) => String.t(),
          required(:label) => String.t(),
          required(:provider) => atom(),
          required(:model) => String.t(),
          required(:deadline_ms) => pos_integer(),
          optional(atom()) => term()
        }

  @spec config() :: keyword()
  def config, do: Application.get_env(:helios, Helios.AI, [])

  @spec all() :: [model()]
  def all, do: Keyword.get(config(), :models, [])

  @spec fetch(term()) :: {:ok, model()} | :error
  def fetch(id) do
    case Enum.find(all(), &(&1.id == id)) do
      nil -> :error
      model -> {:ok, model}
    end
  end

  @spec label(String.t()) :: String.t()
  def label(id) do
    case fetch(id) do
      {:ok, model} -> model.label
      :error -> id
    end
  end

  @doc "The adapter module for a model's provider (whitelisted; raises on unknown providers)."
  @spec provider_module(%{provider: atom()}) :: module()
  def provider_module(%{provider: provider}), do: Map.fetch!(@providers, provider)

  @doc "Display names for AI seats in seat order: the label, then `#2`, `#3`… for repeats."
  @spec seat_labels([String.t()]) :: [String.t()]
  def seat_labels(model_ids) do
    {labels, _seen} =
      Enum.map_reduce(model_ids, %{}, fn id, seen ->
        n = Map.get(seen, id, 0) + 1
        label = if n == 1, do: label(id), else: "#{label(id)} ##{n}"
        {label, Map.put(seen, id, n)}
      end)

    labels
  end
end
```

Create `helios/lib/helios/ai/provider.ex`:

```elixir
defmodule Helios.AI.Provider do
  @moduledoc """
  An AI provider adapter. `decide/3` answers one decision before the absolute
  `deadline` (monotonic milliseconds) with one of the prompt's move ids;
  `probe/1` checks the provider is configured and reachable and returns the
  ids of the given models that are ready.
  """

  @type usage :: %{input_tokens: non_neg_integer(), output_tokens: non_neg_integer()}

  @callback decide(model :: Helios.AI.Models.model(), prompt :: map(), deadline :: integer()) ::
              {:ok, String.t(), usage()} | {:error, term()}

  @callback probe(models :: [Helios.AI.Models.model()]) ::
              {:ok, [String.t()]} | {:error, term()}
end
```

Create `helios/lib/helios/ai/provider/fake.ex`:

```elixir
defmodule Helios.AI.Provider.Fake do
  @moduledoc """
  Test / e2e provider (never configured in prod). The model entry's `model`
  string selects the behaviour: `"first"` / `"last"` answer the first / last
  move, `"timeout"` never answers, `"error"` fails, `"invalid"` answers an id
  that was not offered, `"down"` fails its readiness probe. When
  `config :helios, :fake_ai_listener` holds a pid, it receives
  `{:fake_ai_decide, model_id, prompt}` and `{:fake_ai_probe, ready_ids}`.
  """
  @behaviour Helios.AI.Provider

  @usage %{input_tokens: 0, output_tokens: 0}

  @impl true
  def decide(model, prompt, _deadline) do
    notify({:fake_ai_decide, model.id, prompt})

    case model.model do
      "first" -> {:ok, hd(prompt.moves).id, @usage}
      "last" -> {:ok, List.last(prompt.moves).id, @usage}
      "invalid" -> {:ok, "zz", @usage}
      "error" -> {:error, :fake_error}
      "timeout" -> Process.sleep(:infinity)
    end
  end

  @impl true
  def probe(models) do
    ready = for model <- models, model.model != "down", do: model.id
    notify({:fake_ai_probe, ready})
    {:ok, ready}
  end

  defp notify(message) do
    case Application.get_env(:helios, :fake_ai_listener) do
      pid when is_pid(pid) -> send(pid, message)
      _ -> :ok
    end
  end
end
```

- [ ] **Step 6: Run the tests**

Run: `cd helios && mix test test/helios/ai`
Expected: PASS.

- [ ] **Step 7: Precommit and commit**

Run: `cd helios && mix precommit`
Expected: no warnings, all tests pass.

```bash
git add helios/mix.exs helios/mix.lock helios/config/config.exs helios/config/test.exs helios/config/e2e.exs \
  helios/lib/helios/ai/models.ex helios/lib/helios/ai/provider.ex helios/lib/helios/ai/provider/fake.ex \
  helios/test/helios/ai/models_test.exs helios/test/helios/ai/provider/fake_test.exs
git commit -m "feat(ai): model allowlist, provider behaviour and fake provider

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 2: `Helios.AI.Readiness` and the AI supervisors

**Files:**
- Create: `helios/lib/helios/ai/readiness.ex`
- Modify: `helios/lib/helios/application.ex`, `helios/test/test_helper.exs`
- Test: `helios/test/helios/ai/readiness_test.exs`

**Interfaces:**
- Consumes: `Helios.AI.Models.all/0`, `provider_module/1`, `config/0`. Each adapter's `probe/1`.
- Produces: `Readiness.start_link(opts)` with opts `:name` (default `Helios.AI.Readiness`), `:interval_ms` (default config `probe_interval_ms`), `:refresh_gap_ms` (default `60_000`) and `:models` (default `Models.all/0`; for tests).
- Produces: `ready?(id, server \\ Readiness) :: boolean`, `ready_models(server \\ Readiness) :: [model]`, `refresh(server \\ Readiness) :: :ok`, `probe_now(server \\ Readiness) :: %{ready: MapSet.t(String.t()), reasons: %{atom() => term()}}`, `topic/0 :: "ai:readiness"`.
- Produces: the named processes `Helios.AI.TaskSupervisor` and `Helios.AI.Supervisor`.

- [ ] **Step 1: Write the failing tests**

Create `helios/test/helios/ai/readiness_test.exs`:

```elixir
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
    start_supervised!({Readiness, Keyword.merge([name: name, interval_ms: :timer.hours(1)], opts)})
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
    :sys.replace_state(name, &%{&1 | task: %Task{ref: make_ref(), pid: self(), owner: self(), mfa: {:m, :f, 0}}})
    assert Readiness.ready?("fake-first", name)
  end
end
```

Note on the "unknown provider" test: `probe_now/1` must still reply when one provider's probe raises. `probe_all/1` (Step 3) wraps each provider in `safe_probe/2`, so the bad provider becomes a reason and the fake models stay ready.

- [ ] **Step 2: Run to verify they fail**

Run: `cd helios && mix test test/helios/ai/readiness_test.exs`
Expected: FAIL (`Helios.AI.Readiness` is undefined).

- [ ] **Step 3: Implement**

Create `helios/lib/helios/ai/readiness.ex`:

```elixir
defmodule Helios.AI.Readiness do
  @moduledoc """
  Which allowlisted models may be seated. Every provider is probed at boot,
  every `probe_interval_ms`, and on `refresh/0` (at most once per refresh gap).
  Probes run in a `Helios.AI.TaskSupervisor` task, so `ready?/1` and
  `ready_models/0` never wait on the network. After every probe round
  `{:ai_readiness_changed}` is broadcast on `"ai:readiness"`. A provider whose
  probe fails or raises makes none of its models ready; its reason is logged.
  """
  use GenServer

  require Logger

  alias Helios.AI.Models

  @topic "ai:readiness"
  @default_refresh_gap_ms 60_000
  @probe_now_timeout_ms 30_000

  def topic, do: @topic

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: Keyword.get(opts, :name, __MODULE__))
  end

  @spec ready?(term(), GenServer.server()) :: boolean()
  def ready?(id, server \\ __MODULE__), do: MapSet.member?(GenServer.call(server, :ready), id)

  @spec ready_models(GenServer.server()) :: [Models.model()]
  def ready_models(server \\ __MODULE__) do
    {ready, models} = GenServer.call(server, :ready_with_models)
    Enum.filter(models, &MapSet.member?(ready, &1.id))
  end

  @doc "Re-probes unless a probe ran within the refresh gap."
  @spec refresh(GenServer.server()) :: :ok
  def refresh(server \\ __MODULE__), do: GenServer.cast(server, :refresh)

  @doc "Probes now and waits for the result (mix task, tests)."
  @spec probe_now(GenServer.server()) :: %{ready: MapSet.t(String.t()), reasons: map()}
  def probe_now(server \\ __MODULE__),
    do: GenServer.call(server, :probe_now, @probe_now_timeout_ms)

  @impl true
  def init(opts) do
    state = %{
      models: Keyword.get_lazy(opts, :models, &Models.all/0),
      interval_ms:
        Keyword.get_lazy(opts, :interval_ms, fn ->
          Keyword.get(Models.config(), :probe_interval_ms, :timer.minutes(10))
        end),
      refresh_gap_ms: Keyword.get(opts, :refresh_gap_ms, @default_refresh_gap_ms),
      ready: MapSet.new(),
      reasons: %{},
      task: nil,
      last_probe_at: nil,
      waiting: []
    }

    Process.send_after(self(), :tick, state.interval_ms)
    {:ok, state, {:continue, :probe}}
  end

  @impl true
  def handle_continue(:probe, state), do: {:noreply, start_probe(state)}

  @impl true
  def handle_call(:ready, _from, state), do: {:reply, state.ready, state}

  def handle_call(:ready_with_models, _from, state),
    do: {:reply, {state.ready, state.models}, state}

  def handle_call(:probe_now, from, state) do
    state = start_probe(state)
    {:noreply, %{state | waiting: [from | state.waiting]}}
  end

  @impl true
  def handle_cast(:refresh, state) do
    if state.last_probe_at == nil or now() - state.last_probe_at >= state.refresh_gap_ms,
      do: {:noreply, start_probe(state)},
      else: {:noreply, state}
  end

  @impl true
  def handle_info(:tick, state) do
    Process.send_after(self(), :tick, state.interval_ms)
    {:noreply, start_probe(state)}
  end

  def handle_info({ref, {ready, reasons}}, %{task: %Task{ref: ref}} = state) do
    Process.demonitor(ref, [:flush])

    for {provider, reason} <- reasons,
        do: Logger.warning("AI provider #{provider} is not ready: #{inspect(reason)}")

    finish(%{state | ready: ready, reasons: reasons})
  end

  def handle_info({:DOWN, ref, :process, _pid, reason}, %{task: %Task{ref: ref}} = state) do
    Logger.error("AI readiness probe crashed: #{inspect(reason)}")
    finish(state)
  end

  def handle_info(_message, state), do: {:noreply, state}

  defp finish(state) do
    result = %{ready: state.ready, reasons: state.reasons}
    Enum.each(state.waiting, &GenServer.reply(&1, result))
    Phoenix.PubSub.broadcast(Helios.PubSub, @topic, {:ai_readiness_changed})
    {:noreply, %{state | task: nil, waiting: []}}
  end

  defp start_probe(%{task: nil, models: models} = state) do
    task = Task.Supervisor.async_nolink(Helios.AI.TaskSupervisor, fn -> probe_all(models) end)
    %{state | task: task, last_probe_at: now()}
  end

  defp start_probe(state), do: state

  # One probe per provider, in parallel; a provider that raises or is not
  # whitelisted contributes a reason instead of ready ids.
  defp probe_all(models) do
    models
    |> Enum.group_by(& &1.provider)
    |> Task.async_stream(fn {provider, group} -> {provider, safe_probe(provider, group)} end,
      timeout: :infinity
    )
    |> Enum.reduce({MapSet.new(), %{}}, fn
      {:ok, {_provider, {:ok, ids}}}, {ready, reasons} ->
        {MapSet.union(ready, MapSet.new(ids)), reasons}

      {:ok, {provider, {:error, reason}}}, {ready, reasons} ->
        {ready, Map.put(reasons, provider, reason)}
    end)
  end

  defp safe_probe(provider, models) do
    Models.provider_module(%{provider: provider}).probe(models)
  rescue
    exception -> {:error, {:exception, Exception.message(exception)}}
  end

  defp now, do: System.monotonic_time(:millisecond)
end
```

In `helios/lib/helios/application.ex`, add three children after `HeliosWeb.Presence,`:

```elixir
      {Task.Supervisor, name: Helios.AI.TaskSupervisor},
      {DynamicSupervisor, name: Helios.AI.Supervisor, strategy: :one_for_one},
      Helios.AI.Readiness,
```

In `helios/test/test_helper.exs`, after `ExUnit.start()` add:

```elixir
# The app's Readiness probes the fake models at boot; wait so every test sees them ready.
Helios.AI.Readiness.probe_now()
```

- [ ] **Step 4: Run the tests**

Run: `cd helios && mix test test/helios/ai`
Expected: PASS.

- [ ] **Step 5: Precommit and commit**

Run: `cd helios && mix precommit`
Expected: no warnings, all tests pass.

```bash
git add helios/lib/helios/ai/readiness.ex helios/lib/helios/application.ex helios/test/test_helper.exs \
  helios/test/helios/ai/readiness_test.exs
git commit -m "feat(ai): provider readiness cache with periodic and on-demand probes

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```
