# AI Players — Phase B: Prompt and Providers Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Given an AI seat's own player view, build a token-light decision payload and get one legal move id back from Jev or OpenRouter within a strict deadline. Readiness can also verify both providers. An operator can check keys with `mix helios.ai.probe` and measure real latency and tokens with local-only live tests.

**Architecture:** `Helios.AI.Prompt.build/1` turns a view into `%{state, moves, primer}`:
- `state` is compact JSON-able data enriched from `Helios.Catalogue` and `HeliosWeb.CardText`.
- `moves` are short ids (`b0`, `w0`, `d0`, `f0`, `x0`) with cost labels. Trades show only their cheapest cost.
- `primer` is a fixed persona line.

`Prompt.user_message/2` renders the state for chat models as TOON (default) or compact JSON.

`Helios.AI.HTTP.request/2` wraps `Req` with deadline-bounded retries. `Helios.AI.Provider.Jev` sends one `choice` question to `/v1/systemone`. `Helios.AI.Provider.OpenRouter` sends a chat completion whose `response_format` is a strict JSON schema with an `enum` of move ids. Both implement `probe/1` for `Helios.AI.Readiness`.

**Tech Stack:** Elixir, `Req` + `Req.Test`, Jason, `toon` (TOON encoder, `~> 0.3.0`), ExUnit.

**Spec:** `docs/superpowers/specs/2026-10-13-ai-players/2026-10-13-ai-players-design.md` (sections "The decision payload" incl. "Formats", "Provider adapters", "Provider readiness", "Configuration", "Testing"). Cross-phase contract: `docs/superpowers/plans/2026-10-13-ai-players/README.md`.

## Global Constraints

- **Prerequisite:** phase A is merged (`Helios.AI.Models`, `Helios.AI.Provider`, `Helios.AI.Readiness`, `Req`). If anything there differs from the README, stop and ask.
- Read `helios/AGENTS.md` before touching `helios/`.
- Never send trade payment lists to a provider. A trade appears only as `trade, from <k> coins`, where k is the cheapest total.
- Legality comes only from the view's build options (`Helios.Games.Choice.available?/1`). Aggregated production and science in the prompt are presentation only.
- **One call per decision.** Retries happen only inside the deadline, and only for 429, 529, other 5xx, and transport errors. Each attempt's `receive_timeout` is the remaining budget.
- No real network calls in the default test run. Every adapter test goes through `Req.Test` stubs configured with `req_options: [plug: {Req.Test, <adapter module>}]`. Only the `:live_ai`-tagged tests (Task 6) call real providers, and they are excluded by default.
- API keys only come from `config/runtime.exs` (env `JEV_API_KEY`, `OPENROUTER_API_KEY`). The test config sets dummy keys. Keys are never logged.
- Stage explicit paths only; never stage `helios/config/dev.exs`.
- Run `cd helios && mix precommit` before each commit. Commit messages end with `Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>`.

## Review Focus

- A view where a card's build option is `{:trade, [...]}` with options of different totals: the label must show the *minimum* total, and `to_choice/1` must pick only indexes inside the list. (Test in Task 1.)
- A Halicarnassus build-from-discard view (`hand: []`, `discard_pile: [...]`) offers only `x<n>` moves. A Babylon play-last-card view (one hand card) offers `b`/`w`/`d` for that card only. (Tests in Task 1.)
- The encoded `state` must be valid JSON: no tuples, no structs. Tokens must stay small, so `Jason.encode!(state)` of a 4-player sample stays under 3,000 bytes. (Test in Task 1.)
- A provider answer that names an id that was not offered, or malformed JSON content, becomes `{:error, :invalid_move}` and is never forwarded. (Tests in Tasks 3 and 4.)
- 401 or 403 responses are never retried, and repeated 529s stop before the deadline instead of overrunning it. (Tests in Task 2.)
- Effect texts contain commas and colons; TOON must quote them so the table stays parseable. The TOON state must decode back to the same data. (Test in Task 1, "TOON round-trips".)

---

### Task 1: `Helios.AI.Prompt`

**Files:**
- Modify: `helios/mix.exs` (deps), `helios/mix.lock`
- Create: `helios/lib/helios/ai/prompt.ex`
- Test: `helios/test/helios/ai/prompt_test.exs`

**Interfaces:**
- Consumes: `Helios.Catalogue.card/1`, `wonder_board/2`, `stage/3`; `HeliosWeb.CardText.effects/1`, `cost/1`; `HeliosWeb.GameFormat.others/1`; `Helios.Games.Choice.available?/1`.
- Produces: `Prompt.build(view) :: Prompt.t()`, `Prompt.to_choice(move) :: Choice.t()`, `Prompt.primer/0` (README "Prompt"), `Prompt.user_message(prompt, :toon | :json) :: String.t()`.
- Adds the dependency `{:toon, "~> 0.3.0"}`.

- [ ] **Step 1: Add `toon`**

In `helios/mix.exs` `deps/0`, after `{:req, "~> 0.5"},` add `{:toon, "~> 0.3.0"},`, then run `cd helios && mix deps.get`.
Expected: `toon` is fetched. Check its docs (`mix hex.docs fetch toon` or hexdocs) for the decoder function name used in the round-trip test. The README of 0.3.0 documents `Toon.encode!/1,2`; if the decoder isn't `Toon.decode!/1`, use the package's decoder in that test.

- [ ] **Step 2: Write the failing tests**

Create `helios/test/helios/ai/prompt_test.exs`:

```elixir
defmodule Helios.AI.PromptTest do
  use ExUnit.Case, async: true

  alias Helios.AI.Prompt
  alias Helios.SampleViews

  defp ids(prompt), do: Enum.map(prompt.moves, & &1.id)
  defp move(prompt, id), do: Enum.find(prompt.moves, &(&1.id == id))

  describe "moves" do
    test "only legal moves, per hand card: build, wonder stage, free build, discard" do
      prompt = Prompt.build(SampleViews.view())

      # Baths: build unavailable, stage by trade. Tavern: free + Olympia free build.
      # Tree Farm: 1 coin. Discard is always legal.
      assert ids(prompt) == ~w(w0 d0 b1 f1 d1 b2 d2)

      assert move(prompt, "w0") == %{
               id: "w0",
               label: "build wonder stage 2 with Baths: trade, from 2 coins",
               card: "Baths",
               kind: "wonder_stage",
               trade_options: 2
             }

      assert move(prompt, "b1").label == "build Tavern: free"
      assert move(prompt, "f1") |> Map.take([:kind, :label]) ==
               %{kind: "build_free", label: "build Tavern free (once per age)"}

      assert move(prompt, "b2").label == "build Tree Farm: 1 coin"
      assert move(prompt, "d2") |> Map.take([:kind, :label]) ==
               %{kind: "discard", label: "discard Tree Farm: +3 coins"}
    end

    test "the trade label shows the cheapest total, never payments" do
      options = [
        %{payment: %{west: [{:ore, 2}], east: []}, west_coins: 4, east_coins: 0, bank_coins: 1},
        %{payment: %{west: [], east: [{:ore, 2}]}, west_coins: 0, east_coins: 2, bank_coins: 1}
      ]

      [baths | rest] = SampleViews.hand()
      view = SampleViews.view(%{hand: [%{baths | wonder_stage: {:trade, options}} | rest]})
      prompt = Prompt.build(view)

      assert move(prompt, "w0").label =~ "trade, from 3 coins"
      encoded = Jason.encode!(prompt)
      refute encoded =~ "west_coins"
      refute encoded =~ "payment"
    end

    test "chain builds say so" do
      [baths | rest] = SampleViews.hand()
      view = SampleViews.view(%{hand: [%{baths | build: {:chain, "Altar"}} | rest]})
      assert move(Prompt.build(view), "b0").label == "build Baths: free (chain from Altar)"
    end

    test "build from discard offers only the pile, free" do
      view =
        SampleViews.view(%{
          phase: %{
            kind: :extra_turn,
            age: 1,
            turn: 6,
            direction: :west,
            extra_turn_player: "1",
            extra_turn_kind: :build_from_discard
          },
          hand: [],
          discard_pile: ["Altar", "Baths"]
        })

      prompt = Prompt.build(view)
      assert ids(prompt) == ~w(x0 x1)

      assert move(prompt, "x1") == %{
               id: "x1",
               label: "build Baths from the discard pile, free",
               card: "Baths",
               kind: "build_from_discard",
               trade_options: 0
             }

      assert Enum.map(prompt.state.discard_pile, & &1.name) == ["Altar", "Baths"]
    end

    test "play last card offers the single card's moves" do
      [_baths, tavern, _farm] = SampleViews.hand()

      view =
        SampleViews.view(%{
          phase: %{
            kind: :extra_turn,
            age: 1,
            turn: 6,
            direction: :west,
            extra_turn_player: "1",
            extra_turn_kind: :play_last_card
          },
          hand: [tavern]
        })

      assert ids(Prompt.build(view)) == ~w(b0 f0 d0)
    end
  end

  describe "to_choice/1" do
    test "non-trade moves use option 0" do
      prompt = Prompt.build(SampleViews.view())

      assert Prompt.to_choice(move(prompt, "b2")) ==
               %{card: "Tree Farm", kind: "build", option: 0}

      assert Prompt.to_choice(move(prompt, "d0")) == %{card: "Baths", kind: "discard", option: 0}
    end

    test "trades pick a random payment index inside the engine's list" do
      trade = move(Prompt.build(SampleViews.view()), "w0")
      options = for _ <- 1..60, into: MapSet.new(), do: Prompt.to_choice(trade).option
      assert options == MapSet.new([0, 1])
    end
  end

  describe "state" do
    test "compact boards for me and my neighbours, summaries for the others" do
      state = Prompt.build(SampleViews.view()).state

      assert %{age: 2, turn: 3, pass: "east", discard_count: 2} = state
      assert state.me.wonder == "Gizah A"
      assert state.me.stages == "1/3"
      assert state.me.coins == 4
      assert state.me.built == ["Lumber Yard", "Altar", "Stockade"]
      assert "wood" in state.me.produces
      assert state.me.shields == 1
      assert state.me.military_tokens == [1, -1]
      assert is_binary(state.me.next_stage)
      assert state.east.wonder == "Rhódos B"
      assert state.west.wonder == "Halikarnassós B"

      assert [%{wonder: "Éphesos A", stages: "1/3", coins: 4, shields: 1, cards: cards}] =
               state.others

      assert cards == "1 blue, 1 brown, 1 red"
    end

    test "hand cards are flat rows: colour, effect text and chains as strings" do
      [baths, tavern, farm] = Prompt.build(SampleViews.view()).state.hand
      assert %{name: "Baths", colour: "blue", effects: baths_text, chains_to: chains} = baths
      assert baths_text =~ "victory point"
      assert is_binary(chains)
      assert tavern.colour == "yellow"
      assert farm.colour == "brown"
      assert Enum.all?([baths, tavern, farm], &(Map.keys(&1) |> Enum.sort() == [:chains_to, :colour, :effects, :name]))
    end

    test "is JSON and small" do
      json = Jason.encode!(Prompt.build(SampleViews.view()).state)
      assert byte_size(json) < 3_000
    end

    test "omits extra_turn outside extra turns" do
      refute Map.has_key?(Prompt.build(SampleViews.view()).state, :extra_turn)
    end
  end

  describe "user_message/2" do
    test "lists the moves after the state, in either format" do
      prompt = Prompt.build(SampleViews.view())

      for format <- [:json, :toon] do
        assert Prompt.user_message(prompt, format) =~
                 "\nMoves:\nw0: build wonder stage 2 with Baths: trade, from 2 coins\nd0: discard Baths: +3 coins\n"
      end
    end

    test "TOON writes the hand as a table and is shorter than JSON" do
      prompt = Prompt.build(SampleViews.view())
      toon = Prompt.user_message(prompt, :toon)
      json = Prompt.user_message(prompt, :json)

      assert "State (TOON):\n" <> _ = toon
      assert toon =~ ~r/hand\[3\]\{[a-z_,]+\}:/
      assert "State (JSON):\n" <> _ = json
      assert byte_size(toon) < byte_size(json)
    end

    test "TOON round-trips" do
      prompt = Prompt.build(SampleViews.view())
      "State (TOON):\n" <> rest = Prompt.user_message(prompt, :toon)
      [body, _moves] = String.split(rest, "\nMoves:\n", parts: 2)
      assert Toon.decode!(body) == prompt.state |> Jason.encode!() |> Jason.decode!()
    end
  end

  test "the primer is a short fixed persona" do
    assert Prompt.primer() =~ "7 Wonders"
    assert String.length(Prompt.primer()) < 400
    assert Prompt.build(SampleViews.view()).primer == Prompt.primer()
  end
end
```

- [ ] **Step 3: Run to verify they fail**

Run: `cd helios && mix test test/helios/ai/prompt_test.exs`
Expected: FAIL (`Helios.AI.Prompt` is undefined).

- [ ] **Step 4: Implement**

Create `helios/lib/helios/ai/prompt.ex`:

```elixir
defmodule Helios.AI.Prompt do
  @moduledoc """
  The token-light payload of one AI decision, built from the AI seat's own
  view plus the static catalogue:

    * `state` — compact JSON-able position (boards for me and my neighbours,
      one-line summaries for everyone else, my hand with effect text);
    * `moves` — the legal moves as short ids (`b<n>` build, `w<n>` wonder
      stage, `f<n>` Olympia free build, `d<n>` discard, `x<n>` build from the
      discard pile) with cost labels; trades show only their cheapest total;
    * `primer` — a fixed persona line that relies on the model knowing the game.

  `user_message/2` renders the state for chat models (TOON or JSON).

  Production and science totals are presentation only (summed from catalogue
  effect specs); legality and costs come from the view's build options.
  """

  alias Helios.Catalogue
  alias Helios.Games.Choice
  alias HeliosWeb.{CardText, GameFormat}

  @primer "You are an expert player of the 7 Wonders board game (base game). " <>
            "You see your current position and your legal moves. " <>
            "Choose the move that maximises your final score."

  @colours %{
    raw_material: "brown",
    manufactured_good: "grey",
    civilian: "blue",
    commercial: "yellow",
    military: "red",
    scientific: "green",
    guild: "purple"
  }

  @type move :: %{
          id: String.t(),
          label: String.t(),
          card: String.t(),
          kind: String.t(),
          trade_options: non_neg_integer()
        }
  @type t :: %{state: map(), moves: [move()], primer: String.t()}

  @spec primer() :: String.t()
  def primer, do: @primer

  @spec build(map()) :: t()
  def build(view), do: %{state: state(view), moves: moves(view), primer: @primer}

  @doc "The browser-style choice for a move; trades get a random payment index."
  @spec to_choice(move()) :: Choice.t()
  def to_choice(%{card: card, kind: kind, trade_options: n}) do
    option = if n > 0, do: :rand.uniform(n) - 1, else: 0
    %{card: card, kind: kind, option: option}
  end

  @doc """
  The chat-model user message: the state as TOON (tables for uniform lists)
  or compact JSON, then one `id: label` line per move.
  """
  @spec user_message(t(), :toon | :json) :: String.t()
  def user_message(prompt, format) do
    render(format, prompt.state) <>
      "\nMoves:\n" <> Enum.map_join(prompt.moves, "", &"#{&1.id}: #{&1.label}\n")
  end

  defp render(:json, state), do: "State (JSON):\n" <> Jason.encode!(state)

  # String keys (via a JSON round-trip) so the encoder sees plain data.
  defp render(:toon, state),
    do: "State (TOON):\n" <> Toon.encode!(state |> Jason.encode!() |> Jason.decode!())

  ## Moves

  defp moves(%{phase: %{kind: :extra_turn, extra_turn_kind: :build_from_discard}} = view)
       when is_list(view.discard_pile) do
    view.discard_pile
    |> Enum.with_index()
    |> Enum.map(fn {card, n} ->
      move("x#{n}", "build #{card} from the discard pile, free", card, "build_from_discard", 0)
    end)
  end

  defp moves(view) do
    me = Enum.find(view.players, &(&1.name == view.me))

    view.hand
    |> Enum.with_index()
    |> Enum.flat_map(fn {card, n} -> card_moves(card, n, me.stages_built + 1) end)
  end

  defp card_moves(card, n, stage) do
    [
      Choice.available?(card.build) &&
        move("b#{n}", "build #{card.name}: #{cost(card.build)}", card.name, "build", trades(card.build)),
      Choice.available?(card.wonder_stage) &&
        move(
          "w#{n}",
          "build wonder stage #{stage} with #{card.name}: #{cost(card.wonder_stage)}",
          card.name,
          "wonder_stage",
          trades(card.wonder_stage)
        ),
      card.free_build &&
        move("f#{n}", "build #{card.name} free (once per age)", card.name, "build_free", 0),
      move("d#{n}", "discard #{card.name}: +3 coins", card.name, "discard", 0)
    ]
    |> Enum.filter(& &1)
  end

  defp move(id, label, card, kind, trade_options),
    do: %{id: id, label: label, card: card, kind: kind, trade_options: trade_options}

  defp cost(:free), do: "free"
  defp cost({:chain, from}), do: "free (chain from #{from})"
  defp cost({:coins, 1}), do: "1 coin"
  defp cost({:coins, n}), do: "#{n} coins"
  defp cost({:trade, options}), do: "trade, from #{options |> Enum.map(&total/1) |> Enum.min()} coins"

  defp total(%{west_coins: west, east_coins: east, bank_coins: bank}), do: west + east + bank

  defp trades({:trade, options}), do: length(options)
  defp trades(_option), do: 0

  ## State

  defp state(view) do
    by_name = Map.new(view.players, &{&1.name, &1})

    %{
      age: view.phase.age,
      turn: view.phase.turn,
      pass: Atom.to_string(view.phase.direction),
      me: board(Map.fetch!(by_name, view.me)),
      west: board(Map.fetch!(by_name, view.west)),
      east: board(Map.fetch!(by_name, view.east)),
      others: Enum.map(GameFormat.others(view), &summary/1),
      hand: Enum.map(view.hand, &card_entry(&1.name)),
      discard_count: view.discard_count
    }
    |> put_if(:extra_turn, view.phase.extra_turn_kind && Atom.to_string(view.phase.extra_turn_kind))
    |> put_if(:discard_pile, is_list(view.discard_pile) && Enum.map(view.discard_pile, &card_entry/1))
  end

  defp board(player) do
    effects = city_effects(player)

    %{
      wonder: "#{player.wonder} #{GameFormat.side_label(player.side)}",
      coins: player.coins,
      stages: "#{player.stages_built}/#{player.stages_total}",
      built: Enum.map(player.built, & &1.name),
      produces: produces(effects),
      science: science(effects),
      shields: player.shields,
      military_tokens: player.military_tokens
    }
    |> put_if(:next_stage, next_stage(player))
  end

  defp summary(player) do
    %{
      wonder: "#{player.wonder} #{GameFormat.side_label(player.side)}",
      stages: "#{player.stages_built}/#{player.stages_total}",
      coins: player.coins,
      shields: player.shields,
      cards:
        player.built
        |> Enum.frequencies_by(&Map.fetch!(@colours, &1.category))
        |> Enum.sort()
        |> Enum.map_join(", ", fn {colour, n} -> "#{n} #{colour}" end)
    }
  end

  # Flat rows of scalars with identical keys, so TOON writes them as a table.
  defp card_entry(name) do
    info = Catalogue.card(name)

    %{
      name: name,
      colour: Map.fetch!(@colours, info.category),
      effects: Enum.join(CardText.effects(info.effects), "; "),
      chains_to: Enum.join(info.chains_to, ", ")
    }
  end

  defp city_effects(player) do
    board = Catalogue.wonder_board(player.wonder, player.side)
    stages = board.stages |> Enum.take(player.stages_built) |> Enum.flat_map(& &1.effects)

    cards =
      Enum.flat_map(player.built, fn card ->
        case Catalogue.card(card.name) do
          nil -> []
          info -> info.effects
        end
      end)

    board.effects ++ stages ++ cards
  end

  defp next_stage(%{stages_built: built, stages_total: total} = player) when built < total do
    case Catalogue.stage(player.wonder, player.side, built + 1) do
      nil -> nil
      stage -> "#{CardText.cost(stage.cost)} -> #{Enum.join(CardText.effects(stage.effects), "; ")}"
    end
  end

  defp next_stage(_player), do: nil

  defp produces(effects) do
    tally(effects, fn
      {:produces, %{resources: resources, choice: choice}} -> {resources, choice}
      _other -> nil
    end)
  end

  defp science(effects) do
    tally(effects, fn
      {:science, %{symbols: symbols, choice: choice}} -> {symbols, choice}
      _other -> nil
    end)
  end

  # Fixed items counted ("wood x2"), alternatives listed ("clay/ore").
  defp tally(effects, pick) do
    {fixed, choices} =
      Enum.reduce(effects, {%{}, []}, fn effect, {fixed, choices} ->
        case pick.(effect) do
          {items, false} ->
            {Enum.reduce(items, fixed, &Map.update(&2, &1, 1, fn n -> n + 1 end)), choices}

          {items, true} ->
            {fixed, [Enum.map_join(items, "/", &to_string/1) | choices]}

          nil ->
            {fixed, choices}
        end
      end)

    counted =
      fixed
      |> Enum.sort()
      |> Enum.map(fn
        {item, 1} -> to_string(item)
        {item, n} -> "#{item} x#{n}"
      end)

    counted ++ Enum.reverse(choices)
  end

  defp put_if(map, _key, value) when value in [nil, false], do: map
  defp put_if(map, key, value), do: Map.put(map, key, value)
end
```

Note: `SampleViews.player/2` gives every board `stages_total: 3` regardless of wonder. If `Catalogue.stage/3` returns nil for a stage the real board lacks, `next_stage/1` returns nil, which is fine. If a test assertion about `next_stage` fails because of this, take the value from a real run instead of changing the code.

- [ ] **Step 5: Run the tests**

Run: `cd helios && mix test test/helios/ai/prompt_test.exs`
Expected: PASS. If the "is JSON and small" byte bound fails, report the real size instead of silently raising the bound. The spec's budget is roughly 1–1.5k input tokens.

- [ ] **Step 6: Precommit and commit**

```bash
git add helios/mix.exs helios/mix.lock helios/lib/helios/ai/prompt.ex helios/test/helios/ai/prompt_test.exs
git commit -m "feat(ai): token-light decision payload and legal move ids

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 2: `Helios.AI.HTTP` — deadline-bounded requests

**Files:**
- Create: `helios/lib/helios/ai/http.ex`
- Test: `helios/test/helios/ai/http_test.exs`

**Interfaces:**
- Produces: `HTTP.request(req_options :: keyword(), deadline :: integer()) :: {:ok, Req.Response.t()} | {:error, :deadline | {:http, status} | {:transport, Exception.t()}}`. Non-retriable responses (2xx, 3xx, 4xx other than 429) come back as `{:ok, response}`.
- Produces: `HTTP.now/0 :: integer()` (monotonic milliseconds; the clock deadlines use).

- [ ] **Step 1: Write the failing tests**

Create `helios/test/helios/ai/http_test.exs`:

```elixir
defmodule Helios.AI.HTTPTest do
  use ExUnit.Case, async: true

  alias Helios.AI.HTTP

  @options [method: :get, url: "http://ai.test/x", plug: {Req.Test, __MODULE__}]

  # Answers `statuses` in order (the last one repeats) and counts calls.
  defp stub(statuses) do
    counter = :counters.new(1, [])

    Req.Test.stub(__MODULE__, fn conn ->
      :counters.add(counter, 1, 1)
      n = :counters.get(counter, 1)

      case Enum.at(statuses, n - 1, List.last(statuses)) do
        :transport -> Req.Test.transport_error(conn, :econnrefused)
        status -> conn |> Plug.Conn.put_status(status) |> Req.Test.json(%{"n" => n})
      end
    end)

    fn -> :counters.get(counter, 1) end
  end

  test "a 429 is retried and the success is returned" do
    calls = stub([429, 200])
    assert {:ok, %Req.Response{status: 200, body: %{"n" => 2}}} =
             HTTP.request(@options, HTTP.now() + 2_000)

    assert calls.() == 2
  end

  test "transport errors and 5xx are retried" do
    calls = stub([:transport, 503, 200])
    assert {:ok, %Req.Response{status: 200}} = HTTP.request(@options, HTTP.now() + 3_000)
    assert calls.() == 3
  end

  test "401 is returned as is, never retried" do
    calls = stub([401])
    assert {:ok, %Req.Response{status: 401}} = HTTP.request(@options, HTTP.now() + 2_000)
    assert calls.() == 1
  end

  test "repeated 529s stop before the deadline" do
    stub([529])
    started = HTTP.now()
    assert {:error, {:http, 529}} = HTTP.request(@options, started + 700)
    assert HTTP.now() - started < 700
  end

  test "no attempt when the budget is already too small" do
    calls = stub([200])
    assert {:error, :deadline} = HTTP.request(@options, HTTP.now() + 100)
    assert calls.() == 0
  end
end
```

- [ ] **Step 2: Run to verify they fail**

Run: `cd helios && mix test test/helios/ai/http_test.exs`
Expected: FAIL (`Helios.AI.HTTP` is undefined).

- [ ] **Step 3: Implement**

Create `helios/lib/helios/ai/http.ex`:

```elixir
defmodule Helios.AI.HTTP do
  @moduledoc """
  One provider request under an absolute deadline (monotonic milliseconds).
  429, 529, other 5xx and transport errors are retried with short backoff
  while at least #{300} ms remain; every attempt's `receive_timeout` is the
  remaining budget, so the call never outlives the deadline by more than
  connection teardown. Other responses are returned unchanged.
  """

  @backoff_ms [100, 250, 500]
  @min_attempt_ms 300

  @spec now() :: integer()
  def now, do: System.monotonic_time(:millisecond)

  @spec request(keyword(), integer()) ::
          {:ok, Req.Response.t()} | {:error, :deadline | {:http, pos_integer()} | {:transport, term()}}
  def request(options, deadline), do: attempt(options, deadline, @backoff_ms, :deadline)

  defp attempt(options, deadline, backoff, last_error) do
    remaining = deadline - now()

    if remaining < @min_attempt_ms do
      {:error, last_error}
    else
      options
      |> Keyword.merge(retry: false, receive_timeout: remaining, connect_options: [timeout: remaining])
      |> Req.request()
      |> handle(options, deadline, backoff)
    end
  end

  defp handle({:ok, %Req.Response{status: status} = response}, options, deadline, backoff) do
    if status == 429 or status >= 500,
      do: retry(options, deadline, backoff, {:http, status}),
      else: {:ok, response}
  end

  defp handle({:error, exception}, options, deadline, backoff),
    do: retry(options, deadline, backoff, {:transport, exception})

  defp retry(_options, _deadline, [], reason), do: {:error, reason}

  defp retry(options, deadline, [delay | rest], reason) do
    if deadline - now() - delay < @min_attempt_ms do
      {:error, reason}
    else
      Process.sleep(delay)
      attempt(options, deadline, rest, reason)
    end
  end
end
```

- [ ] **Step 4: Run the tests**

Run: `cd helios && mix test test/helios/ai/http_test.exs`
Expected: PASS.

- [ ] **Step 5: Precommit and commit**

```bash
git add helios/lib/helios/ai/http.ex helios/test/helios/ai/http_test.exs
git commit -m "feat(ai): deadline-bounded provider HTTP with retries inside the budget

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 3: Jev adapter

**Files:**
- Create: `helios/lib/helios/ai/provider/jev.ex`
- Modify: `helios/config/test.exs`
- Test: `helios/test/helios/ai/provider/jev_test.exs`

**Interfaces:**
- Consumes: `Helios.AI.HTTP.request/2`, `now/0`; prompt shape from Task 1.
- Produces: `Helios.AI.Provider.Jev.decide/3` and `probe/1` (behaviour). Config: `config :helios, Helios.AI.Provider.Jev, api_key: String.t() | nil, base_url: String.t() (default "https://api.typesafe.ai"), req_options: keyword()`.

- [ ] **Step 1: Test config**

Append to `helios/config/test.exs`:

```elixir
config :helios, Helios.AI.Provider.Jev,
  api_key: "test-jev-key",
  req_options: [plug: {Req.Test, Helios.AI.Provider.Jev}]
```

- [ ] **Step 2: Write the failing tests**

Create `helios/test/helios/ai/provider/jev_test.exs`:

```elixir
defmodule Helios.AI.Provider.JevTest do
  use ExUnit.Case, async: false

  alias Helios.AI.HTTP
  alias Helios.AI.Provider.Jev

  @model %{id: "jev-1.13", label: "Jev 1.13", provider: :jev, model: "jev-1.13.0", deadline_ms: 5_000}
  @prompt %{
    state: %{age: 1, turn: 1},
    primer: "You are an expert player.",
    moves: [
      %{id: "b0", label: "build Altar: free", card: "Altar", kind: "build", trade_options: 0},
      %{id: "d0", label: "discard Altar: +3 coins", card: "Altar", kind: "discard", trade_options: 0}
    ]
  }

  defp capture(response) do
    test = self()

    Req.Test.stub(Jev, fn conn ->
      {:ok, body, conn} = Plug.Conn.read_body(conn)
      send(test, {:request, conn.method, conn.request_path, Plug.Conn.get_req_header(conn, "authorization"), body})
      response.(conn)
    end)
  end

  defp answer(choice) do
    fn conn ->
      Req.Test.json(conn, %{
        "model" => "jev-1.13.0",
        "answers" => %{"move" => %{"type" => "choice", "choice" => choice, "confidence" => 0.9}},
        "usage" => %{"input_tokens" => 812, "output_tokens" => 3}
      })
    end
  end

  test "decide/3 sends one choice question with the moves as criteria" do
    capture(answer("d0"))

    assert {:ok, "d0", %{input_tokens: 812, output_tokens: 3}} =
             Jev.decide(@model, @prompt, HTTP.now() + 2_000)

    assert_received {:request, "POST", "/v1/systemone", ["Bearer test-jev-key"], body}

    assert Jason.decode!(body) == %{
             "model" => "jev-1.13.0",
             "state" => %{"age" => 1, "turn" => 1},
             "questions" => %{
               "move" => %{
                 "type" => "choice",
                 "instructions" => "You are an expert player.",
                 "criteria" => %{"b0" => "build Altar: free", "d0" => "discard Altar: +3 coins"}
               }
             }
           }
  end

  test "an id that was not offered is :invalid_move" do
    capture(answer("x9"))
    assert {:error, :invalid_move} = Jev.decide(@model, @prompt, HTTP.now() + 2_000)
  end

  test "errors are returned, 401 without retry" do
    capture(fn conn -> conn |> Plug.Conn.put_status(401) |> Req.Test.json(%{}) end)
    assert {:error, {:http, 401}} = Jev.decide(@model, @prompt, HTTP.now() + 2_000)
    assert_received {:request, _, _, _, _}
    refute_received {:request, _, _, _, _}
  end

  test "no key: not ready, no request" do
    original = Application.get_env(:helios, Jev)
    Application.put_env(:helios, Jev, Keyword.put(original, :api_key, nil))
    on_exit(fn -> Application.put_env(:helios, Jev, original) end)

    assert {:error, :no_key} = Jev.decide(@model, @prompt, HTTP.now() + 2_000)
    assert {:error, :no_key} = Jev.probe([@model])
  end

  describe "probe/1" do
    test "a listing answer makes every Jev model ready" do
      capture(fn conn -> Req.Test.json(conn, %{"data" => [%{"id" => "jev-1.13.0"}]}) end)
      assert {:ok, ["jev-1.13"]} = Jev.probe([@model])
      assert_received {:request, "GET", "/v1/models", ["Bearer test-jev-key"], _}
    end

    test "without a listing endpoint it falls back to a tiny decision" do
      capture(fn
        %{request_path: "/v1/models"} = conn ->
          conn |> Plug.Conn.put_status(404) |> Req.Test.json(%{})

        conn ->
          Req.Test.json(conn, %{"answers" => %{"ok" => %{"choice" => "yes"}}, "usage" => %{}})
      end)

      assert {:ok, ["jev-1.13"]} = Jev.probe([@model])
      assert_received {:request, "GET", "/v1/models", _, _}
      assert_received {:request, "POST", "/v1/systemone", _, _}
    end

    test "a rejected key is not ready" do
      capture(fn conn -> conn |> Plug.Conn.put_status(401) |> Req.Test.json(%{}) end)
      assert {:error, {:http, 401}} = Jev.probe([@model])
    end
  end
end
```

- [ ] **Step 3: Run to verify they fail**

Run: `cd helios && mix test test/helios/ai/provider/jev_test.exs`
Expected: FAIL (`Helios.AI.Provider.Jev` is undefined).

- [ ] **Step 4: Implement**

Create `helios/lib/helios/ai/provider/jev.ex`:

```elixir
defmodule Helios.AI.Provider.Jev do
  @moduledoc """
  TypeSafe's Jev decision model (`POST /v1/systemone`). A decision is one
  `choice` question: the moves are its criteria (id => label), the payload
  state is the `state`, the primer the instructions. The answer can only be
  one of the offered ids. Priced per input token; output is free.

  Readiness: `GET /v1/models` with the key; if the API has no listing
  (404), a two-option decision (a few input tokens) proves the key works.
  """
  @behaviour Helios.AI.Provider

  alias Helios.AI.HTTP

  @default_base_url "https://api.typesafe.ai"
  @probe_budget_ms 10_000

  @impl true
  def decide(model, prompt, deadline) do
    with {:ok, key} <- api_key() do
      body = %{
        model: model.model,
        state: prompt.state,
        questions: %{
          move: %{
            type: "choice",
            instructions: prompt.primer,
            criteria: Map.new(prompt.moves, &{&1.id, &1.label})
          }
        }
      }

      case HTTP.request(req(key, method: :post, url: "/v1/systemone", json: body), deadline) do
        {:ok, %Req.Response{status: 200, body: %{"answers" => %{"move" => %{"choice" => id}}} = body}} ->
          if Enum.any?(prompt.moves, &(&1.id == id)),
            do: {:ok, id, usage(body)},
            else: {:error, :invalid_move}

        {:ok, %Req.Response{status: 200}} ->
          {:error, :invalid_move}

        {:ok, %Req.Response{status: status}} ->
          {:error, {:http, status}}

        {:error, reason} ->
          {:error, reason}
      end
    end
  end

  @impl true
  def probe(models) do
    deadline = HTTP.now() + @probe_budget_ms
    ids = Enum.map(models, & &1.id)

    with {:ok, key} <- api_key() do
      case HTTP.request(req(key, method: :get, url: "/v1/models"), deadline) do
        {:ok, %Req.Response{status: 200}} -> {:ok, ids}
        {:ok, %Req.Response{status: 404}} -> probe_decision(key, hd(models), ids, deadline)
        {:ok, %Req.Response{status: status}} -> {:error, {:http, status}}
        {:error, reason} -> {:error, reason}
      end
    end
  end

  defp probe_decision(key, model, ids, deadline) do
    body = %{
      model: model.model,
      state: "ping",
      questions: %{ok: %{type: "choice", instructions: "Answer yes.", criteria: %{yes: nil, no: nil}}}
    }

    case HTTP.request(req(key, method: :post, url: "/v1/systemone", json: body), deadline) do
      {:ok, %Req.Response{status: 200}} -> {:ok, ids}
      {:ok, %Req.Response{status: status}} -> {:error, {:http, status}}
      {:error, reason} -> {:error, reason}
    end
  end

  defp usage(body) do
    %{
      input_tokens: get_in(body, ["usage", "input_tokens"]) || 0,
      output_tokens: get_in(body, ["usage", "output_tokens"]) || 0
    }
  end

  defp req(key, options) do
    [base_url: Keyword.get(config(), :base_url, @default_base_url), auth: {:bearer, key}]
    |> Keyword.merge(options)
    |> Keyword.merge(Keyword.get(config(), :req_options, []))
  end

  defp api_key do
    case Keyword.get(config(), :api_key) do
      key when is_binary(key) and key != "" -> {:ok, key}
      _missing -> {:error, :no_key}
    end
  end

  defp config, do: Application.get_env(:helios, __MODULE__, [])
end
```

- [ ] **Step 5: Run the tests**

Run: `cd helios && mix test test/helios/ai/provider/jev_test.exs`
Expected: PASS.

- [ ] **Step 6: Precommit and commit**

```bash
git add helios/lib/helios/ai/provider/jev.ex helios/config/test.exs helios/test/helios/ai/provider/jev_test.exs
git commit -m "feat(ai): Jev provider (TypeSafe systemone choice) with readiness probe

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 4: OpenRouter adapter

**Files:**
- Create: `helios/lib/helios/ai/provider/open_router.ex`
- Modify: `helios/config/test.exs`
- Test: `helios/test/helios/ai/provider/open_router_test.exs`

**Interfaces:**
- Consumes: `Helios.AI.HTTP.request/2`, `now/0`; `Helios.AI.Prompt.user_message/2` and the prompt shape from Task 1.
- Produces: `Helios.AI.Provider.OpenRouter.decide/3` and `probe/1`. Config: `config :helios, Helios.AI.Provider.OpenRouter, api_key, base_url (default "https://openrouter.ai/api/v1"), req_options`. Model entry options: `max_tokens` (default 16), `reasoning` (default false) and `format` (`:toon` default, or `:json`).

- [ ] **Step 1: Test config**

Append to `helios/config/test.exs`:

```elixir
config :helios, Helios.AI.Provider.OpenRouter,
  api_key: "test-openrouter-key",
  req_options: [plug: {Req.Test, Helios.AI.Provider.OpenRouter}]
```

- [ ] **Step 2: Write the failing tests**

Create `helios/test/helios/ai/provider/open_router_test.exs`:

```elixir
defmodule Helios.AI.Provider.OpenRouterTest do
  use ExUnit.Case, async: false

  alias Helios.AI.HTTP
  alias Helios.AI.Provider.OpenRouter

  @model %{id: "or-small", label: "Small", provider: :openrouter, model: "vendor/small", deadline_ms: 8_000}
  @prompt %{
    state: %{age: 1},
    primer: "You are an expert player.",
    moves: [
      %{id: "b0", label: "build Altar: free", card: "Altar", kind: "build", trade_options: 0},
      %{id: "d0", label: "discard Altar: +3 coins", card: "Altar", kind: "discard", trade_options: 0}
    ]
  }

  defp capture(response) do
    test = self()

    Req.Test.stub(OpenRouter, fn conn ->
      {:ok, body, conn} = Plug.Conn.read_body(conn)
      send(test, {:request, conn.method, conn.request_path, Plug.Conn.get_req_header(conn, "authorization"), body})
      response.(conn)
    end)
  end

  defp completion(content) do
    fn conn ->
      Req.Test.json(conn, %{
        "choices" => [%{"message" => %{"role" => "assistant", "content" => content}}],
        "usage" => %{"prompt_tokens" => 640, "completion_tokens" => 6}
      })
    end
  end

  test "decide/3 asks for a strict enum of move ids with a tiny token cap" do
    capture(completion(~s({"move":"b0"})))

    assert {:ok, "b0", %{input_tokens: 640, output_tokens: 6}} =
             OpenRouter.decide(@model, @prompt, HTTP.now() + 2_000)

    assert_received {:request, "POST", "/api/v1/chat/completions", ["Bearer test-openrouter-key"], body}
    body = Jason.decode!(body)

    assert body["model"] == "vendor/small"
    assert body["max_tokens"] == 16
    assert body["provider"] == %{"require_parameters" => true}
    refute Map.has_key?(body, "reasoning")

    assert [%{"role" => "system", "content" => "You are an expert player."}, %{"role" => "user", "content" => user}] =
             body["messages"]

    assert user =~ "State (TOON):\nage: 1"
    assert user =~ "b0: build Altar: free"
    assert user =~ "d0: discard Altar: +3 coins"

    assert %{
             "type" => "json_schema",
             "json_schema" => %{
               "strict" => true,
               "schema" => %{"properties" => %{"move" => %{"enum" => ["b0", "d0"]}}, "required" => ["move"]}
             }
           } = body["response_format"]
  end

  test "format: :json sends compact JSON state" do
    capture(completion(~s({"move":"b0"})))
    assert {:ok, "b0", _} = OpenRouter.decide(Map.put(@model, :format, :json), @prompt, HTTP.now() + 2_000)
    assert_received {:request, _, _, _, body}
    [_system, %{"content" => user}] = Jason.decode!(body)["messages"]
    assert user =~ ~s(State (JSON):\n{"age":1})
  end

  test "reasoning models opt in explicitly" do
    capture(completion(~s({"move":"d0"})))
    model = Map.merge(@model, %{reasoning: true, max_tokens: 400})
    assert {:ok, "d0", _} = OpenRouter.decide(model, @prompt, HTTP.now() + 2_000)
    assert_received {:request, _, _, _, body}
    assert %{"max_tokens" => 400, "reasoning" => %{"effort" => "low", "exclude" => true}} = Jason.decode!(body)
  end

  test "unparseable or unknown answers are :invalid_move" do
    for content <- ["b0", ~s({"move":"x9"}), ~s({"other":1}), ""] do
      capture(completion(content))
      assert {:error, :invalid_move} = OpenRouter.decide(@model, @prompt, HTTP.now() + 2_000), content
    end
  end

  test "HTTP errors are returned" do
    capture(fn conn -> conn |> Plug.Conn.put_status(402) |> Req.Test.json(%{}) end)
    assert {:error, {:http, 402}} = OpenRouter.decide(@model, @prompt, HTTP.now() + 2_000)
  end

  describe "probe/1" do
    defp catalogue(models, key \\ %{"limit_remaining" => nil}) do
      capture(fn
        %{request_path: "/api/v1/key"} = conn -> Req.Test.json(conn, %{"data" => key})
        %{request_path: "/api/v1/models"} = conn -> Req.Test.json(conn, %{"data" => models})
      end)
    end

    test "models that are listed and support structured outputs are ready" do
      catalogue([
        %{"id" => "vendor/small", "supported_parameters" => ["max_tokens", "structured_outputs"]},
        %{"id" => "vendor/plain", "supported_parameters" => ["max_tokens"]}
      ])

      plain = %{@model | id: "or-plain", model: "vendor/plain"}
      gone = %{@model | id: "or-gone", model: "vendor/gone"}

      ExUnit.CaptureLog.capture_log(fn ->
        assert {:ok, ["or-small"]} = OpenRouter.probe([@model, plain, gone])
      end)
    end

    test "an exhausted key is not ready" do
      catalogue([], %{"limit_remaining" => 0})
      assert {:error, :out_of_credit} = OpenRouter.probe([@model])
    end

    test "a rejected key is not ready" do
      capture(fn conn -> conn |> Plug.Conn.put_status(401) |> Req.Test.json(%{}) end)
      assert {:error, {:http, 401}} = OpenRouter.probe([@model])
    end
  end
end
```

- [ ] **Step 3: Run to verify they fail**

Run: `cd helios && mix test test/helios/ai/provider/open_router_test.exs`
Expected: FAIL (`Helios.AI.Provider.OpenRouter` is undefined).

- [ ] **Step 4: Implement**

Create `helios/lib/helios/ai/provider/open_router.ex`:

```elixir
defmodule Helios.AI.Provider.OpenRouter do
  @moduledoc """
  OpenRouter chat completions. A decision is a system message (the primer)
  plus one user message (`Prompt.user_message/2`: the state as TOON or JSON,
  per the model's `format`, and `id: label` move lines); the
  `response_format` is a strict JSON schema whose `move` is an enum of the
  offered ids, `max_tokens` is tiny and `provider.require_parameters` keeps
  routing on backends that honour structured output. Reasoning is off unless
  the model entry sets `reasoning: true`.

  Readiness: `GET /key` (valid key, credit left), then `GET /models`: a model
  is ready when it is listed with `structured_outputs` or `response_format`
  among its supported parameters.
  """
  @behaviour Helios.AI.Provider

  require Logger

  alias Helios.AI.{HTTP, Prompt}

  @default_base_url "https://openrouter.ai/api/v1"
  @probe_budget_ms 10_000

  @impl true
  def decide(model, prompt, deadline) do
    with {:ok, key} <- api_key() do
      case HTTP.request(req(key, method: :post, url: "/chat/completions", json: body(model, prompt)), deadline) do
        {:ok, %Req.Response{status: 200, body: body}} -> answer(body, prompt)
        {:ok, %Req.Response{status: status}} -> {:error, {:http, status}}
        {:error, reason} -> {:error, reason}
      end
    end
  end

  @impl true
  def probe(models) do
    deadline = HTTP.now() + @probe_budget_ms

    with {:ok, key} <- api_key(),
         :ok <- check_key(key, deadline),
         {:ok, supported} <- structured_models(key, deadline) do
      {ready, missing} = Enum.split_with(models, &MapSet.member?(supported, &1.model))

      for model <- missing,
          do: Logger.warning("OpenRouter model #{model.model} is unknown or lacks structured outputs")

      {:ok, Enum.map(ready, & &1.id)}
    end
  end

  defp body(model, prompt) do
    ids = Enum.map(prompt.moves, & &1.id)

    %{
      model: model.model,
      messages: [
        %{role: "system", content: prompt.primer},
        %{role: "user", content: Prompt.user_message(prompt, Map.get(model, :format, :toon))}
      ],
      response_format: %{
        type: "json_schema",
        json_schema: %{
          name: "move",
          strict: true,
          schema: %{
            type: "object",
            properties: %{move: %{type: "string", enum: ids}},
            required: ["move"],
            additionalProperties: false
          }
        }
      },
      max_tokens: Map.get(model, :max_tokens, 16),
      temperature: 0.2,
      provider: %{require_parameters: true}
    }
    |> then(fn body ->
      if Map.get(model, :reasoning, false),
        do: Map.put(body, :reasoning, %{effort: "low", exclude: true}),
        else: body
    end)
  end

  defp answer(%{"choices" => [%{"message" => %{"content" => content}} | _]} = body, prompt)
       when is_binary(content) do
    with {:ok, %{"move" => id}} <- Jason.decode(content),
         true <- Enum.any?(prompt.moves, &(&1.id == id)) do
      {:ok, id, usage(body)}
    else
      _invalid -> {:error, :invalid_move}
    end
  end

  defp answer(_body, _prompt), do: {:error, :invalid_move}

  defp usage(body) do
    %{
      input_tokens: get_in(body, ["usage", "prompt_tokens"]) || 0,
      output_tokens: get_in(body, ["usage", "completion_tokens"]) || 0
    }
  end

  defp check_key(key, deadline) do
    case HTTP.request(req(key, method: :get, url: "/key"), deadline) do
      {:ok, %Req.Response{status: 200, body: %{"data" => %{"limit_remaining" => left}}}}
      when is_number(left) and left <= 0 ->
        {:error, :out_of_credit}

      {:ok, %Req.Response{status: 200}} ->
        :ok

      {:ok, %Req.Response{status: status}} ->
        {:error, {:http, status}}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp structured_models(key, deadline) do
    case HTTP.request(req(key, method: :get, url: "/models"), deadline) do
      {:ok, %Req.Response{status: 200, body: %{"data" => models}}} when is_list(models) ->
        {:ok,
         for(
           %{"id" => id} = model <- models,
           params = model["supported_parameters"] || [],
           "structured_outputs" in params or "response_format" in params,
           into: MapSet.new(),
           do: id
         )}

      {:ok, %Req.Response{status: status}} ->
        {:error, {:http, status}}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp req(key, options) do
    [
      base_url: Keyword.get(config(), :base_url, @default_base_url),
      auth: {:bearer, key},
      headers: [{"x-title", "Seven Wonders"}]
    ]
    |> Keyword.merge(options)
    |> Keyword.merge(Keyword.get(config(), :req_options, []))
  end

  defp api_key do
    case Keyword.get(config(), :api_key) do
      key when is_binary(key) and key != "" -> {:ok, key}
      _missing -> {:error, :no_key}
    end
  end

  defp config, do: Application.get_env(:helios, __MODULE__, [])
end
```

Note: with `base_url: ".../api/v1"` and `url: "/chat/completions"`, Req joins them into `/api/v1/chat/completions`, which is the path the tests assert. If Req's joining differs (for example, it drops the base path), adjust `req/2` so the requested path is `/api/v1/...`. Don't change the tests.

- [ ] **Step 5: Run the tests**

Run: `cd helios && mix test test/helios/ai/provider/open_router_test.exs`
Expected: PASS.

- [ ] **Step 6: Precommit and commit**

```bash
git add helios/lib/helios/ai/provider/open_router.ex helios/config/test.exs \
  helios/test/helios/ai/provider/open_router_test.exs
git commit -m "feat(ai): OpenRouter provider with strict enum structured output

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 5: API keys, the allowlist file and `mix helios.ai.probe`

**Files:**
- Create: `helios/config/ai_models.exs`
- Modify: `helios/config/config.exs`, `helios/config/runtime.exs`
- Create: `helios/lib/mix/tasks/helios.ai.probe.ex`
- Test: `helios/test/mix/tasks/helios_ai_probe_test.exs`

**Interfaces:**
- Consumes: `Helios.AI.Readiness.probe_now/0`; `Helios.AI.Models.all/0`.
- Produces: `config/ai_models.exs`, which holds the real allowlist (imported by `config/config.exs`; read directly by Task 6).
- Produces: the `mix helios.ai.probe` task (readiness only), plus `Mix.Tasks.Helios.Ai.Probe.report/0 :: [String.t()]` (the lines it prints).

- [ ] **Step 1: HUMAN CHECKPOINT — the initial allowlist**

Stop and ask the user for:
- the OpenRouter models to offer. Each needs an OpenRouter slug such as `vendor/model`, a label, a starting `deadline_ms`, and optionally `reasoning: true` with `max_tokens`, or `format: :json`;
- whether the Jev entry should keep `deadline_ms: 5_000`.

Don't pick OpenRouter models yourself. The deadlines are starting values: Task 6's report suggests real ones.

- [ ] **Step 2: Config**

Create `helios/config/ai_models.exs` with the agreed list. Jev comes first, for example:

```elixir
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
```

In `helios/config/config.exs`:
- remove `models: []` from the `Helios.AI` block, keeping `probe_interval_ms`;
- add `import_config "ai_models.exs"` directly above the final `import_config "#{config_env()}.exs"`.

Keyword configs merge, so `config/test.exs` and `config/e2e.exs` (imported later) still override `models` with the fake entries. Run `cd helios && mix run -e 'IO.inspect(Helios.AI.Models.all() |> Enum.map(& &1.id))'` (dev) and `MIX_ENV=test mix run -e '…'` to confirm.

In `helios/config/runtime.exs`, after the `default_port … config :helios, HeliosWeb.Endpoint, http: …` block, add:

```elixir
# AI provider keys (server-side only). Missing keys make that provider's
# models unavailable. Tests use dummy keys and Req.Test stubs; the local-only
# live tests (`mix test --only live_ai`) read the env vars themselves.
if config_env() != :test do
  config :helios, Helios.AI.Provider.Jev, api_key: System.get_env("JEV_API_KEY")
  config :helios, Helios.AI.Provider.OpenRouter, api_key: System.get_env("OPENROUTER_API_KEY")
end
```

- [ ] **Step 3: Write the failing test**

Create `helios/test/mix/tasks/helios_ai_probe_test.exs`:

```elixir
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
```

- [ ] **Step 4: Run to verify it fails**

Run: `cd helios && mix test test/mix/tasks/helios_ai_probe_test.exs`
Expected: FAIL (module is undefined).

- [ ] **Step 5: Implement**

Create `helios/lib/mix/tasks/helios.ai.probe.ex`:

```elixir
defmodule Mix.Tasks.Helios.Ai.Probe do
  @shortdoc "Checks which configured AI models are ready (keys, credit, model listing)"
  @moduledoc """
  Runs the readiness probes against the configured providers with the real
  API keys from the environment and prints each model's status and every
  provider problem. For real decisions, latency and token counts run the
  live tests: `mix test --only live_ai`.

      JEV_API_KEY=… OPENROUTER_API_KEY=… mix helios.ai.probe
  """
  use Mix.Task

  alias Helios.AI.{Models, Readiness}

  @impl true
  def run(_args) do
    Mix.Task.run("app.start")
    Enum.each(report(), &Mix.shell().info/1)
  end

  @doc "The lines `run/1` prints."
  @spec report() :: [String.t()]
  def report do
    %{ready: ready, reasons: reasons} = Readiness.probe_now()

    models =
      for model <- Models.all(),
          do: "#{model.id}: #{if MapSet.member?(ready, model.id), do: "ready", else: "not ready"}"

    problems = for {provider, reason} <- reasons, do: "#{provider}: #{inspect(reason)}"
    models ++ problems ++ ["Run `mix test --only live_ai` for real decisions, latency and token counts."]
  end
end
```

- [ ] **Step 6: Run the tests**

Run: `cd helios && mix test test/mix/tasks/helios_ai_probe_test.exs`
Expected: PASS.

- [ ] **Step 7: Precommit and commit**

```bash
git add helios/config/ai_models.exs helios/config/config.exs helios/config/runtime.exs \
  helios/lib/mix/tasks/helios.ai.probe.ex helios/test/mix/tasks/helios_ai_probe_test.exs
git commit -m "feat(ai): provider keys, allowlist file and mix helios.ai.probe

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 6: Live provider tests (local only)

**Files:**
- Create: `helios/test/helios/ai/live_providers_test.exs`
- Modify: `helios/test/test_helper.exs`

**Interfaces:**
- Consumes:
  - `config/ai_models.exs` (Task 5);
  - `Helios.AI.Models.provider_module/1`;
  - `Helios.AI.Prompt.build/1`;
  - each adapter's `probe/1` and `decide/3`;
  - `Helios.Core`;
  - `Helios.GameDriver.core_funs/1`, `play_until/5` and `find_seed/4`.
- Produces: the `:live_ai` ExUnit tag (excluded by default) and its per-model report.

- [ ] **Step 1: Exclude the tag by default**

In `helios/test/test_helper.exs`, replace `ExUnit.start()` with:

```elixir
# :live_ai tests call real providers with real keys; run them locally with
# `JEV_API_KEY=… OPENROUTER_API_KEY=… mix test --only live_ai`.
ExUnit.start(exclude: [:live_ai])
```

- [ ] **Step 2: Write the live test**

Create `helios/test/helios/ai/live_providers_test.exs`:

```elixir
defmodule Helios.AI.LiveProvidersTest do
  @moduledoc """
  Real calls to every model in config/ai_models.exs. Local only (excluded by
  default):

      JEV_API_KEY=… OPENROUTER_API_KEY=… mix test --only live_ai
      LIVE_AI_MODEL=jev-1.13 LIVE_AI_RUNS=10 mix test --only live_ai

  Per model: the readiness probe (key, credit, listing), then real engine
  positions decided with a #{30_000} ms cap (OpenRouter models in both TOON
  and JSON). Every answer must be an offered id. Prints latency, token and
  deadline figures per model; models without a key are skipped.
  """
  use ExUnit.Case, async: false

  alias Helios.AI.{Models, Prompt}
  alias Helios.{Core, GameDriver}

  @moduletag :live_ai
  @moduletag timeout: :infinity

  @cap_ms 30_000
  @keys %{jev: "JEV_API_KEY", openrouter: "OPENROUTER_API_KEY"}
  @modules %{jev: Helios.AI.Provider.Jev, openrouter: Helios.AI.Provider.OpenRouter}

  # Real keys, no Req.Test plug, for the duration of this module.
  setup_all do
    for {provider, var} <- @keys do
      module = Map.fetch!(@modules, provider)
      original = Application.get_env(:helios, module, [])
      Application.put_env(:helios, module, api_key: System.get_env(var))
      on_exit(fn -> Application.put_env(:helios, module, original) end)
    end

    %{positions: positions()}
  end

  test "allowlisted models answer legal moves; latency and token report", %{positions: positions} do
    models = live_models()
    assert models != [], "no model selected (check LIVE_AI_MODEL and config/ai_models.exs)"

    results = Enum.map(models, &run_model(&1, positions))
    Enum.each(results, &IO.puts(report(&1)))

    for %{model: model, failures: failures} <- results do
      assert failures == [], "#{model.id}: #{inspect(failures, limit: 5)}"
    end
  end

  defp live_models do
    models =
      "../../../config/ai_models.exs"
      |> Path.expand(__DIR__)
      |> Config.Reader.read!()
      |> get_in([:helios, Helios.AI, :models])

    case System.get_env("LIVE_AI_MODEL") do
      nil -> models
      id -> Enum.filter(models, &(&1.id == id))
    end
  end

  defp runs, do: String.to_integer(System.get_env("LIVE_AI_RUNS", "3"))

  defp run_model(model, positions) do
    var = Map.fetch!(@keys, model.provider)

    if System.get_env(var) in [nil, ""] do
      %{model: model, skipped: "#{var} not set", samples: [], failures: []}
    else
      module = Models.provider_module(model)

      case module.probe([model]) do
        {:ok, [_ready]} -> sample(model, module, positions)
        {:ok, []} -> %{model: model, skipped: nil, samples: [], failures: [:not_listed]}
        {:error, reason} -> %{model: model, skipped: nil, samples: [], failures: [{:probe, reason}]}
      end
    end
  end

  defp sample(model, module, positions) do
    formats = if model.provider == :openrouter, do: [:toon, :json], else: [:json]

    samples =
      for {name, view} <- positions, format <- formats, _run <- 1..runs() do
        prompt = Prompt.build(view)
        started = System.monotonic_time(:millisecond)
        result = module.decide(Map.put(model, :format, format), prompt, started + @cap_ms)

        %{
          position: name,
          format: format,
          ms: System.monotonic_time(:millisecond) - started,
          result: result,
          legal?: legal?(result, prompt)
        }
      end

    failures = for s <- samples, !s.legal?, do: {s.position, s.format, s.result}
    %{model: model, skipped: nil, samples: samples, failures: failures}
  end

  defp legal?({:ok, id, _usage}, prompt), do: Enum.any?(prompt.moves, &(&1.id == id))
  defp legal?(_result, _prompt), do: false

  defp report(%{model: model, skipped: reason}) when is_binary(reason),
    do: "#{model.id}: skipped (#{reason})"

  defp report(%{model: model, samples: [], failures: failures}),
    do: "#{model.id}: not ready #{inspect(failures)}"

  defp report(%{model: model, samples: samples, failures: failures}) do
    ms = samples |> Enum.map(& &1.ms) |> Enum.sort()
    p95 = percentile(ms, 95)
    late = Enum.count(ms, &(&1 > model.deadline_ms))

    tokens_in =
      samples
      |> Enum.group_by(& &1.format)
      |> Enum.map_join(" / ", fn {format, group} -> "#{avg(group, :input_tokens)} (#{format})" end)

    """
    #{model.id}  runs=#{length(samples)}  ok=#{length(samples) - length(failures)}  failed=#{length(failures)}
      latency ms: min #{hd(ms)}  p50 #{percentile(ms, 50)}  p95 #{p95}  max #{List.last(ms)}
      tokens in: avg #{tokens_in}  out: avg #{avg(samples, :output_tokens)}
      configured deadline #{model.deadline_ms} ms -> #{late} would have timed out · suggested deadline ≈ #{suggest(p95)} ms (p95 × 1.5)
    """
  end

  defp avg(samples, key) do
    values = for %{result: {:ok, _id, usage}} <- samples, do: Map.get(usage, key, 0)
    if values == [], do: 0, else: div(Enum.sum(values), length(values))
  end

  defp percentile(sorted, p), do: Enum.at(sorted, min(length(sorted) - 1, div(p * length(sorted), 100)))

  defp suggest(p95), do: trunc(Float.ceil(p95 * 1.5 / 250) * 250)

  ## Positions: real engine views, deterministic seeds

  defp positions do
    ids = ["p1", "p2", "p3", "p4"]
    {:ok, ref} = Core.new_game(ids, :random, 7)
    {:ok, opening} = Core.view(ref, "p1")

    [
      {"age1-turn1", opening},
      {"age2-turn3", reach(ids, &match?(%{phase: %{kind: :choosing_cards, age: 2, turn: 3}}, &1))},
      {"age3-turn5", reach(ids, &match?(%{phase: %{kind: :choosing_cards, age: 3, turn: 5}}, &1))},
      {"build-from-discard", discard_turn()}
    ]
  end

  defp reach(ids, stop?) do
    {:ok, ref} = Core.new_game(ids, :random, 7)
    {view_fun, submit_fun} = GameDriver.core_funs(ref)
    strategies = Map.new(ids, &{&1, :build})
    {:reached, _view} = GameDriver.play_until(ids, strategies, view_fun, submit_fun, stop?)
    view_fun.("p1")
  end

  defp discard_turn do
    ids = ["p1", "p2", "p3"]
    wonders = [{"Halikarnassós", :b}, {"Gizah", :a}, {"Rhódos", :a}]
    strategies = %{"p1" => :wonder, "p2" => :build, "p3" => :build}
    stop? = &match?(%{phase: %{kind: :extra_turn, extra_turn_kind: :build_from_discard}}, &1)
    seed = GameDriver.find_seed(ids, wonders, strategies, stop?)
    {:ok, ref} = Core.new_game(ids, {:explicit, wonders}, seed)
    {view_fun, submit_fun} = GameDriver.core_funs(ref)
    {:reached, _view} = GameDriver.play_until(ids, strategies, view_fun, submit_fun, stop?)
    view_fun.("p1")
  end
end
```

- [ ] **Step 3: Check the default run skips it**

Run: `cd helios && mix test`
Expected: the suite passes and reports the live test as excluded (`1 excluded` or similar). No network access.

Run: `cd helios && mix test --only live_ai` with **no** keys set.
Expected: every model prints `skipped (… not set)` and the test passes. This proves the harness works without spending anything.

- [ ] **Step 4: Optional real run (user's keys)**

If the user provides keys, run `cd helios && JEV_API_KEY=… OPENROUTER_API_KEY=… mix test --only live_ai` and paste the report into the task summary. Report the observed latencies, the suggested deadlines and the TOON-vs-JSON token figures. Don't change `config/ai_models.exs` deadlines or formats without the user's agreement. Never commit keys or reports containing them.

- [ ] **Step 5: Precommit and commit**

```bash
git add helios/test/test_helper.exs helios/test/helios/ai/live_providers_test.exs
git commit -m "test(ai): local-only live provider tests with latency and token report

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```
