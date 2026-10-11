# Table Polish — Phase E: Wonder Selection Modes in Helios Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** The table owner picks how wonder boards are handed out ("Random board & side", "Random board, choose side", "Choose board"); in the two choosing modes the game opens on a setup screen where players claim a board and/or side and confirm, and Age I starts once everyone has confirmed. Setup survives refresh and server restart, replays start at Age I, and one e2e game exercises the whole flow.

**Architecture:** A `wonder_mode` column on `lobbies` (owner-only setter, `{:settings_changed}` on `lobby:<id>`). `Games.start_game` turns the mode into the engine's `wonders` term (`:choose_board | :choose_side`, or today's `:random` / config `{:explicit, …}`), persisted by `ActionCodec` as `%{"mode" => …}`. Claims and confirmations are ordinary engine actions submitted through the existing `Games.submit/3` → `GameServer` path and logged in `game_actions`. `GameLive` renders a new `HeliosWeb.GameComponents.WonderSetup` instead of the table and dock while `view.phase.kind == :setup` (when the engine view has `players: []`). `Games.replay/2` opens its first frame only once the phase leaves setup.

**Tech Stack:** Phoenix 1.8 / LiveView 1.1, Ecto (SQLite, portable queries), Tailwind 4.1, ExUnit + LazyHTML, Playwright. Engine: Rustler NIF from phase D (no core changes here).

**Spec:** `docs/superpowers/specs/2026-10-11-table-polish/2026-10-11-wonder-modes-design.md` (section "Helios", plus the Helios/e2e parts of "Testing"). Cross-phase contract: `docs/superpowers/plans/2026-10-11-table-polish/README.md` ("D → E (NIF terms)" and "A → B").

## Global Constraints

- Read `helios/AGENTS.md` before touching `helios/`.
- **Prerequisites:** phases A–D are merged. Phase D supplies exactly the NIF terms listed in the README "D → E" section; phase A supplies `top_bar/1`'s `replay` attr and the phone row `#pass-row`; phase B supplies `#discard-pile` (shown only when `discard_count > 0`) and the `discard_pile`/`discard_count` frame fields. If any of these differ from the README, stop and ask.
- Elixir never re-implements rules. Which board is free, who may claim what and when setup ends are decided by the engine; the UI only disables buttons according to `view.setup` and flashes engine errors.
- No new game broadcasts: setup updates use the existing `{:game_updated, seq}` → re-fetch flow. `{:settings_changed}` is a lobby-topic message.
- Decoding never creates atoms: modes, sides and action types map through whitelists.
- Kept ids (unchanged): `#top-bar` (+ `data-phase`, `data-turn-key`), `#age-label`, `#turn-label`, `#pass-row`, `#pass-direction`, `#waiting-for`, `#waiting-count`, `#connections` / `#connection-<id>`, `#table`, `#dock`, `#hand`, `#my-board`, `#start-game`, `#start-blocker`, `#game-panel`. New: `#wonder-mode`, `#wonder-mode-random|choose_side|choose_board`, `#wonder-setup`, `#setup-boards`, `#setup-wonder-<slug>`, `#claim-<slug>`, `#side-<slug>-a|b`, `#setup-side-a|b`, `#setup-seats` / `#setup-seat-<id>`, `#confirm-wonder`, `#setup-waiting`. `<slug>` is `GameAssets.wonder_slug/1` (ASCII city name: `gizah`, `rhodos`, `ephesos`, `alexandria`, `babylon`, `olympia`, `halikarnassus`).
- Persisted action maps keep the existing `"type"` key (the spec's `"kind"` in the codec example is read as `"type"`, the key every stored action already uses).
- Ecto queries stay portable (no SQLite-only SQL). Only one active game per lobby: tests create each game in a fresh lobby or finish the previous one.
- Touch targets ≥ 44×44 CSS px (`pointer-coarse:min-h-11`); no horizontal scroll at ≥ 360px.
- Stage explicit paths only; never stage `helios/config/config.exs` / `helios/config/dev.exs`.
- `cd helios && mix precommit` before each commit. Commit messages end with `Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>`.

## Review Focus

- A setup view (`players: []`) must never crash mount, refresh, `show_card`, `submit` or the top bar, and must render neither `#table` nor `#dock`. (Test in Task 7, "forged setup events …", plus Task 5 top bar test.)
- The config `wonders:` override applies only in `random` mode; `choose_side` / `choose_board` games persist `%{"mode" => …}` and open in setup even when the override is set. (Test in Task 4.)
- Two players racing for one board: the second sees "‹Board› was just taken", nothing is logged for them, and the first player keeps the board. (Test in Task 7.)
- A `choose_board` game's replay frames start at Age I turn 1, number one per turn plus the final table, and no setup action is ever a frame's `my_choice`. (Test in Task 8.)
- Only the owner can change the mode; a guest's forged `set_wonder_mode` and unknown mode strings are rejected without changing the row or creating atoms. (Tests in Tasks 1 and 2.)

---

### Task 1: Lobby `wonder_mode` setting

**Files:**
- Create: `helios/priv/repo/migrations/<timestamp>_add_wonder_mode_to_lobbies.exs` (generated)
- Modify: `helios/lib/helios/lobbies/lobby.ex`, `helios/lib/helios/lobbies.ex`
- Test: `helios/test/helios/lobbies_test.exs`

**Interfaces:**
- Produces: `Lobby` field `wonder_mode :: String.t()` (default `"random"`), `Lobby.wonder_modes/0 :: ["random", "choose_side", "choose_board"]`, `Lobby.wonder_mode_changeset/2`.
- Produces: `Lobbies.set_wonder_mode(scope, lobby, mode) :: {:ok, Lobby.t()} | {:error, :not_leader | :invalid_wonder_mode}`; broadcasts `{:settings_changed}` on `"lobby:<id>"`. `Lobbies.error_message(:invalid_wonder_mode)`.

- [ ] **Step 1: Write the failing tests**

In `helios/test/helios/lobbies_test.exs`, add to the `error_message/1` test:

```elixir
      assert Lobbies.error_message(:invalid_wonder_mode) == "That board mode does not exist"
```

and add a new `describe` block after `describe "decline/2"` (it uses the file's existing `owner_and_lobby/0` helper):

```elixir
  describe "set_wonder_mode/3" do
    test "the owner sets the mode, which is persisted and broadcast" do
      %{lobby: lobby, scope: scope} = owner_and_lobby()
      assert lobby.wonder_mode == "random"
      Phoenix.PubSub.subscribe(Helios.PubSub, Lobbies.lobby_topic(lobby.id))

      for mode <- ~w(choose_side choose_board random) do
        assert {:ok, %Lobby{wonder_mode: ^mode}} = Lobbies.set_wonder_mode(scope, lobby, mode)
        assert_receive {:settings_changed}
        assert Repo.get!(Lobby, lobby.id).wonder_mode == mode
      end
    end

    test "only the owner may change it" do
      %{owner: owner, lobby: lobby} = owner_and_lobby()
      guest = player_fixture()
      invite_fixture(lobby, owner, guest)
      Phoenix.PubSub.subscribe(Helios.PubSub, Lobbies.lobby_topic(lobby.id))

      assert {:error, :not_leader} =
               Lobbies.set_wonder_mode(Scope.for_user(guest), lobby, "choose_board")

      assert Repo.get!(Lobby, lobby.id).wonder_mode == "random"
      refute_received {:settings_changed}
    end

    test "unknown modes are rejected without creating atoms" do
      %{lobby: lobby, scope: scope} = owner_and_lobby()
      name = "never_a_mode_#{System.unique_integer([:positive])}"

      for mode <- [name, "", nil, 3, "Random"] do
        assert Lobbies.set_wonder_mode(scope, lobby, mode) == {:error, :invalid_wonder_mode},
               "accepted #{inspect(mode)}"
      end

      assert Repo.get!(Lobby, lobby.id).wonder_mode == "random"
      assert_raise ArgumentError, fn -> String.to_existing_atom(name) end
    end
  end
```

- [ ] **Step 2: Run to verify they fail**

Run: `cd helios && mix test test/helios/lobbies_test.exs`
Expected: FAIL (`wonder_mode` key missing on `%Lobby{}`, `Lobbies.set_wonder_mode/3` undefined, no `error_message(:invalid_wonder_mode)` clause).

- [ ] **Step 3: Migration**

Run: `cd helios && mix ecto.gen.migration add_wonder_mode_to_lobbies`

Replace the generated file's body with:

```elixir
defmodule Helios.Repo.Migrations.AddWonderModeToLobbies do
  use Ecto.Migration

  def change do
    alter table(:lobbies) do
      add :wonder_mode, :string, null: false, default: "random"
    end
  end
end
```

- [ ] **Step 4: Schema**

`helios/lib/helios/lobbies/lobby.ex` becomes:

```elixir
defmodule Helios.Lobbies.Lobby do
  @moduledoc """
  A table. Its `id` is the public `game_id`; its owner is its leader forever.
  `wonder_mode` says how the next game hands out wonder boards.
  """
  use Ecto.Schema
  import Ecto.Changeset

  alias Helios.Accounts.User

  @wonder_modes ~w(random choose_side choose_board)

  @type t :: %__MODULE__{}

  @primary_key {:id, :binary_id, autogenerate: true}
  schema "lobbies" do
    field :wonder_mode, :string, default: "random"
    belongs_to :owner, User

    timestamps(type: :utc_datetime)
  end

  @doc "Every valid `wonder_mode`, default first."
  def wonder_modes, do: @wonder_modes

  def wonder_mode_changeset(lobby, attrs) do
    lobby
    |> cast(attrs, [:wonder_mode])
    |> validate_required([:wonder_mode])
    |> validate_inclusion(:wonder_mode, @wonder_modes)
  end
end
```

- [ ] **Step 5: `Lobbies.set_wonder_mode/3`**

In `helios/lib/helios/lobbies.ex`, extend the moduledoc's lobby topic line:

```elixir
    * `"lobby:<lobby_id>"` — `{:members_changed}`, `{:declined, %User{}}`, `{:settings_changed}`
```

Add after `decline/2`:

```elixir
  @doc """
  The owner picks how the next game hands out wonder boards (`Lobby.wonder_modes/0`).
  Allowed while a game runs; it applies to the next one. Broadcasts `{:settings_changed}`.
  """
  @spec set_wonder_mode(Scope.t(), Lobby.t(), term()) ::
          {:ok, Lobby.t()} | {:error, :not_leader | :invalid_wonder_mode}
  def set_wonder_mode(%Scope{user: %User{} = user}, %Lobby{} = lobby, mode) do
    with :ok <- ensure_leader(lobby, user),
         {:ok, lobby} <-
           lobby |> Lobby.wonder_mode_changeset(%{wonder_mode: mode}) |> Repo.update() do
      broadcast(lobby_topic(lobby.id), {:settings_changed})
      {:ok, lobby}
    else
      {:error, %Ecto.Changeset{}} -> {:error, :invalid_wonder_mode}
      {:error, _reason} = error -> error
    end
  end
```

Add next to the other `error_message/1` clauses:

```elixir
  def error_message(:invalid_wonder_mode), do: "That board mode does not exist"
```

- [ ] **Step 6: Run and commit**

Run: `cd helios && mix test test/helios/lobbies_test.exs && mix precommit`
Expected: PASS.

```bash
git add helios/priv/repo/migrations/*_add_wonder_mode_to_lobbies.exs helios/lib/helios/lobbies/lobby.ex helios/lib/helios/lobbies.ex helios/test/helios/lobbies_test.exs
git commit -m "feat(lobby): owner-only wonder_mode setting" -m "Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 2: Wonder mode control in the lobby

**Files:**
- Modify: `helios/lib/helios_web/components/lobby_game_panel.ex`, `helios/lib/helios_web/live/lobby_live.ex`
- Test: `helios/test/helios_web/live/lobby_live_test.exs`

**Interfaces:**
- Consumes: `Lobbies.set_wonder_mode/3`, `Lobbies.fetch_lobby/1`, `{:settings_changed}` (Task 1).
- Produces: `LobbyGamePanel.game_panel/1` attr `wonder_mode` (string, default `"random"`); `LobbyGamePanel.wonder_mode_label/1`. Owner: `div#wonder-mode[role=group][data-mode]` with `button#wonder-mode-<mode>[aria-pressed]` (`phx-click="set_wonder_mode"`, `phx-value-mode`). Guests: read-only `p#wonder-mode[data-mode]`. `LobbyLive` events `set_wonder_mode`, message `{:settings_changed}`.

- [ ] **Step 1: Write the failing tests**

Append to `helios/test/helios_web/live/lobby_live_test.exs`, before `describe "access"`:

```elixir
  describe "wonder mode" do
    test "the owner switches the mode; guests see it read-only and live", %{conn: conn} do
      a = player_with_lobby(conn)
      b = player_with_lobby(build_conn())
      invite_fixture(a.lobby, a.user, b.user)

      {:ok, view_a, _} = live(a.conn, ~p"/lobby/#{a.lobby.id}")
      {:ok, view_b, _} = live(b.conn, ~p"/lobby/#{a.lobby.id}")

      assert has_element?(view_a, "#wonder-mode-random[aria-pressed='true']")
      assert has_element?(view_a, "#wonder-mode-choose_side", "Random board, choose side")
      assert has_element?(view_a, "#wonder-mode-choose_board[aria-pressed='false']", "Choose board")
      assert has_element?(view_b, "#wonder-mode[data-mode='random']")
      refute has_element?(view_b, "#wonder-mode-random")

      view_a |> element("#wonder-mode-choose_board") |> render_click()

      assert has_element?(view_a, "#wonder-mode-choose_board[aria-pressed='true']")
      assert has_element?(view_a, "#wonder-mode-random[aria-pressed='false']")
      assert {:ok, %{wonder_mode: "choose_board"}} = Lobbies.fetch_lobby(a.lobby.id)
      assert has_element?(view_b, "#wonder-mode[data-mode='choose_board']", "Choose board")
    end

    test "forged mode changes are rejected server-side", %{conn: conn} do
      a = player_with_lobby(conn)
      b = player_with_lobby(build_conn())
      invite_fixture(a.lobby, a.user, b.user)

      {:ok, view_a, _} = live(a.conn, ~p"/lobby/#{a.lobby.id}")
      {:ok, view_b, _} = live(b.conn, ~p"/lobby/#{a.lobby.id}")

      render_hook(view_b, "set_wonder_mode", %{"mode" => "choose_board"})
      assert has_element?(view_b, "#flash-error", "Only the table leader can do that")

      render_hook(view_a, "set_wonder_mode", %{"mode" => "chaos"})
      assert has_element?(view_a, "#flash-error", "That board mode does not exist")

      render_hook(view_a, "set_wonder_mode", %{})
      assert has_element?(view_a, "#wonder-mode-random[aria-pressed='true']")
      assert {:ok, %{wonder_mode: "random"}} = Lobbies.fetch_lobby(a.lobby.id)
    end
  end
```

- [ ] **Step 2: Run to verify they fail**

Run: `cd helios && mix test test/helios_web/live/lobby_live_test.exs`
Expected: FAIL (no `#wonder-mode-random`; `set_wonder_mode` event has no clause → the LiveView crashes).

- [ ] **Step 3: The control in `LobbyGamePanel`**

In `helios/lib/helios_web/components/lobby_game_panel.ex`, update the moduledoc and add above `attr :owner?`:

```elixir
  @wonder_modes [
    {"random", "Random board & side"},
    {"choose_side", "Random board, choose side"},
    {"choose_board", "Choose board"}
  ]

  @doc "Label for a lobby `wonder_mode`."
  def wonder_mode_label(mode) do
    case List.keyfind(@wonder_modes, mode, 0) do
      {_mode, label} -> label
      nil -> mode
    end
  end
```

Add the attr next to the others:

```elixir
  attr :wonder_mode, :string, default: "random"
```

At the top of `game_panel/1`, before `~H`:

```elixir
    assigns = assign(assigns, :wonder_modes, @wonder_modes)
```

Insert between the `#game-in-progress` div and the owner's Start row (`<div :if={@owner?} class="flex flex-wrap items-center gap-3">`):

```heex
      <div :if={@owner?} class="flex flex-col gap-1.5">
        <span id="wonder-mode-label" class="text-sm font-semibold text-zinc-700">
          Wonder boards
        </span>
        <div
          id="wonder-mode"
          role="group"
          aria-labelledby="wonder-mode-label"
          data-mode={@wonder_mode}
          class="flex flex-col overflow-hidden rounded-lg bg-white ring-1 ring-zinc-300 sm:flex-row sm:self-start"
        >
          <button
            :for={{mode, label} <- @wonder_modes}
            id={"wonder-mode-#{mode}"}
            type="button"
            phx-click="set_wonder_mode"
            phx-value-mode={mode}
            aria-pressed={to_string(@wonder_mode == mode)}
            class={[
              "px-3 py-2 text-sm font-semibold transition pointer-coarse:min-h-11",
              if(@wonder_mode == mode,
                do: "bg-zinc-900 text-white",
                else: "text-zinc-800 hover:bg-zinc-100"
              )
            ]}
          >
            {label}
          </button>
        </div>
      </div>
      <p :if={!@owner?} id="wonder-mode" data-mode={@wonder_mode} class="text-sm text-zinc-700">
        Wonder boards: <span class="font-semibold">{wonder_mode_label(@wonder_mode)}</span>
      </p>
```

- [ ] **Step 4: `LobbyLive` wiring**

In `helios/lib/helios_web/live/lobby_live.ex`, pass the mode to the panel:

```heex
        <.game_panel
          owner?={@owner?}
          active_game={@active_game}
          seated?={@seated?}
          start_blocker={@start_blocker}
          wonder_mode={@lobby.wonder_mode}
        />
```

Add after the `"start_game"` event clause:

```elixir
  def handle_event("set_wonder_mode", %{"mode" => mode}, socket) do
    %{current_scope: scope, lobby: lobby} = socket.assigns

    case Lobbies.set_wonder_mode(scope, lobby, mode) do
      {:ok, lobby} -> {:noreply, assign(socket, :lobby, lobby)}
      {:error, reason} -> {:noreply, put_flash(socket, :error, Lobbies.error_message(reason))}
    end
  end

  def handle_event("set_wonder_mode", _params, socket) do
    {:noreply, put_flash(socket, :error, Lobbies.error_message(:invalid_wonder_mode))}
  end
```

Add after the `{:members_changed}` `handle_info/2` clause:

```elixir
  # The owner changed a table setting (possibly in another tab): reload the row.
  def handle_info({:settings_changed}, socket) do
    case Lobbies.fetch_lobby(socket.assigns.lobby.id) do
      {:ok, lobby} -> {:noreply, assign(socket, :lobby, lobby)}
      {:error, _reason} -> {:noreply, socket}
    end
  end
```

(`start_game` already passes `socket.assigns.lobby`, so it now carries the current mode into `Games.start_game/2`.)

- [ ] **Step 5: Run and commit**

Run: `cd helios && mix test test/helios_web/live/lobby_live_test.exs test/helios_web/live/lobby_live_game_test.exs && mix precommit`
Expected: PASS.

```bash
git add helios/lib/helios_web/components/lobby_game_panel.ex helios/lib/helios_web/live/lobby_live.ex helios/test/helios_web/live/lobby_live_test.exs
git commit -m "feat(lobby): wonder mode segmented control for the owner" -m "Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 3: Setup terms at the boundary — codec, Core types, claim parsing, errors

**Files:**
- Modify: `helios/lib/helios/core.ex`, `helios/lib/helios/games/action_codec.ex`, `helios/lib/helios/games/choice.ex`, `helios/lib/helios/games.ex` (`error_message/1` only)
- Test: `helios/test/helios/games/action_codec_test.exs`, `helios/test/helios/games/choice_test.exs`, `helios/test/helios/games_test.exs`, `helios/test/helios/core_shapes_test.exs`

**Interfaces:**
- Consumes (phase D, README "D → E"): `Core.new_game/3` with `:choose_board | :choose_side`; actions `{:claim_wonder, %{wonder, side}}`, `:confirm_wonder`; errors `:unknown_wonder | :wonder_taken | :not_your_wonder | :no_claim | :already_confirmed`; setup view shape.
- Produces: `ActionCodec.encode/decode` for `claim_wonder` (`%{"type" => "claim_wonder", "wonder" => name, "side" => "a" | "b"}`) and `confirm_wonder` (`%{"type" => "confirm_wonder"}`); `ActionCodec.encode_wonders/decode_wonders` for `%{"mode" => "choose_board" | "choose_side"}`; `Choice.parse_claim(params) :: {:ok, {:claim_wonder, …}} | {:error, :invalid_choice}`; `Games.error_message/1` copy for the five setup errors.

- [ ] **Step 1: Write the failing tests**

`helios/test/helios/games/action_codec_test.exs` — extend `@actions`:

```elixir
    {:build_from_discard, "Library"},
    {:claim_wonder, %{wonder: "Rhódos", side: :b}},
    :confirm_wonder
  ]
```

and add:

```elixir
  test "setup actions encode with the existing type key" do
    assert ActionCodec.encode({:claim_wonder, %{wonder: "Gizah", side: :a}}) ==
             %{"type" => "claim_wonder", "wonder" => "Gizah", "side" => "a"}

    assert ActionCodec.encode(:confirm_wonder) == %{"type" => "confirm_wonder"}

    for bad <- [
          %{"type" => "claim_wonder", "wonder" => "Gizah", "side" => "c"},
          %{"type" => "claim_wonder", "wonder" => 5, "side" => "a"},
          %{"type" => "claim_wonder", "side" => "a"}
        ] do
      assert ActionCodec.decode(bad) == {:error, :invalid_action}, "accepted #{inspect(bad)}"
    end
  end

  test "setup wonder modes round-trip; unknown modes are rejected" do
    for mode <- [:choose_board, :choose_side] do
      encoded = ActionCodec.encode_wonders(mode)
      assert encoded == %{"mode" => Atom.to_string(mode)}
      assert encoded |> through_json() |> ActionCodec.decode_wonders() == {:ok, mode}
    end

    assert ActionCodec.decode_wonders(%{"mode" => "random"}) == {:error, :invalid_wonders}
    assert ActionCodec.decode_wonders(%{"mode" => "chaos"}) == {:error, :invalid_wonders}
  end
```

`helios/test/helios/games/choice_test.exs` — add:

```elixir
  describe "parse_claim/1" do
    test "a wonder name and a whitelisted side" do
      assert Choice.parse_claim(%{"wonder" => "Gizah", "side" => "b"}) ==
               {:ok, {:claim_wonder, %{wonder: "Gizah", side: :b}}}
    end

    test "rejects unknown sides and missing or non-string wonders" do
      for params <- [
            %{"wonder" => "Gizah", "side" => "c"},
            %{"wonder" => "Gizah"},
            %{"wonder" => 3, "side" => "a"},
            %{}
          ] do
        assert Choice.parse_claim(params) == {:error, :invalid_choice}, "accepted #{inspect(params)}"
      end
    end
  end
```

`helios/test/helios/games_test.exs` — in "has specific copy for every engine and context error", add to the reason list (before `{:setup_failed, :anything}`):

```elixir
            :unknown_wonder,
            :wonder_taken,
            :not_your_wonder,
            :no_claim,
            :already_confirmed,
```

and after the existing exact assertions:

```elixir
      assert Games.error_message(:not_your_wonder) == "That is not your board"
      assert Games.error_message(:no_claim) == "Pick a board first"
```

`helios/test/helios/core_shapes_test.exs` — a contract check of phase D's terms (expected to pass already; if it fails, phase D does not match the README contract — stop and ask):

```elixir
  test "view/2 and submit/3 during a choose_board setup" do
    {:ok, ref} = Core.new_game(@players, :choose_board, 1)
    assert {:ok, view} = Core.view(ref, "1")

    assert %{players: [], hand: [], west: "3", east: "2", my_pending: nil, scores: nil} = view
    assert %{discard_pile: nil, discard_count: 0} = view
    assert %{kind: :setup, age: 1, turn: 1, extra_turn_player: nil} = view.phase
    assert view.submitted == [{"1", false}, {"2", false}, {"3", false}]

    assert %{mode: :choose_board, available: available, seats: seats} = view.setup
    assert length(available) == 7
    assert [%{player: "1", wonder: nil, side: nil, confirmed: false} | _] = seats

    assert :ok = Core.submit(ref, "1", {:claim_wonder, %{wonder: "Gizah", side: :b}})

    assert {:error, :wonder_taken} =
             Core.submit(ref, "2", {:claim_wonder, %{wonder: "Gizah", side: :a}})

    assert {:error, :action_not_allowed_now} = Core.submit(ref, "2", {:discard, "Altar"})
    assert {:error, :no_claim} = Core.submit(ref, "2", :confirm_wonder)
  end
```

and in the existing "view/2 at the start of a game" test add `assert view.setup == nil`.

- [ ] **Step 2: Run to verify they fail**

Run: `cd helios && mix test test/helios/games/action_codec_test.exs test/helios/games/choice_test.exs test/helios/games_test.exs test/helios/core_shapes_test.exs`
Expected: FAIL in the codec, choice and error-message tests (`FunctionClauseError` in `encode/1` / `encode_wonders/1`, `parse_claim/1` undefined, fallback copy for the new reasons). The core-shapes tests PASS.

- [ ] **Step 3: `ActionCodec`**

In `helios/lib/helios/games/action_codec.ex`, add after `@sides`:

```elixir
  @modes %{"choose_board" => :choose_board, "choose_side" => :choose_side}
```

Replace the two types:

```elixir
  @type action ::
          {:build, %{card: String.t(), payment: payment()}}
          | {:build_wonder_stage, %{card: String.t(), payment: payment()}}
          | {:discard, String.t()}
          | {:build_free, String.t()}
          | {:build_from_discard, String.t()}
          | {:claim_wonder, %{wonder: String.t(), side: :a | :b}}
          | :confirm_wonder
  @type wonders :: :random | :choose_board | :choose_side | {:explicit, [{String.t(), :a | :b}]}
```

Add after the second `encode/1` clause:

```elixir
  def encode({:claim_wonder, %{wonder: wonder, side: side}})
      when is_binary(wonder) and side in [:a, :b] do
    %{"type" => "claim_wonder", "wonder" => wonder, "side" => Atom.to_string(side)}
  end

  def encode(:confirm_wonder), do: %{"type" => "confirm_wonder"}
```

Add before `def decode(_other)`:

```elixir
  def decode(%{"type" => "claim_wonder", "wonder" => wonder, "side" => side})
      when is_binary(wonder) and is_map_key(@sides, side) do
    {:ok, {:claim_wonder, %{wonder: wonder, side: Map.fetch!(@sides, side)}}}
  end

  def decode(%{"type" => "confirm_wonder"}), do: {:ok, :confirm_wonder}
```

Add after `def encode_wonders(:random), do: nil`:

```elixir
  def encode_wonders(mode) when mode in [:choose_board, :choose_side],
    do: %{"mode" => Atom.to_string(mode)}
```

Add before `def decode_wonders(_other)`:

```elixir
  def decode_wonders(%{"mode" => mode}) when is_map_key(@modes, mode),
    do: {:ok, Map.fetch!(@modes, mode)}
```

Update the moduledoc's first paragraph to mention setup: "Converts engine action terms (card actions and the setup actions `claim_wonder` / `confirm_wonder`) and wonder selections (`nil` = random, `%{"mode" => …}`, `%{"explicit" => …}`) to JSON-safe maps …".

- [ ] **Step 4: `Helios.Core` types**

In `helios/lib/helios/core.ex` replace `@type action`, `@type wonders` and extend `@type action_error`:

```elixir
  @type action ::
          {:build, %{card: String.t(), payment: payment()}}
          | {:build_wonder_stage, %{card: String.t(), payment: payment()}}
          | {:discard, String.t()}
          | {:build_free, String.t()}
          | {:build_from_discard, String.t()}
          | {:claim_wonder, %{wonder: String.t(), side: :a | :b}}
          | :confirm_wonder
  @type wonders :: :random | :choose_board | :choose_side | {:explicit, [{String.t(), :a | :b}]}
```

```elixir
          | :action_not_allowed_now
          | :unknown_wonder
          | :wonder_taken
          | :not_your_wonder
          | :no_claim
          | :already_confirmed
          | :lock_fail
```

In the `view/2` doc add: "During setup (`phase.kind == :setup`) `players` and `hand` are empty and `setup` holds `%{mode, seats, available}`; otherwise `setup` is nil."

- [ ] **Step 5: `Choice.parse_claim/1`**

In `helios/lib/helios/games/choice.ex` add after `@empty_payment`:

```elixir
  @sides %{"a" => :a, "b" => :b}
```

and after `parse/1`:

```elixir
  @doc """
  A setup claim as sent by the browser (`wonder` name, side `"a"` / `"b"`).
  Whether the board may be claimed is the engine's decision.
  """
  @spec parse_claim(map()) ::
          {:ok, {:claim_wonder, %{wonder: String.t(), side: :a | :b}}} | {:error, :invalid_choice}
  def parse_claim(%{"wonder" => wonder, "side" => side})
      when is_binary(wonder) and is_map_key(@sides, side) do
    {:ok, {:claim_wonder, %{wonder: wonder, side: Map.fetch!(@sides, side)}}}
  end

  def parse_claim(_params), do: {:error, :invalid_choice}
```

- [ ] **Step 6: Error copy**

In `helios/lib/helios/games.ex`, after `def error_message(:action_not_allowed_now)`:

```elixir
  def error_message(:unknown_wonder), do: "That board does not exist"
  def error_message(:wonder_taken), do: "That board was just taken"
  def error_message(:not_your_wonder), do: "That is not your board"
  def error_message(:no_claim), do: "Pick a board first"
  def error_message(:already_confirmed), do: "You already confirmed your board"
```

- [ ] **Step 7: Run and commit**

Run: `cd helios && mix test test/helios/games/action_codec_test.exs test/helios/games/choice_test.exs test/helios/games_test.exs test/helios/core_shapes_test.exs && mix precommit`
Expected: PASS.

```bash
git add helios/lib/helios/core.ex helios/lib/helios/games/action_codec.ex helios/lib/helios/games/choice.ex helios/lib/helios/games.ex helios/test/helios/games/action_codec_test.exs helios/test/helios/games/choice_test.exs helios/test/helios/games_test.exs helios/test/helios/core_shapes_test.exs
git commit -m "feat(games): codec, types and copy for wonder setup actions" -m "Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 4: Starting a game in a wonder mode

**Files:**
- Modify: `helios/lib/helios/games.ex` (`start_game/2`, `configured_wonders`), `helios/test/support/games_fixtures.ex`, `helios/test/support/game_driver.ex`
- Test: `helios/test/helios/games_test.exs`

**Interfaces:**
- Consumes: `lobby.wonder_mode` (Task 1), codec (Task 3).
- Produces: `Games.start_game/2` honours the lobby mode. Test support: `game_fixture(users, selection: :choose_board | :choose_side | {:explicit, …})` (default unchanged: explicit default wonders); `GameDriver.finish_setup(players, view_fun, submit_fun) :: :ok` — every seat claims side A (first `available` board in `choose_board`, its drawn board in `choose_side`) and confirms.

- [ ] **Step 1: Write the failing tests**

In `helios/test/helios/games_test.exs`, add `alias Helios.Games.Replay` to the aliases (`alias Helios.Games.{Game, Replay}`). In "uses the configured explicit wonders and persists them", add as the first line:

```elixir
      assert lobby.wonder_mode == "random"
```

Add to `describe "start_game/2"`:

```elixir
    test "choose_side starts in setup with drawn boards and ignores the wonders override", %{
      owner: owner,
      guests: [b, c | _],
      lobby: lobby
    } do
      put_games_config(wonders: default_wonders())
      connect_to_lobby(lobby, [b, c])
      {:ok, lobby} = Lobbies.set_wonder_mode(Scope.for_user(owner), lobby, "choose_side")

      assert {:ok, game} = Games.start_game(Scope.for_user(owner), lobby)
      assert game.wonders == %{"mode" => "choose_side"}

      {:ok, view} = Games.view(game.id, owner.id)
      assert %{phase: %{kind: :setup}, players: [], setup: %{mode: :choose_side}} = view
      assert view.setup.available == []
      assert length(view.setup.seats) == 3

      assert Enum.all?(view.setup.seats, fn seat ->
               is_binary(seat.wonder) and is_nil(seat.side) and not seat.confirmed
             end)
    end

    test "choose_board starts in setup with every board free", %{
      owner: owner,
      guests: [b, c | _],
      lobby: lobby
    } do
      put_games_config(wonders: default_wonders())
      connect_to_lobby(lobby, [b, c])
      {:ok, lobby} = Lobbies.set_wonder_mode(Scope.for_user(owner), lobby, "choose_board")

      assert {:ok, game} = Games.start_game(Scope.for_user(owner), lobby)
      assert game.wonders == %{"mode" => "choose_board"}

      {:ok, view} = Games.view(game.id, owner.id)
      assert %{phase: %{kind: :setup}, setup: %{mode: :choose_board}} = view
      assert length(view.setup.available) == 7
      assert Enum.all?(view.setup.seats, &is_nil(&1.wonder))
    end

    test "setup actions are logged and survive a server restart", %{
      owner: owner,
      guests: [b, c | _],
      lobby: lobby
    } do
      connect_to_lobby(lobby, [b, c])
      {:ok, lobby} = Lobbies.set_wonder_mode(Scope.for_user(owner), lobby, "choose_board")
      {:ok, game} = Games.start_game(Scope.for_user(owner), lobby)

      assert :ok =
               Games.submit(game.id, owner.id, {:claim_wonder, %{wonder: "Babylon", side: :b}})

      assert {:error, :wonder_taken} =
               Games.submit(game.id, b.id, {:claim_wonder, %{wonder: "Babylon", side: :a}})

      assert :ok = Games.submit(game.id, owner.id, :confirm_wonder)

      assert [
               %{"type" => "claim_wonder", "wonder" => "Babylon", "side" => "b"},
               %{"type" => "confirm_wonder"}
             ] = Enum.map(Replay.actions(game.id), & &1.action)

      stop_game_servers()
      {:ok, view} = Games.view(game.id, b.id)
      owner_id = to_string(owner.id)

      assert %{wonder: "Babylon", side: :b, confirmed: true} =
               Enum.find(view.setup.seats, &(&1.player == owner_id))

      refute "Babylon" in view.setup.available
    end

    test "once every seat confirms, Age I is dealt", %{
      owner: owner,
      guests: [b, c | _],
      lobby: lobby
    } do
      connect_to_lobby(lobby, [b, c])
      {:ok, lobby} = Lobbies.set_wonder_mode(Scope.for_user(owner), lobby, "choose_side")
      {:ok, game} = Games.start_game(Scope.for_user(owner), lobby)
      ids = Enum.map([owner, b, c], &to_string(&1.id))
      {view_fun, submit_fun} = Helios.GameDriver.games_funs(game.id)

      assert :ok = Helios.GameDriver.finish_setup(ids, view_fun, submit_fun)

      view = view_fun.(hd(ids))
      assert %{phase: %{kind: :choosing_cards, age: 1, turn: 1}, setup: nil} = view
      assert length(view.hand) == 7
      assert Enum.all?(view.players, &(&1.side == :a))
    end
```

- [ ] **Step 2: Run to verify they fail**

Run: `cd helios && mix test test/helios/games_test.exs`
Expected: FAIL — the choose tests get `game.wonders == %{"explicit" => …}` / `nil` and a `:choosing_cards` view; `GameDriver.finish_setup/3` is undefined.

- [ ] **Step 3: `configured_wonders` from the lobby**

In `helios/lib/helios/games.ex` `start_game/2`, change the `wonders =` line:

```elixir
         wonders = configured_wonders(lobby, length(players)),
```

and replace `configured_wonders/1`:

```elixir
  # The lobby's mode decides; the config `wonders:` override (e2e only) applies
  # only to random games, so e2e can still exercise the setup modes.
  defp configured_wonders(%{wonder_mode: "choose_side"}, _count), do: :choose_side
  defp configured_wonders(%{wonder_mode: "choose_board"}, _count), do: :choose_board

  defp configured_wonders(_lobby, count) do
    case Keyword.get(config(), :wonders) do
      nil -> :random
      wonders -> {:explicit, Enum.take(wonders, count)}
    end
  end
```

(`Replay.rebuild/4` needs no change: it decodes `games.wonders` through `ActionCodec.decode_wonders/1`.)

- [ ] **Step 4: Test support**

`helios/test/support/games_fixtures.ex` — in `game_fixture/2` replace the `wonders: ActionCodec.encode_wonders({:explicit, wonders})` line and add the `selection` binding after `wonders = …`:

```elixir
    selection = Keyword.get(opts, :selection, {:explicit, wonders})
```

```elixir
        wonders: ActionCodec.encode_wonders(selection)
```

and document the option in its `@doc`: `"Inserts an active game directly (no GameServer is started). opts: :lobby, :wonders, :seed, :selection (:choose_board | :choose_side | {:explicit, …})."`

`helios/test/support/game_driver.ex` — add after `step/4`:

```elixir
  @doc """
  Finishes a `choose_board` / `choose_side` setup: every seat claims side A
  (the first free board, or its drawn board) in seat order, then all confirm.
  """
  def finish_setup(players, view_fun, submit_fun) do
    for player <- players do
      %{setup: setup} = view_fun.(player)

      wonder =
        case setup.mode do
          :choose_board -> hd(setup.available)
          :choose_side -> Enum.find(setup.seats, &(&1.player == player)).wonder
        end

      :ok = submit_fun.(player, {:claim_wonder, %{wonder: wonder, side: :a}})
    end

    Enum.each(players, &(:ok = submit_fun.(&1, :confirm_wonder)))
  end
```

- [ ] **Step 5: Run and commit**

Run: `cd helios && mix test test/helios/games_test.exs && mix precommit`
Expected: PASS.

```bash
git add helios/lib/helios/games.ex helios/test/support/games_fixtures.ex helios/test/support/game_driver.ex helios/test/helios/games_test.exs
git commit -m "feat(games): start games in the lobby's wonder mode" -m "Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 5: Presentation helpers and the top bar during setup

**Files:**
- Modify: `helios/lib/helios_web/game_format.ex`, `helios/lib/helios_web/game_assets.ex`, `helios/lib/helios/catalogue.ex`, `helios/lib/helios_web/components/game_components/top_bar.ex`, `helios/test/support/sample_views.ex`
- Test: `helios/test/helios_web/game_format_test.exs`, `helios/test/helios_web/game_assets_test.exs`, `helios/test/helios/catalogue_test.exs`, `helios/test/helios_web/components/game_components/top_bar_test.exs`

**Interfaces:**
- Consumes: setup view shape (README "D → E"); phase A's `top_bar/1` (`replay` attr, `#pass-row`, `#waiting-count` button / `#waiting-list`, desktop `#waiting-for`); phase B's `#discard-pile`.
- Produces: `GameFormat.seat_ids(view) :: [String.t()]` (from `setup.seats` during setup, else `players`); `GameFormat.statuses/1` keyed by `seat_ids/1`; `GameFormat.show_hand?/1` false during setup (so `dock?/1` false); `GameAssets.wonder_slug/1`; `Catalogue.wonder_names/0` (engine order). `SampleViews.setup_view(mode \\ :choose_board)`, `SampleViews.wonder_names/0`; `SampleViews.view/1` gains `setup: nil`. Top bar in setup: `#age-label` = "Choosing boards", no `#turn-label` / `#pass-row` / `#pass-direction`, waiting UI lists unconfirmed seats, `#connections` lists every seat.

- [ ] **Step 1: Sample setup views**

In `helios/test/support/sample_views.ex`, add `setup: nil` to the default map in `view/1` (after `scores: nil`), and add:

```elixir
  def wonder_names,
    do: ["Alexandria", "Babylon", "Éphesos", "Gizah", "Halikarnassós", "Olympía", "Rhódos"]

  @doc "A setup-phase view (engine shape: no boards, no hand). Me = \"1\"."
  def setup_view(mode \\ :choose_board) do
    seats =
      case mode do
        :choose_board ->
          [
            %{player: "1", wonder: "Gizah", side: :b, confirmed: true},
            %{player: "2", wonder: "Rhódos", side: :a, confirmed: false},
            %{player: "3", wonder: nil, side: nil, confirmed: false},
            %{player: "4", wonder: "Babylon", side: :a, confirmed: true}
          ]

        :choose_side ->
          [
            %{player: "1", wonder: "Gizah", side: nil, confirmed: false},
            %{player: "2", wonder: "Rhódos", side: :b, confirmed: true},
            %{player: "3", wonder: "Éphesos", side: nil, confirmed: false},
            %{player: "4", wonder: "Babylon", side: :a, confirmed: false}
          ]
      end

    held = for %{wonder: wonder} <- seats, wonder != nil, do: wonder
    available = if mode == :choose_board, do: wonder_names() -- held, else: []

    view(%{
      phase: %{
        kind: :setup,
        age: 1,
        turn: 1,
        direction: :west,
        extra_turn_player: nil,
        extra_turn_kind: nil
      },
      players: [],
      hand: [],
      discard_pile: nil,
      discard_count: 0,
      my_pending: nil,
      scores: nil,
      submitted: Enum.map(seats, &{&1.player, &1.confirmed}),
      setup: %{mode: mode, seats: seats, available: available}
    })
  end
```

- [ ] **Step 2: Write the failing tests**

`helios/test/helios_web/game_format_test.exs`:

```elixir
  test "during setup seats, statuses and waiting come from the setup seats" do
    view = SampleViews.setup_view()
    assert GameFormat.seat_ids(view) == ["1", "2", "3", "4"]

    assert GameFormat.statuses(view) ==
             %{"1" => :chosen, "2" => :thinking, "3" => :thinking, "4" => :chosen}

    assert GameFormat.waiting_for(view, SampleViews.names()) == ["Bob", "Cid"]
    refute GameFormat.show_hand?(view)
    refute GameFormat.dock?(view)
    assert GameFormat.seat_ids(SampleViews.view()) == ["1", "2", "3", "4"]
  end
```

`helios/test/helios_web/game_assets_test.exs`:

```elixir
  test "wonder slugs are the ASCII city names used by the images" do
    assert Enum.map(Helios.SampleViews.wonder_names(), &GameAssets.wonder_slug/1) ==
             ~w(alexandria babylon ephesos gizah halikarnassus olympia rhodos)
  end
```

`helios/test/helios/catalogue_test.exs`:

```elixir
  test "wonder names in engine order" do
    names = Catalogue.wonder_names()
    assert length(names) == 7
    assert Enum.sort(names) == Enum.sort(Helios.SampleViews.wonder_names())
    assert names == Enum.map(Helios.Core.game_settings().wonders, & &1.name)
  end
```

`helios/test/helios_web/components/game_components/top_bar_test.exs`:

```elixir
  test "setup: choosing boards, unconfirmed players waiting, every seat listed" do
    html = render_bar(SampleViews.setup_view())
    assert attrs(html, "#top-bar", "data-phase") == ["setup"]
    assert text(html, "#age-label") == "Choosing boards"
    assert count(html, "#turn-label") == 0
    assert count(html, "#pass-row") == 0
    assert count(html, "#pass-direction") == 0
    assert count(html, "#discard-pile") == 0
    assert text(html, "#waiting-for") == "Waiting for: Bob, Cid"
    assert count(html, "#connections li") == 4
    assert attrs(html, "#connection-2", "data-connected") == ["true"]
    assert attrs(html, "#connection-1", "data-status") == ["chosen"]
    assert attrs(html, "#connection-3", "data-status") == ["thinking"]
  end
```

(If phase A changed `render_bar/2` to pass `replay:`, keep its call shape; the setup view is live, `replay: false`.)

- [ ] **Step 3: Run to verify they fail**

Run: `cd helios && mix test test/helios_web/game_format_test.exs test/helios_web/game_assets_test.exs test/helios/catalogue_test.exs test/helios_web/components/game_components/top_bar_test.exs`
Expected: FAIL (`seat_ids/1`, `wonder_slug/1`, `wonder_names/0` undefined; top bar shows "Age I", `#turn-label`, no connections).

- [ ] **Step 4: `GameFormat`, `GameAssets`, `Catalogue`**

`helios/lib/helios_web/game_format.ex` — replace `statuses/1` and add `seat_ids/1` above it:

```elixir
  @doc "Engine ids of every seat in seat order (also during setup, when `players` is empty)."
  def seat_ids(%{phase: %{kind: :setup}, setup: %{seats: seats}}),
    do: Enum.map(seats, & &1.player)

  def seat_ids(%{players: players}), do: Enum.map(players, & &1.name)

  def statuses(view), do: Map.new(seat_ids(view), &{&1, player_status(view, &1)})
```

Add a first clause to `show_hand?/1`:

```elixir
  def show_hand?(%{phase: %{kind: :setup}}), do: false
```

Add a comment above `waiting_for/2`'s last clause: `# Setup: \`submitted\` holds {seat, confirmed?}, so this lists unconfirmed seats.` (`player_status/2` likewise reports `:chosen` for confirmed seats.)

`helios/lib/helios_web/game_assets.ex` — replace `wonder_path/2`:

```elixir
  @doc "ASCII city name of a wonder, as used by its image files and DOM ids."
  @spec wonder_slug(String.t()) :: String.t()
  def wonder_slug(wonder) do
    city =
      wonder
      |> :unicode.characters_to_nfd_binary()
      |> String.replace(~r/\p{Mn}/u, "")
      |> String.downcase()

    if city == "halikarnassos", do: "halikarnassus", else: city
  end

  def wonder_path(wonder, side) when side in [:a, :b] do
    static("/images/wonders/#{wonder_slug(wonder)}#{side |> Atom.to_string() |> String.upcase()}.png")
  end
```

`helios/lib/helios/catalogue.ex` — add `def wonder_names, do: index().wonder_names` next to `boards/0`, and a key in `build/1`:

```elixir
        ),
      wonder_names: Enum.map(settings.wonders, & &1.name)
    }
```

- [ ] **Step 5: Top bar**

In `helios/lib/helios_web/components/game_components/top_bar.ex` (as left by phases A and B), make four edits:

1. In `top_bar/1`'s `assign(assigns, …)` add:

```elixir
        setup?: assigns.view.phase.kind == :setup,
        seats: GameFormat.seat_ids(assigns.view),
```

2. `#age-label` body:

```heex
          <span id="age-label" class="text-lg font-bold tracking-wide whitespace-nowrap sm:text-xl">
            {if @setup?, do: "Choosing boards", else: "Age #{GameFormat.roman(@phase.age)}"}
          </span>
```

3. Turn and pass information are not rendered during setup: add `:if={!@setup?}` to the `#turn-label` span of the non-game-over branch and to the `#pass-row` container (phase A). If phase A/B left a separate desktop `#pass-direction` or `#discard-pile` outside `#pass-row`, give it the same condition. Before phase A the equivalent edit is:

```heex
          <%= cond do %>
            <% @setup? -> %>
            <% @phase.kind == :game_over -> %>
              <span id="turn-label" class="whitespace-nowrap font-semibold">Game over</span>
            <% true -> %>
              <span id="turn-label" class="whitespace-nowrap">Turn {@phase.turn}/6</span>
              <%!-- pass direction as before --%>
          <% end %>
```

The waiting UI (`#waiting-count` / `#waiting-list` / `#waiting-for`) stays as is: `@waiting` already lists the unconfirmed seats.

4. `#connections`: iterate seat ids instead of `@view.players` (empty during setup):

```heex
        <li
          :for={id <- @seats}
          id={"connection-#{id}"}
          data-connected={to_string(MapSet.member?(@connected, id))}
          data-status={@statuses[id]}
          class="flex shrink-0 items-center gap-1.5 text-sm"
        >
          <span class={[
            "size-2.5 shrink-0 rounded-full transition",
            if(MapSet.member?(@connected, id), do: "bg-emerald-400", else: "bg-disconnected")
          ]}>
          </span>
          <span class="max-w-32 truncate sm:max-w-none">
            {GameFormat.player_name(@names, id)}
          </span>
          <.icon
            :if={@statuses[id] == :chosen}
            name="hero-check"
            class="size-4 shrink-0 text-emerald-200"
          />
        </li>
```

(Keep any classes phase A changed on these elements; only the `:for` binding and `player.name` → `id` change.)

- [ ] **Step 6: Run and commit**

Run: `cd helios && mix test test/helios_web test/helios/catalogue_test.exs && mix precommit`
Expected: PASS (existing top bar, game format and game live tests unchanged).

```bash
git add helios/lib/helios_web/game_format.ex helios/lib/helios_web/game_assets.ex helios/lib/helios/catalogue.ex helios/lib/helios_web/components/game_components/top_bar.ex helios/test/support/sample_views.ex helios/test/helios_web/game_format_test.exs helios/test/helios_web/game_assets_test.exs helios/test/helios/catalogue_test.exs helios/test/helios_web/components/game_components/top_bar_test.exs
git commit -m "feat(web): seat-based top bar and helpers for the setup phase" -m "Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 6: `WonderSetup` component

**Files:**
- Create: `helios/lib/helios_web/components/game_components/wonder_setup.ex`
- Test: `helios/test/helios_web/components/game_components/wonder_setup_test.exs` (new)

**Interfaces:**
- Consumes: `GameAssets.wonder_slug/1`, `GameAssets.wonder_path/2`, `GameFormat.player_name/2`, `GameFormat.side_label/1`; `SampleViews.setup_view/1`, `SampleViews.wonder_names/0` (Task 5).
- Produces: `WonderSetup.wonder_setup/1` — attrs `setup` (`view.setup`), `me`, `names`, `wonders` (all base wonder names), `previews` (map wonder → side shown on a free board, default `%{}`). Events emitted: `claim_wonder` (`phx-value-wonder`, `phx-value-side`), `show_side` (same values; free boards only), `confirm_wonder`.
  - `choose_board`: `#setup-boards` with one `article#setup-wonder-<slug>` per wonder (`data-wonder`, `data-side`, `data-holder` when held, `data-mine` when mine); inside `button#claim-<slug>` (disabled when held by anyone or after I confirmed), side toggles `button#side-<slug>-a|b` (`aria-pressed`; `claim_wonder` on my board, `show_side` on a free board, disabled on others' boards or after I confirmed), `[data-holder-name]` ("You" or the name) and `[data-confirmed]` once that holder confirmed.
  - `choose_side`: `button#setup-side-a|b` (my drawn board's sides, `aria-pressed`, disabled after confirming); `ul#setup-seats` with `li#setup-seat-<id>` for every other seat ("‹Board› · A/B" or "‹Board› · choosing", `[data-confirmed]`).
  - Both: `button#confirm-wonder` (enabled once my seat has a side and is not confirmed), `#setup-waiting` "Waiting for ‹names›" after I confirmed.

- [ ] **Step 1: Write the failing test**

`helios/test/helios_web/components/game_components/wonder_setup_test.exs`:

```elixir
defmodule HeliosWeb.GameComponents.WonderSetupTest do
  use ExUnit.Case, async: true

  import Phoenix.LiveViewTest
  import HeliosWeb.HTMLHelpers

  alias Helios.SampleViews
  alias HeliosWeb.GameComponents.WonderSetup

  defp render_setup(setup, opts \\ []) do
    render_component(
      &WonderSetup.wonder_setup/1,
      Keyword.merge(
        [
          setup: setup,
          me: "1",
          names: SampleViews.names(),
          wonders: SampleViews.wonder_names(),
          previews: %{}
        ],
        opts
      )
    )
  end

  defp with_seat(setup, player, changes) do
    seats =
      Enum.map(setup.seats, fn seat ->
        if seat.player == player, do: Map.merge(seat, changes), else: seat
      end)

    %{setup | seats: seats}
  end

  defp board_setup, do: SampleViews.setup_view(:choose_board).setup

  test "choose_board: one tile per wonder with its holder, side and actions" do
    html = render_setup(with_seat(board_setup(), "1", %{confirmed: false}))

    assert attrs(html, "#wonder-setup", "data-mode") == ["choose_board"]
    assert count(html, "#setup-boards article[data-wonder]") == 7

    # My board: flipping a side re-claims it.
    assert attrs(html, "#setup-wonder-gizah", "data-holder") == ["1"]
    assert count(html, "#setup-wonder-gizah[data-mine]") == 1
    assert attrs(html, "#setup-wonder-gizah img", "src") == ["/images/wonders/gizahB.png"]
    assert attrs(html, "#side-gizah-b", "aria-pressed") == ["true"]
    assert attrs(html, "#side-gizah-a", "phx-click") == ["claim_wonder"]
    assert attrs(html, "#side-gizah-a", "phx-value-side") == ["a"]
    assert text(html, "#setup-wonder-gizah [data-holder-name]") == "You"

    # Someone else's board: disabled, named, confirmed mark when confirmed.
    assert count(html, "#claim-rhodos[disabled]") == 1
    assert count(html, "#side-rhodos-b[disabled]") == 1
    assert text(html, "#setup-wonder-rhodos [data-holder-name]") == "Bob"
    assert count(html, "#setup-wonder-rhodos [data-confirmed]") == 0
    assert count(html, "#setup-wonder-babylon [data-confirmed]") == 1

    # A free board: claim with the shown side, toggles only preview.
    assert count(html, "#setup-wonder-alexandria[data-holder]") == 0
    assert count(html, "#claim-alexandria[disabled]") == 0
    assert attrs(html, "#claim-alexandria", "phx-click") == ["claim_wonder"]
    assert attrs(html, "#claim-alexandria", "phx-value-wonder") == ["Alexandria"]
    assert attrs(html, "#claim-alexandria", "phx-value-side") == ["a"]
    assert attrs(html, "#side-alexandria-b", "phx-click") == ["show_side"]
    assert attrs(html, "#setup-wonder-halikarnassus img", "src") ==
             ["/images/wonders/halikarnassusA.png"]

    assert count(html, "#confirm-wonder[disabled]") == 0
    assert attrs(html, "#confirm-wonder", "phx-click") == ["confirm_wonder"]
    assert count(html, "#setup-waiting") == 0
  end

  test "a previewed side is shown and claimed on a free board" do
    html =
      render_setup(with_seat(board_setup(), "1", %{confirmed: false}),
        previews: %{"Alexandria" => :b}
      )

    assert attrs(html, "#setup-wonder-alexandria", "data-side") == ["b"]
    assert attrs(html, "#setup-wonder-alexandria img", "src") == ["/images/wonders/alexandriaB.png"]
    assert attrs(html, "#claim-alexandria", "phx-value-side") == ["b"]
    assert attrs(html, "#side-alexandria-b", "aria-pressed") == ["true"]
  end

  test "without a claim Confirm is disabled; after confirming everything is locked" do
    unclaimed = render_setup(with_seat(board_setup(), "1", %{wonder: nil, side: nil, confirmed: false}))
    assert count(unclaimed, "#confirm-wonder[disabled]") == 1
    assert count(unclaimed, "[data-mine]") == 0
    assert count(unclaimed, "#claim-gizah[disabled]") == 0

    confirmed = render_setup(board_setup())
    assert count(confirmed, "#confirm-wonder[disabled]") == 1
    assert text(confirmed, "#setup-waiting") == "Waiting for Bob, Cid"
    assert count(confirmed, "#claim-alexandria[disabled]") == 1
    assert count(confirmed, "#side-gizah-a[disabled]") == 1
    assert count(confirmed, "#setup-wonder-gizah [data-confirmed]") == 1
  end

  test "choose_side: both sides of my drawn board and the other seats" do
    setup = SampleViews.setup_view(:choose_side).setup
    html = render_setup(setup)

    assert attrs(html, "#wonder-setup", "data-mode") == ["choose_side"]
    assert count(html, "#setup-boards") == 0
    assert attrs(html, "#setup-side-a img", "src") == ["/images/wonders/gizahA.png"]
    assert attrs(html, "#setup-side-b", "phx-click") == ["claim_wonder"]
    assert attrs(html, "#setup-side-b", "phx-value-wonder") == ["Gizah"]
    assert attrs(html, "#setup-side-b", "phx-value-side") == ["b"]
    assert attrs(html, "#setup-side-a", "aria-pressed") == ["false"]
    assert count(html, "#confirm-wonder[disabled]") == 1

    assert count(html, "#setup-seat-1") == 0
    assert text(html, "#setup-seat-2") =~ "Bob"
    assert text(html, "#setup-seat-2") =~ "Rhódos · B"
    assert count(html, "#setup-seat-2 [data-confirmed]") == 1
    assert text(html, "#setup-seat-3") =~ "Éphesos · choosing"

    picked = render_setup(with_seat(setup, "1", %{side: :a}))
    assert attrs(picked, "#setup-side-a", "aria-pressed") == ["true"]
    assert count(picked, "#confirm-wonder[disabled]") == 0
  end
end
```

- [ ] **Step 2: Run to verify it fails**

Run: `cd helios && mix test test/helios_web/components/game_components/wonder_setup_test.exs`
Expected: FAIL (`HeliosWeb.GameComponents.WonderSetup` undefined).

- [ ] **Step 3: The component**

`helios/lib/helios_web/components/game_components/wonder_setup.ex`:

```elixir
defmodule HeliosWeb.GameComponents.WonderSetup do
  @moduledoc """
  The setup screen shown while `view.phase.kind == :setup`, instead of the
  table and dock: claim a board and side (`choose_board`) or pick a side of
  your drawn board (`choose_side`), then confirm. All of it is public. The
  engine decides every claim; buttons are only disabled when `view.setup`
  says the action cannot succeed.
  """
  use HeliosWeb, :html

  alias HeliosWeb.{GameAssets, GameFormat}

  attr :setup, :map, required: true, doc: "view.setup"
  attr :me, :string, required: true
  attr :names, :map, required: true
  attr :wonders, :list, required: true, doc: "every base wonder name"
  attr :previews, :map, default: %{}, doc: "wonder name => side shown on a free board"

  def wonder_setup(assigns) do
    mine = Enum.find(assigns.setup.seats, &(&1.player == assigns.me))

    waiting =
      for seat <- assigns.setup.seats,
          !seat.confirmed,
          do: GameFormat.player_name(assigns.names, seat.player)

    assigns = assign(assigns, mine: mine, waiting: waiting)

    ~H"""
    <section id="wonder-setup" data-mode={@setup.mode} class="flex flex-1 flex-col gap-3 sm:gap-4">
      <header class="rounded-xl bg-antique/90 p-3 shadow">
        <h2 class="text-lg font-semibold text-zinc-900">
          {if @setup.mode == :choose_board, do: "Choose your wonder", else: "Choose your side"}
        </h2>
        <p class="text-sm text-zinc-700">
          {if @setup.mode == :choose_board,
            do: "Tap a board to claim it. You can switch until you confirm.",
            else: "Pick side A or B of your board, then confirm."}
        </p>
      </header>

      <div :if={@setup.mode == :choose_board} id="setup-boards" class="grid gap-3 sm:grid-cols-2 lg:grid-cols-3">
        <.board_tile
          :for={wonder <- @wonders}
          wonder={wonder}
          holder={Enum.find(@setup.seats, &(&1.wonder == wonder))}
          me={@me}
          names={@names}
          preview={Map.get(@previews, wonder, :a)}
          locked={@mine.confirmed}
        />
      </div>

      <%= if @setup.mode == :choose_side do %>
        <div id="setup-sides" class="grid gap-3 sm:grid-cols-2">
          <button
            :for={side <- [:a, :b]}
            id={"setup-side-#{side}"}
            type="button"
            phx-click="claim_wonder"
            phx-value-wonder={@mine.wonder}
            phx-value-side={side}
            aria-pressed={to_string(@mine.side == side)}
            disabled={@mine.confirmed}
            class={[
              "flex flex-col gap-1 rounded-xl bg-antique/90 p-2 text-left shadow ring-2 transition disabled:cursor-default",
              if(@mine.side == side, do: "ring-amber-400", else: "ring-transparent hover:ring-zinc-300")
            ]}
          >
            <img
              src={GameAssets.wonder_path(@mine.wonder, side)}
              alt={"#{@mine.wonder} #{GameFormat.side_label(side)}"}
              class="aspect-[16/5] w-full rounded-lg object-cover"
            />
            <span class="text-sm font-semibold text-zinc-900">Side {GameFormat.side_label(side)}</span>
          </button>
        </div>
        <ul id="setup-seats" class="grid gap-2 rounded-xl bg-antique/90 p-3 shadow sm:grid-cols-2">
          <li
            :for={seat <- @setup.seats}
            :if={seat.player != @me}
            id={"setup-seat-#{seat.player}"}
            class="flex min-w-0 items-center gap-2 text-sm"
          >
            <img
              src={GameAssets.wonder_path(seat.wonder, seat.side || :a)}
              alt=""
              class="aspect-[16/5] w-16 shrink-0 rounded object-cover"
            />
            <span class="truncate font-semibold text-zinc-900">
              {GameFormat.player_name(@names, seat.player)}
            </span>
            <span class="whitespace-nowrap text-zinc-700">
              {seat.wonder} · {if seat.side, do: GameFormat.side_label(seat.side), else: "choosing"}
            </span>
            <span :if={seat.confirmed} data-confirmed class="shrink-0 text-emerald-600">
              <.icon name="hero-check-circle" class="size-4" />
            </span>
          </li>
        </ul>
      <% end %>

      <div class="sticky bottom-0 z-30 -mx-2 flex flex-wrap items-center justify-end gap-3 rounded-t-xl bg-antique/95 px-3 pt-2 pb-[max(0.5rem,env(safe-area-inset-bottom))] shadow-[0_-4px_12px_rgba(0,0,0,0.2)] sm:mx-0">
        <p :if={@mine.confirmed} id="setup-waiting" class="mr-auto text-sm text-zinc-700">
          Waiting for {Enum.join(@waiting, ", ")}
        </p>
        <button
          id="confirm-wonder"
          type="button"
          phx-click="confirm_wonder"
          disabled={is_nil(@mine.side) or @mine.confirmed}
          class="rounded-lg bg-zinc-900 px-5 py-2 font-semibold text-white shadow transition hover:bg-zinc-700 active:bg-zinc-800 disabled:cursor-not-allowed disabled:opacity-50 pointer-coarse:min-h-11"
        >
          {if @mine.confirmed, do: "Confirmed", else: "Confirm"}
        </button>
      </div>
    </section>
    """
  end

  attr :wonder, :string, required: true
  attr :holder, :map, default: nil
  attr :me, :string, required: true
  attr :names, :map, required: true
  attr :preview, :atom, required: true
  attr :locked, :boolean, required: true

  defp board_tile(assigns) do
    holder = assigns.holder
    mine? = holder != nil and holder.player == assigns.me

    assigns =
      assign(assigns,
        slug: GameAssets.wonder_slug(assigns.wonder),
        mine?: mine?,
        taken?: holder != nil and not mine?,
        side: (holder && holder.side) || assigns.preview
      )

    ~H"""
    <article
      id={"setup-wonder-#{@slug}"}
      data-wonder={@wonder}
      data-side={@side}
      data-holder={@holder && @holder.player}
      data-mine={@mine?}
      class={[
        "flex min-w-0 flex-col gap-2 rounded-xl bg-antique/90 p-2 shadow ring-2",
        if(@mine?, do: "ring-amber-400", else: "ring-transparent")
      ]}
    >
      <button
        id={"claim-#{@slug}"}
        type="button"
        phx-click="claim_wonder"
        phx-value-wonder={@wonder}
        phx-value-side={@side}
        disabled={@holder != nil or @locked}
        aria-label={"Claim #{@wonder} #{GameFormat.side_label(@side)}"}
        class="overflow-hidden rounded-lg transition enabled:hover:-translate-y-0.5 disabled:cursor-default"
      >
        <img
          src={GameAssets.wonder_path(@wonder, @side)}
          alt={"#{@wonder} #{GameFormat.side_label(@side)}"}
          class={["aspect-[16/5] w-full object-cover", @taken? && "opacity-60 grayscale"]}
        />
      </button>
      <div class="flex items-center justify-between gap-2">
        <span class="flex min-w-0 items-center gap-1.5 text-sm">
          <span class="font-semibold text-zinc-900">{@wonder}</span>
          <span :if={@holder} data-holder-name class="truncate text-zinc-700">
            {if @mine?, do: "You", else: GameFormat.player_name(@names, @holder.player)}
          </span>
          <span :if={@holder && @holder.confirmed} data-confirmed class="shrink-0 text-emerald-600">
            <.icon name="hero-check-circle" class="size-4" />
          </span>
        </span>
        <span
          role="group"
          aria-label={"#{@wonder} side"}
          class="flex shrink-0 overflow-hidden rounded-md ring-1 ring-zinc-300"
        >
          <button
            :for={side <- [:a, :b]}
            id={"side-#{@slug}-#{side}"}
            type="button"
            phx-click={if @mine?, do: "claim_wonder", else: "show_side"}
            phx-value-wonder={@wonder}
            phx-value-side={side}
            aria-pressed={to_string(@side == side)}
            disabled={@taken? or @locked}
            class={[
              "px-3 py-1 text-sm font-semibold transition disabled:cursor-not-allowed disabled:opacity-50 pointer-coarse:min-h-11 pointer-coarse:min-w-11",
              if(@side == side,
                do: "bg-zinc-900 text-white",
                else: "bg-white text-zinc-900 hover:bg-zinc-100"
              )
            ]}
          >
            {GameFormat.side_label(side)}
          </button>
        </span>
      </div>
    </article>
    """
  end
end
```

(`data-mine={false}` and `data-holder={nil}` omit the attribute, so `[data-mine]` / `[data-holder]` match only mine / held boards.)

- [ ] **Step 4: Run and commit**

Run: `cd helios && mix test test/helios_web/components/game_components/wonder_setup_test.exs && mix precommit`
Expected: PASS.

```bash
git add helios/lib/helios_web/components/game_components/wonder_setup.ex helios/test/helios_web/components/game_components/wonder_setup_test.exs
git commit -m "feat(web): WonderSetup screen for choosing boards and sides" -m "Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 7: `GameLive` renders the setup phase

**Files:**
- Modify: `helios/lib/helios_web/live/game_live.ex`
- Test: `helios/test/helios_web/live/game_live_wonder_setup_test.exs` (new)

**Interfaces:**
- Consumes: `WonderSetup.wonder_setup/1` (Task 6); `Choice.parse_claim/1`, `Games.error_message/1` (Task 3); `game_fixture(…, selection:)` (Task 4); `Catalogue.wonder_names/0`, `GameFormat.statuses/1` (Task 5); `Games.submit/3` (existing, goes through `GameServer` → `Core.submit`, logs the action and broadcasts `{:game_updated, seq}`).
- Produces: `GameLive` events `claim_wonder`, `confirm_wonder`, `show_side`; assigns `wonder_names`, `setup_previews`; a setup clause of `assign_view/2` that derives nothing from `players`.

- [ ] **Step 1: Write the failing test**

`helios/test/helios_web/live/game_live_wonder_setup_test.exs`:

```elixir
defmodule HeliosWeb.GameLiveWonderSetupTest do
  use HeliosWeb.ConnCase, async: false

  import Ecto.Query
  import Phoenix.LiveViewTest
  import Helios.GamesFixtures

  alias Helios.{Games, Repo}
  alias Helios.Games.GameAction

  setup do
    on_exit(&stop_game_servers/0)
    %{players: for(_ <- 1..3, do: user_with_token_fixture())}
  end

  defp open(token, game_id), do: live(log_in_conn(build_conn(), token), ~p"/game/#{game_id}")

  defp start(players, selection) do
    game = game_fixture(Enum.map(players, &elem(&1, 0)), selection: selection)
    views = Enum.map(players, fn {_user, token} -> elem(open(token, game.id), 1) end)
    {game, views}
  end

  defp action_count(game),
    do: Repo.aggregate(from(a in GameAction, where: a.game_id == ^game.id), :count)

  test "choose_board: claim, flip, switch and confirm; Age I starts once everyone confirmed",
       %{players: players} do
    [{a, _}, {b, _}, {c, _}] = players
    {game, [view_a, view_b, view_c] = views} = start(players, :choose_board)

    for view <- views do
      assert has_element?(view, "#wonder-setup[data-mode='choose_board']")
      assert has_element?(view, "#top-bar[data-phase='setup']")
      assert has_element?(view, "#age-label", "Choosing boards")
      refute has_element?(view, "#table")
      refute has_element?(view, "#dock")
      assert has_element?(view, "#confirm-wonder[disabled]")
    end

    tiles =
      view_a
      |> render()
      |> LazyHTML.from_fragment()
      |> LazyHTML.query("#setup-boards article[data-wonder]")

    assert Enum.count(tiles) == 7

    view_a |> element("#claim-gizah") |> render_click()
    assert has_element?(view_a, "#setup-wonder-gizah[data-holder='#{a.id}'][data-mine][data-side='a']")
    refute has_element?(view_a, "#confirm-wonder[disabled]")
    assert has_element?(view_b, "#claim-gizah[disabled]")
    assert has_element?(view_b, "#setup-wonder-gizah [data-holder-name]", a.name)

    view_a |> element("#side-gizah-b") |> render_click()
    assert has_element?(view_b, "#setup-wonder-gizah[data-side='b']")

    view_a |> element("#claim-rhodos") |> render_click()
    refute has_element?(view_a, "#setup-wonder-gizah[data-holder]")
    assert has_element?(view_a, "#setup-wonder-rhodos[data-mine][data-side='a']")

    # A free board's side toggle only changes what this player sees.
    view_b |> element("#side-ephesos-b") |> render_click()
    assert has_element?(view_b, "#setup-wonder-ephesos[data-side='b']")
    refute has_element?(view_b, "#setup-wonder-ephesos[data-holder]")
    assert has_element?(view_a, "#setup-wonder-ephesos[data-side='a']")
    view_b |> element("#claim-ephesos") |> render_click()
    assert has_element?(view_a, "#setup-wonder-ephesos[data-holder='#{b.id}'][data-side='b']")

    view_c |> element("#claim-gizah") |> render_click()

    view_a |> element("#confirm-wonder") |> render_click()
    assert has_element?(view_a, "#confirm-wonder[disabled]")
    assert has_element?(view_a, "#setup-waiting", b.name)
    assert has_element?(view_a, "#setup-waiting", c.name)
    assert has_element?(view_a, "#waiting-for", c.name)
    assert has_element?(view_a, "#side-rhodos-b[disabled]")
    assert has_element?(view_b, "#setup-wonder-rhodos [data-confirmed]")
    assert has_element?(view_b, "#connection-#{a.id}[data-status='chosen']")

    view_b |> element("#confirm-wonder") |> render_click()
    view_c |> element("#confirm-wonder") |> render_click()

    for view <- views do
      assert has_element?(view, "#top-bar[data-phase='choosing_cards'][data-turn-key='1-1']")
      refute has_element?(view, "#wonder-setup")
      assert has_element?(view, "#hand-card-6")
      refute has_element?(view, "#history-0")
    end

    assert has_element?(view_a, "#my-board img[alt='Rhódos A']")
    assert has_element?(view_b, "#my-board img[alt='Éphesos B']")
    assert has_element?(view_c, "#my-board img[alt='Gizah A']")
    # gizah, flip, rhodos (A) + ephesos (B) + gizah (C) + three confirmations
    assert action_count(game) == 8
  end

  test "choose_side: each player picks a side of their drawn board", %{players: players} do
    [{a, _}, {b, _}, _] = players
    {game, [view_a, view_b, view_c]} = start(players, :choose_side)
    {:ok, %{setup: setup}} = Games.view(game.id, a.id)
    drawn = Map.new(setup.seats, &{&1.player, &1.wonder})
    mine = drawn[to_string(a.id)]

    assert has_element?(view_a, "#wonder-setup[data-mode='choose_side']")
    assert has_element?(view_a, "#setup-side-a img[alt='#{mine} A']")
    assert has_element?(view_a, "#setup-side-b img[alt='#{mine} B']")
    assert has_element?(view_a, "#confirm-wonder[disabled]")
    assert has_element?(view_a, "#setup-seat-#{b.id}", drawn[to_string(b.id)])
    refute has_element?(view_a, "#setup-seat-#{a.id}")

    view_a |> element("#setup-side-b") |> render_click()
    assert has_element?(view_a, "#setup-side-b[aria-pressed='true']")
    assert has_element?(view_b, "#setup-seat-#{a.id}", "#{mine} · B")
    refute has_element?(view_a, "#confirm-wonder[disabled]")

    render_click(view_a, "claim_wonder", %{"wonder" => drawn[to_string(b.id)], "side" => "a"})
    assert has_element?(view_a, "#flash-error", "That is not your board")

    view_b |> element("#setup-side-a") |> render_click()
    view_c |> element("#setup-side-a") |> render_click()
    for view <- [view_a, view_b, view_c], do: view |> element("#confirm-wonder") |> render_click()

    assert has_element?(view_a, "#top-bar[data-phase='choosing_cards'][data-turn-key='1-1']")
    assert has_element?(view_a, "#my-board img[alt='#{mine} B']")
    assert has_element?(view_b, "#my-board img[alt='#{drawn[to_string(b.id)]} A']")
  end

  test "two players racing for one board: the second gets a flash and nothing is logged",
       %{players: players} do
    [{a, _}, _, _] = players
    {game, [view_a, view_b, _]} = start(players, :choose_board)

    view_a |> element("#claim-babylon") |> render_click()
    # B tapped the board before A's claim reached their page.
    render_click(view_b, "claim_wonder", %{"wonder" => "Babylon", "side" => "a"})

    assert has_element?(view_b, "#flash-error", "Babylon was just taken")
    assert has_element?(view_b, "#setup-wonder-babylon[data-holder='#{a.id}']")
    refute has_element?(view_b, "#setup-boards [data-mine]")
    assert has_element?(view_b, "#confirm-wonder[disabled]")
    assert action_count(game) == 1
  end

  test "refreshing mid-setup, even after a server restart, keeps claims and confirmations",
       %{players: players} do
    [{a, ta}, {_b, tb}, _] = players
    {game, [view_a, view_b, _]} = start(players, :choose_board)

    view_a |> element("#side-olympia-b") |> render_click()
    view_a |> element("#claim-olympia") |> render_click()
    view_b |> element("#claim-alexandria") |> render_click()
    view_b |> element("#confirm-wonder") |> render_click()

    stop_game_servers()
    {:ok, again_a, _html} = open(ta, game.id)
    {:ok, again_b, _html} = open(tb, game.id)

    assert has_element?(again_a, "#setup-wonder-olympia[data-mine][data-side='b']")
    refute has_element?(again_a, "#confirm-wonder[disabled]")
    assert has_element?(again_a, "#setup-wonder-alexandria [data-confirmed]")
    assert has_element?(again_b, "#confirm-wonder[disabled]")
    assert has_element?(again_b, "#setup-waiting", a.name)
  end

  test "forged setup events are flashed and never crash the page", %{players: players} do
    {game, [view_a | _]} = start(players, :choose_board)

    render_click(view_a, "claim_wonder", %{"wonder" => "Atlantis", "side" => "a"})
    assert has_element?(view_a, "#flash-error", "That board does not exist")

    render_click(view_a, "claim_wonder", %{"wonder" => "Gizah", "side" => "c"})
    assert has_element?(view_a, "#flash-error", "That choice is not available")

    render_click(view_a, "claim_wonder", %{})
    render_click(view_a, "confirm_wonder", %{})
    assert has_element?(view_a, "#flash-error", "Pick a board first")

    render_click(view_a, "submit", %{"card" => "Altar", "kind" => "discard"})
    assert has_element?(view_a, "#flash-error", "You can't do that right now")

    render_click(view_a, "show_card", %{"card" => "Altar", "owner" => "1"})
    render_click(view_a, "select_card", %{"card" => "Altar"})
    render_click(view_a, "show_side", %{"wonder" => "Gizah", "side" => "z"})
    render_click(view_a, "show_side", %{"wonder" => "Atlantis", "side" => "b"})
    send(view_a.pid, {:game_updated, 0})

    assert has_element?(view_a, "#wonder-setup")
    assert has_element?(view_a, "#setup-wonder-gizah[data-side='a']")
    refute has_element?(view_a, "#card-sheet")
    refute has_element?(view_a, "#action-panel")
    assert action_count(game) == 0
  end
end
```

- [ ] **Step 2: Run to verify it fails**

Run: `cd helios && mix test test/helios_web/live/game_live_wonder_setup_test.exs`
Expected: FAIL — mount crashes in `assign_view/2` (`Map.fetch!(by_name, view.me)` on `players: []`).

- [ ] **Step 3: Setup-safe `assign_view/2`**

In `helios/lib/helios_web/live/game_live.ex`, add this clause directly above the existing `defp assign_view(socket, view) do`:

```elixir
  # During setup the engine view has no boards and no hand (`players: []`):
  # nothing is derived from players, and the table, dock and sheets are not
  # rendered. Selection and the card sheet are cleared so no stale panel
  # survives into Age I.
  defp assign_view(socket, %{phase: %{kind: :setup}} = view) do
    assign(socket,
      view: view,
      me: nil,
      west: nil,
      east: nil,
      others: [],
      statuses: GameFormat.statuses(view),
      selected: nil,
      selected_card: nil,
      info_card: nil,
      discard_minimised: false
    )
  end
```

(`refresh/1` needs no change: setup produces no history events, `my_pending` is nil, and the phase is not `:game_over`. `show_card` finds no player in `[]` and assigns `info_card: nil`; `select_card` finds no card in `hand: []`.)

- [ ] **Step 4: Render the setup screen**

Add to the imports and aliases:

```elixir
  import HeliosWeb.GameComponents.WonderSetup
```

```elixir
  alias Helios.{Catalogue, Games, Lobbies}
```

In `render/1`, wrap the existing `<%= if @view.phase.kind == :game_over do %> … <% else %> … <% end %>` block (everything after `<.top_bar … />` inside the `max-w-7xl` div) in a setup branch; the wrapped block itself is unchanged:

```heex
          <%= if @view.phase.kind == :setup do %>
            <.wonder_setup
              setup={@view.setup}
              me={@view.me}
              names={@names}
              wonders={@wonder_names}
              previews={@setup_previews}
            />
          <% else %>
            <%!-- existing game_over / table + dock block, unchanged --%>
          <% end %>
```

In `setup/3`, add to the `assign(…)` list:

```elixir
      wonder_names: Catalogue.wonder_names(),
      setup_previews: %{},
```

- [ ] **Step 5: Setup events**

Add after the `"submit"` event clause:

```elixir
  def handle_event("claim_wonder", params, socket),
    do: submit_setup(socket, Choice.parse_claim(params))

  def handle_event("confirm_wonder", _params, socket),
    do: submit_setup(socket, {:ok, :confirm_wonder})

  # Which side a free board shows before it is claimed: local to this page.
  def handle_event("show_side", %{"wonder" => wonder, "side" => side}, socket)
      when side in ["a", "b"] do
    previews =
      if wonder in socket.assigns.wonder_names,
        do: Map.put(socket.assigns.setup_previews, wonder, if(side == "a", do: :a, else: :b)),
        else: socket.assigns.setup_previews

    {:noreply, assign(socket, :setup_previews, previews)}
  end

  def handle_event("show_side", _params, socket), do: {:noreply, socket}
```

and a private helper next to `refresh/1`:

```elixir
  # Claims and confirmations are ordinary engine actions; the engine decides.
  # A lost race (`:wonder_taken`) names the board that was just taken.
  defp submit_setup(socket, parsed) do
    %{game: game, current_scope: %{user: user}} = socket.assigns
    result = with {:ok, action} <- parsed, do: Games.submit(game.id, user.id, action)

    socket =
      case result do
        :ok ->
          clear_flash(socket, :error)

        {:error, :wonder_taken} ->
          {:ok, {:claim_wonder, %{wonder: wonder}}} = parsed
          put_flash(socket, :error, "#{wonder} was just taken")

        {:error, reason} ->
          put_flash(socket, :error, Games.error_message(reason))
      end

    {:noreply, refresh(socket)}
  end
```

Update the moduledoc's last sentence: "… sends choices as `{card, kind, option index}` and setup claims as `{wonder, side}`; the GameServer resolves them against its view. While `view.phase.kind == :setup` the page shows `WonderSetup` instead of the table."

- [ ] **Step 6: Run and commit**

Run: `cd helios && mix test test/helios_web/live && mix precommit`
Expected: PASS (the existing `game_live_test.exs` and `game_live_extra_turns_test.exs` are unaffected).

```bash
git add helios/lib/helios_web/live/game_live.ex helios/test/helios_web/live/game_live_wonder_setup_test.exs
git commit -m "feat(web): setup screen in GameLive for choosing boards and sides" -m "Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 8: Replays start at Age I

**Files:**
- Modify: `helios/lib/helios/games.ex` (`replay/2`, `frame_step/4`)
- Test: `helios/test/helios/games/replay_test.exs`

**Interfaces:**
- Consumes: `game_fixture(…, selection: :choose_board)`, `GameDriver.finish_setup/3` (Task 4); phase B's `frame_snapshot/2` (unchanged name/arity).
- Produces: `Games.replay/2` frames for setup-mode games: the first frame is the table as Age I turn 1 begins; setup rows open no frame and never set `my_choice`. Random/explicit games unchanged.

- [ ] **Step 1: Write the failing test**

Append to `helios/test/helios/games/replay_test.exs` (the module `setup` still plays its own random game; this test creates a second game in another lobby):

```elixir
  test "a choose_board game's frames start at Age I turn 1; setup is nobody's choice" do
    users = for _ <- 1..3, do: player_fixture()
    [a | _] = users
    game = game_fixture(users, selection: :choose_board)
    ids = Enum.map(users, &to_string(&1.id))
    {view_fun, submit_fun} = GameDriver.games_funs(game.id)

    :ok = GameDriver.finish_setup(ids, view_fun, submit_fun)
    {:game_over, _} = GameDriver.play_until(ids, %{}, view_fun, submit_fun, fn _ -> false end)
    game = Repo.get!(Game, game.id)
    stop_game_servers()

    assert {:ok, %{frames: frames, history: history}} = Games.replay(game, a.id)
    turns = Enum.count(history, &match?({:turn, _}, &1))
    assert length(frames) == turns + 1

    [first | _] = frames
    assert %{phase: %{kind: :choosing_cards, age: 1, turn: 1}, event_index: 0} = first
    assert length(first.hand) == 7
    assert Enum.all?(first.players, &(&1.side == :a))
    assert first.my_choice == {:discard, hd(first.hand)}

    refute Enum.any?(frames, &(&1.phase.kind == :setup))

    refute Enum.any?(frames, fn frame ->
             match?({:claim_wonder, _}, frame.my_choice) or frame.my_choice == :confirm_wonder
           end)
  end
```

- [ ] **Step 2: Run to verify it fails**

Run: `cd helios && mix test test/helios/games/replay_test.exs`
Expected: FAIL — the first frame is the setup table (`phase.kind == :setup`, `players: []`, `hand: []`), so the `%{phase: %{kind: :choosing_cards, …}}` match fails.

- [ ] **Step 3: Open the first frame when setup ends**

In `helios/lib/helios/games.ex` `replay/2`, change `init`:

```elixir
    init = fn ref -> %{current: opening_frame(ref, me), done: [], turns: 0, my_last: nil} end
```

Add directly above the existing `defp frame_step(row, ref, acc, me) do`:

```elixir
  # Setup actions (choose_board / choose_side) open no frame and are nobody's
  # choice: the first frame is the table as Age I turn 1 begins.
  defp frame_step(_row, ref, %{current: nil} = acc, me),
    do: %{acc | current: opening_frame(ref, me)}
```

and after `frame_step/4`:

```elixir
  defp opening_frame(ref, me) do
    case Core.view(ref, me) do
      {:ok, %{phase: %{kind: :setup}}} -> nil
      {:ok, _view} -> frame_snapshot(ref, me)
    end
  end
```

Extend the `replay/2` doc: "For setup-mode games the first frame is Age I turn 1; setup actions are not frames."

- [ ] **Step 4: Run and commit**

Run: `cd helios && mix test test/helios/games/replay_test.exs test/helios_web/live/game_live_test.exs && mix precommit`
Expected: PASS (the random-game replay tests are unchanged).

```bash
git add helios/lib/helios/games.ex helios/test/helios/games/replay_test.exs
git commit -m "feat(games): replays of setup-mode games start at Age I" -m "Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 9: e2e — a `choose_board` game through setup to Age I

**Files:**
- Create: `e2e/tests/wonder-modes.spec.ts`

**Interfaces:**
- Consumes: `setupTable`, `startGame`, `waitForTurn`, `playFirstBuildableOrDiscard` from `e2e/tests/support/game.ts`; ids from Tasks 2, 5, 6. The e2e config's `wonders:` override does not apply because the mode is not `random`; `fixed_seed` still does.

- [ ] **Step 1: Write the spec**

`e2e/tests/wonder-modes.spec.ts`:

```ts
import { expect, test } from "@playwright/test";
import { playFirstBuildableOrDiscard, setupTable, startGame, waitForTurn } from "./support/game";

test("choose board: everyone claims and confirms, then Age I begins", async ({ browser }) => {
  test.setTimeout(120_000);
  const { owner, guests, players } = await setupTable(browser, 3);

  await owner.page.locator("#wonder-mode-choose_board").click();
  await expect(owner.page.locator("#wonder-mode-choose_board")).toHaveAttribute("aria-pressed", "true");
  for (const guest of guests) {
    await expect(guest.page.locator("#wonder-mode")).toHaveAttribute("data-mode", "choose_board");
  }

  await startGame(owner, players);
  for (const player of players) {
    await expect(player.page.locator("#top-bar")).toHaveAttribute("data-phase", "setup");
    await expect(player.page.locator("#age-label")).toHaveText("Choosing boards");
    await expect(player.page.locator("#confirm-wonder")).toBeDisabled();
  }

  const [a, b, c] = players;
  await a.page.locator("#claim-gizah").click();
  await expect(a.page.locator("#setup-wonder-gizah[data-mine]")).toHaveCount(1);
  await expect(b.page.locator("#claim-gizah")).toBeDisabled();

  await b.page.locator("#side-rhodos-b").click();
  await expect(b.page.locator("#setup-wonder-rhodos")).toHaveAttribute("data-side", "b");
  await b.page.locator("#claim-rhodos").click();
  await expect(b.page.locator("#setup-wonder-rhodos[data-mine]")).toHaveCount(1);

  await c.page.locator("#claim-ephesos").click();
  await expect(c.page.locator("#setup-wonder-ephesos[data-mine]")).toHaveCount(1);

  for (const player of players) {
    await expect(player.page.locator("#confirm-wonder")).toBeEnabled();
    await player.page.locator("#confirm-wonder").click();
  }

  for (const player of players) {
    await expect(player.page.locator("#top-bar")).toHaveAttribute("data-phase", "choosing_cards");
    await waitForTurn(player.page, 1, 1);
    await expect(player.page.locator("#hand [data-card]")).toHaveCount(7);
  }
  await expect(a.page.locator("#my-board img[alt='Gizah A']")).toBeVisible();
  await expect(b.page.locator("#my-board img[alt='Rhódos B']")).toBeVisible();
  await expect(c.page.locator("#my-board img[alt='Éphesos A']")).toBeVisible();

  for (const player of players) await playFirstBuildableOrDiscard(player.page);
  for (const player of players) await waitForTurn(player.page, 1, 2);

  for (const player of players) await player.context.close();
});
```

(The `chromium` and `mobile` projects both run it; `responsive` ignores it.)

- [ ] **Step 2: Run the spec, then the full phase gate**

Run: `cd e2e && npx playwright test tests/wonder-modes.spec.ts`
Expected: PASS on `chromium` and `mobile`.

Run: `cd helios && mix precommit && cd ../e2e && npx playwright test`
Expected: all green (existing specs run in `random` mode with the explicit e2e wonders, unchanged).

- [ ] **Step 3: Commit**

```bash
git add e2e/tests/wonder-modes.spec.ts
git commit -m "test(e2e): choose_board game through setup to Age I" -m "Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```
