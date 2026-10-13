# AI Players — Phase D: UI and e2e Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Owners add and remove AI players in the lobby from a dropdown of ready models. Everyone sees AI seats, with a bot icon and the model name, on the table. Random fallback moves are tagged "random" in the history and on the board. During setup the owner chooses boards for AI seats. One e2e spec covers the flow.

**Architecture:**
- **Lobby.** `LobbyLive` subscribes to `"ai:readiness"`, calls `Readiness.refresh/0` on connect, renders an "Add an AI player" form for the owner and lists AI rows for everyone.
- **Table.**
  - `GameLive` builds names with `Games.display_names/1`, treats AI ids as always connected, and keeps `@random` from `Games.snapshot/2` / `Games.replay/2`.
  - It passes `@random` to the history drawer and computes the per-board "random move" badge for the latest resolved turn.
- **Setup.** During setup an owner gets a "Choosing for" switch (`#setup-as`). The setup component renders for the selected seat, and submissions for AI seats go through `Games.submit_for_ai_seat/4`.

**Tech Stack:** Phoenix 1.8 / LiveView 1.1, Tailwind 4.1, ExUnit + LazyHTML, Playwright.

**Spec:** `docs/superpowers/specs/2026-10-13-ai-players/2026-10-13-ai-players-design.md` (section "UI", plus the LiveView/e2e parts of "Testing"). Cross-phase contract: `docs/superpowers/plans/2026-10-13-ai-players/README.md`.

## Global Constraints

- **Prerequisites:** phases A–C are merged (README "Cross-phase interfaces"). If any function named there differs, stop and ask.
- Read `helios/AGENTS.md` before touching `helios/` (forms via `<.input>`, icons via `<.icon>`, no inline scripts).
- Kept ids, unchanged: `#invite-form`, `#members-table`, `[data-member]`, `[data-free-slot]`, `#start-game`, `#start-blocker`, `#top-bar`, `#connections` / `#connection-<id>`, `#history-drawer`, `#history-button`, `#west-panel`, `#east-panel`, `#my-board`, `#board-<id>`, `#wonder-setup`, `#claim-<slug>`, `#confirm-wonder`.
- New ids:
  - lobby: `#ai-seat-form`, `#add-ai-seat`, `#ai-seat-<seat id>` (with `data-ai-seat`, `data-model`, `data-ready`), `#remove-ai-seat-<seat id>`, `[data-unavailable]`;
  - table: `[data-ai-icon]`, `#random-badge-<player id>` (with `data-random-badge`), `[data-random]` (history tag);
  - setup: `#setup-as`, `#setup-as-<player id>`.
- AI engine ids are `ai-<seat>`, so `#connection-ai-1` and `#board-ai-3` are valid selectors.
- The UI never decides legality. Buttons only reflect `view.setup` and readiness.
- Touch targets ≥ 44×44 CSS px (`pointer-coarse:min-h-11`); no horizontal scroll at ≥ 360px.
- Tests that seat AI players but drive them by hand use `put_ai_config(players: false)`.
- Stage explicit paths only; never stage `helios/config/dev.exs`.
- Run `cd helios && mix precommit` before each commit. Commit messages end with `Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>`.

## Review Focus

- A guest (non-owner) lobby page lists AI rows but never shows `#ai-seat-form` or `#remove-ai-seat-*`. Forged `add_ai_seat` / `remove_ai_seat` events from a guest are rejected with a flash. (Test in Task 1.)
- A finished game with AI seats (no user rows) renders in the past-games list and replay without crashing on `player.user.name`. (Test in Task 2.)
- The "random move" badge belongs to the *latest resolved turn* only. It disappears when the next turn resolves without a random move, and it never shows during replay. (Test in Task 2.)
- During setup, a guest can't act for an AI seat even by forging `setup_as`. The owner's own seat and the AI seats don't interfere: claiming for an AI seat never changes the owner's claim. (Test in Task 3.)
- On a phone viewport, the AI controls and badges don't add horizontal scroll. (e2e runs the spec in the `mobile` project, Task 4.)

---

### Task 1: Lobby — add, list and remove AI players

**Files:**
- Modify: `helios/lib/helios_web/live/lobby_live.ex`
- Test: `helios/test/helios_web/live/lobby_live_ai_test.exs`

**Interfaces:**
- Consumes:
  - `Lobbies.ai_seats/1`, `add_ai_seat/3`, `remove_ai_seat/3` and `error_message/1`;
  - `Helios.AI.Readiness.ready_models/0`, `ready?/1`, `refresh/0` and `topic/0`;
  - `Helios.AI.Models.seat_labels/1`;
  - `Games.seat_plan/1`.
- Produces: LiveView events `add_ai_seat` (`%{"ai" => %{"model_id" => id}}`) and `remove_ai_seat` (`%{"id" => seat_id}`); `handle_info({:ai_readiness_changed}, …)`.

- [ ] **Step 1: Write the failing tests**

Create `helios/test/helios_web/live/lobby_live_ai_test.exs`:

```elixir
defmodule HeliosWeb.LobbyLiveAITest do
  use HeliosWeb.ConnCase, async: false

  import Phoenix.LiveViewTest
  import Helios.GamesFixtures

  alias Helios.{Games, Lobbies, Repo}
  alias Helios.Lobbies.AISeat

  setup do
    on_exit(&stop_game_servers/0)
    put_ai_config(players: false)
    [{owner, _} = o, {guest, _} = g] = for _ <- 1..2, do: user_with_token_fixture()
    lobby = Lobbies.get_or_create_own_lobby(owner)
    :ok = invite_all(owner, lobby, [guest])
    %{owner: o, guest: g, lobby: lobby}
  end

  defp open(token, lobby), do: live(log_in_conn(build_conn(), token), ~p"/lobby/#{lobby.id}")

  test "the owner picks from ready models only", %{owner: {_, token}, lobby: lobby} do
    {:ok, lv, _html} = open(token, lobby)
    assert has_element?(lv, "#ai-seat-form option[value='fake-first']", "Fake First")
    refute has_element?(lv, "#ai-seat-form option[value='fake-down']")
  end

  test "adding and removing AI players; they take free slots", %{owner: {_, token}, lobby: lobby} do
    {:ok, lv, _html} = open(token, lobby)
    free = fn -> lv |> render() |> LazyHTML.from_fragment() |> LazyHTML.query("[data-free-slot]") |> Enum.count() end
    before = free.()

    lv |> form("#ai-seat-form", ai: %{model_id: "fake-first"}) |> render_submit()
    lv |> form("#ai-seat-form", ai: %{model_id: "fake-first"}) |> render_submit()

    [first, second] = Lobbies.ai_seats(lobby)
    assert has_element?(lv, "#ai-seat-#{first.id}[data-ai-seat][data-model='fake-first']", "Fake First")
    assert has_element?(lv, "#ai-seat-#{second.id}", "Fake First #2")
    assert free.() == before - 2

    lv |> element("#remove-ai-seat-#{first.id}") |> render_click()
    refute has_element?(lv, "#ai-seat-#{first.id}")
    assert has_element?(lv, "#ai-seat-#{second.id}", "Fake First")
  end

  test "guests see AI players but cannot change them", %{owner: {owner, _}, guest: {_, token}, lobby: lobby} do
    {:ok, seat} = Lobbies.add_ai_seat(Helios.Accounts.Scope.for_user(owner), lobby, "fake-first")
    {:ok, lv, _html} = open(token, lobby)

    assert has_element?(lv, "#ai-seat-#{seat.id}", "Fake First")
    refute has_element?(lv, "#ai-seat-form")
    refute has_element?(lv, "#remove-ai-seat-#{seat.id}")

    render_hook(lv, "add_ai_seat", %{"ai" => %{"model_id" => "fake-first"}})
    render_hook(lv, "remove_ai_seat", %{"id" => to_string(seat.id)})
    assert render(lv) =~ "Only the table leader can do that"
    assert length(Lobbies.ai_seats(lobby)) == 1
  end

  test "a seated model that is no longer ready is flagged", %{owner: {_, token}, lobby: lobby} do
    seat = Repo.insert!(%AISeat{lobby_id: lobby.id, model_id: "fake-down"})
    {:ok, lv, _html} = open(token, lobby)
    assert has_element?(lv, "#ai-seat-#{seat.id}[data-ready='false'] [data-unavailable]")
  end

  test "the owner alone plus two AI players can start", %{owner: {owner, token}, lobby: lobby} do
    {:ok, lv, _html} = open(token, lobby)
    assert has_element?(lv, "#start-game[disabled]")

    for _ <- 1..2, do: lv |> form("#ai-seat-form", ai: %{model_id: "fake-first"}) |> render_submit()
    refute has_element?(lv, "#start-game[disabled]")

    lv |> element("#start-game") |> render_click()
    game = Games.active_game_for_lobby(lobby.id)
    assert_redirect(lv, ~p"/game/#{game.id}")
    assert Enum.map(Games.players(game), & &1.user_id) == [owner.id, nil, nil]
  end
end
```

The guest in `setup` is invited but not connected, so the owner plus two AI seats make 3 seats and the guest is not seated.

- [ ] **Step 2: Run to verify they fail**

Run: `cd helios && mix test test/helios_web/live/lobby_live_ai_test.exs`
Expected: FAIL (no `#ai-seat-form`).

- [ ] **Step 3: Implement**

In `helios/lib/helios_web/live/lobby_live.ex`:

- add aliases: `alias Helios.AI.{Models, Readiness}`;
- in `setup/3`, inside `if connected?(socket) do`, add:

```elixir
      Phoenix.PubSub.subscribe(Helios.PubSub, Readiness.topic())
      Readiness.refresh()
```

  and add `|> assign(:ai_form, to_form(%{"model_id" => ""}, as: :ai))` next to `invite_form`;
- in the template, after the `#invite-form` `</.form>`, add:

```heex
        <.form
          :if={@owner?}
          for={@ai_form}
          id="ai-seat-form"
          phx-submit="add_ai_seat"
          class="flex flex-col gap-2 sm:flex-row sm:items-end sm:gap-3"
        >
          <div class="flex-1">
            <.input
              field={@ai_form[:model_id]}
              type="select"
              label="Add an AI player"
              options={@ai_options}
              prompt={if @ai_options == [], do: "No AI models available", else: "Select model"}
            />
          </div>
          <.button
            id="add-ai-seat"
            type="submit"
            disabled={@ai_options == [] or @free_slots == 0}
            title={ai_hint(@ai_options, @free_slots)}
            class="w-full rounded-lg bg-zinc-900 px-5 py-2 font-semibold text-white shadow-sm transition hover:bg-zinc-700 disabled:cursor-not-allowed disabled:opacity-40 sm:mb-2 sm:w-auto pointer-coarse:min-h-11"
          >
            Add AI player
          </.button>
        </.form>
```

- in `#members-table` `<tbody>`, between the member rows and the free-slot rows, add:

```heex
              <tr
                :for={seat <- @ai_rows}
                id={"ai-seat-#{seat.id}"}
                data-ai-seat
                data-model={seat.model_id}
                data-ready={to_string(seat.ready?)}
                class="border-t border-zinc-900/10 text-zinc-900"
              >
                <td class="px-4 py-3">
                  <span class="flex min-w-0 flex-wrap items-center gap-2 font-medium">
                    <.icon name="hero-cpu-chip" class="size-4 shrink-0 text-sky-700" />
                    <span class="truncate">{seat.label}</span>
                    <span
                      :if={!seat.ready?}
                      data-unavailable
                      class="rounded bg-amber-100 px-1.5 py-0.5 text-xs font-semibold text-amber-800"
                    >
                      unavailable — moves will be random
                    </span>
                  </span>
                </td>
                <td :if={@owner?} class="px-4 py-3 text-right">
                  <button
                    id={"remove-ai-seat-#{seat.id}"}
                    type="button"
                    phx-click="remove_ai_seat"
                    phx-value-id={seat.id}
                    aria-label={"Remove #{seat.label}"}
                    class="inline-flex size-8 items-center justify-center rounded-md bg-zinc-900 text-white transition hover:bg-red-700 active:bg-red-800 pointer-coarse:size-11"
                  >
                    <.icon name="hero-x-mark" class="size-4" />
                  </button>
                </td>
              </tr>
```

- add the events after `uninvite`:

```elixir
  def handle_event("add_ai_seat", %{"ai" => %{"model_id" => model_id}}, socket) do
    %{current_scope: scope, lobby: lobby} = socket.assigns

    case Lobbies.add_ai_seat(scope, lobby, model_id) do
      {:ok, _seat} -> {:noreply, socket |> load_members() |> assign_game_state()}
      {:error, reason} -> {:noreply, put_flash(socket, :error, Lobbies.error_message(reason))}
    end
  end

  def handle_event("add_ai_seat", _params, socket),
    do: {:noreply, put_flash(socket, :error, Lobbies.error_message(:model_unavailable))}

  def handle_event("remove_ai_seat", %{"id" => seat_id}, socket) do
    %{current_scope: scope, lobby: lobby} = socket.assigns

    case Lobbies.remove_ai_seat(scope, lobby, seat_id) do
      :ok -> {:noreply, socket |> load_members() |> assign_game_state()}
      {:error, reason} -> {:noreply, put_flash(socket, :error, Lobbies.error_message(reason))}
    end
  end

  def handle_event("remove_ai_seat", _params, socket),
    do: {:noreply, put_flash(socket, :error, Lobbies.error_message(:no_ai_seat))}
```

- add `def handle_info({:ai_readiness_changed}, socket), do: {:noreply, assign_derived(socket)}` next to the other `handle_info` clauses;
- in `load_members/1`, add `|> assign(:ai_seats, Lobbies.ai_seats(socket.assigns.lobby))` before `assign_derived()`;
- in `assign_derived/1`, add `ai_seats: ai_seats` to the destructured assigns, and before the final pipeline:

```elixir
    labels = Models.seat_labels(Enum.map(ai_seats, & &1.model_id))

    ai_rows =
      Enum.zip_with(ai_seats, labels, fn seat, label ->
        %{id: seat.id, model_id: seat.model_id, label: label, ready?: Readiness.ready?(seat.model_id)}
      end)

    ai_options = for model <- Readiness.ready_models(), do: {model.label, model.id}
```

  then add `|> assign(ai_rows: ai_rows, ai_options: ai_options)` to the pipeline. Change the free-slots expression to `max(Lobbies.max_players() - length(members) - length(ai_seats), 0)`;
- add the hint helper:

```elixir
  defp ai_hint([], _free), do: "No AI model is available right now"
  defp ai_hint(_options, 0), do: "The table is full"
  defp ai_hint(_options, _free), do: nil
```

`assign_derived/1` runs from `setup/3` before `load_members/1` the first time (via `load_connected/1`). Make sure `:ai_seats` is assigned before the first `assign_derived/1` call: add `|> assign(:ai_seats, Lobbies.ai_seats(lobby))` in `setup/3` before `load_members()`.

- [ ] **Step 4: Run the tests**

Run: `cd helios && mix test test/helios_web/live/lobby_live_ai_test.exs test/helios_web/live/lobby_live_test.exs test/helios_web/live/lobby_live_game_test.exs`
Expected: PASS. If an existing free-slot count assertion changes, it's only because AI seats now take slots. No existing test seats AI, so none should change.

- [ ] **Step 5: Precommit and commit**

```bash
git add helios/lib/helios_web/live/lobby_live.ex helios/test/helios_web/live/lobby_live_ai_test.exs
git commit -m "feat(web): owners add and remove AI players in the lobby

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 2: Table — AI names, icons, connection and "random" tags

**Files:**
- Modify: `helios/lib/helios_web/live/game_live.ex`, `helios/lib/helios_web/game_format.ex`, `helios/lib/helios_web/components/game_components/player_board.ex`, `helios/lib/helios_web/components/game_components/top_bar.ex`, `helios/lib/helios_web/components/game_components/history_drawer.ex`, `helios/lib/helios_web/live/past_games_live.ex`
- Test: `helios/test/helios_web/live/game_live_ai_test.exs`, `helios/test/helios_web/game_format_test.exs`

**Interfaces:**
- Consumes: `Games.display_names/1`, `ai_player_ids/1`, `player_id/1`, `snapshot/2` (`random`), `replay/2` (`random`); `Helios.Games.RandomMoves.random?/3`.
- Produces:
  - `GameFormat.last_random(history, random) :: MapSet.t(String.t())`;
  - `player_board/1` attrs `ai` and `random` (booleans, default false);
  - `top_bar/1` attr `ai_ids` (MapSet, default empty);
  - the history drawer's `ctx` may carry `:random`;
  - GameLive assigns `@ai_ids` and `@random`.

- [ ] **Step 1: Write the failing tests**

Add to `helios/test/helios_web/game_format_test.exs`:

```elixir
  describe "last_random/2" do
    test "players whose play in the latest turn was random" do
      plays = fn players -> Enum.map(players, &%{player: &1, action: :discarded}) end
      history = [
        {:turn, %{age: 1, turn: 1, kind: :regular, plays: plays.(["1", "ai-1"]), coins: []}},
        {:turn, %{age: 1, turn: 2, kind: :regular, plays: plays.(["1", "ai-1", "ai-2"]), coins: []}}
      ]

      random = MapSet.new([{"ai-1", 1, 1, :regular}, {"ai-2", 1, 2, :regular}])
      assert GameFormat.last_random(history, random) == MapSet.new(["ai-2"])
      assert GameFormat.last_random([], random) == MapSet.new()
    end
  end
```

Create `helios/test/helios_web/live/game_live_ai_test.exs`:

```elixir
defmodule HeliosWeb.GameLiveAITest do
  use HeliosWeb.ConnCase, async: false

  import Phoenix.LiveViewTest
  import Helios.GamesFixtures

  alias Helios.{GameDriver, Games, Repo}
  alias Helios.Games.Game

  setup do
    on_exit(&stop_game_servers/0)
    put_ai_config(players: false)
    {user, token} = user_with_token_fixture()
    game = game_fixture([user, {:ai, "fake-first"}, {:ai, "fake-first"}])
    %{user: user, token: token, game: game}
  end

  defp open(token, game), do: live(log_in_conn(build_conn(), token), ~p"/game/#{game.id}")

  defp ai_discard(game, seat, source) do
    {:ok, view} = Games.view(game.id, {:ai, seat})
    :ok = Games.submit(game.id, {:ai, seat}, {:discard, hd(view.hand).name}, source)
  end

  test "AI seats show their model name, a bot icon and are always connected", %{token: token, game: game} do
    {:ok, lv, _html} = open(token, game)

    assert has_element?(lv, "[data-player='ai-1']", "Fake First")
    assert has_element?(lv, "[data-player='ai-2']", "Fake First #2")
    assert has_element?(lv, "[data-player='ai-1'] [data-ai-icon]")
    refute has_element?(lv, "#my-board [data-ai-icon]")
    assert has_element?(lv, "#connection-ai-1[data-connected='true'] [data-ai-icon]")
  end

  test "a random move is tagged in history and badged for the latest turn only",
       %{user: user, token: token, game: game} do
    {:ok, lv, _html} = open(token, game)

    ai_discard(game, 1, :random)
    ai_discard(game, 2, :ai)
    {:ok, view} = Games.view(game.id, user.id)
    :ok = Games.submit(game.id, user.id, {:discard, hd(view.hand).name})
    _ = render(lv)

    assert has_element?(lv, "#random-badge-ai-1[data-random-badge]")
    refute has_element?(lv, "#random-badge-ai-2")
    html = lv |> element("#history-drawer") |> render()
    assert html |> LazyHTML.from_fragment() |> LazyHTML.query("[data-random]") |> Enum.count() == 1

    ai_discard(game, 1, :ai)
    ai_discard(game, 2, :ai)
    {:ok, view} = Games.view(game.id, user.id)
    :ok = Games.submit(game.id, user.id, {:discard, hd(view.hand).name})
    _ = render(lv)

    refute has_element?(lv, "[data-random-badge]")
    assert has_element?(lv, "#history-drawer [data-random]")
  end

  test "finished games with AI seats replay and list in past games",
       %{user: user, token: token, game: game} do
    ids = [to_string(user.id), "ai-1", "ai-2"]
    {view_fun, _} = GameDriver.games_funs(game.id)

    submit = fn
      "ai-" <> seat, action -> Games.submit(game.id, {:ai, String.to_integer(seat)}, action, :random)
      _human, action -> Games.submit(game.id, user.id, action)
    end

    {:game_over, _} = GameDriver.play_until(ids, %{}, view_fun, submit, fn _ -> false end)
    stop_game_servers()
    assert %Game{status: "finished"} = Repo.get!(Game, game.id)

    {:ok, lv, _html} = open(token, game)
    assert has_element?(lv, "#history-drawer [data-random]")
    refute has_element?(lv, "[data-random-badge]")

    {:ok, _past, html} = live(log_in_conn(build_conn(), token), ~p"/games")
    assert html =~ "Fake First #2"
  end
end
```

- [ ] **Step 2: Run to verify they fail**

Run: `cd helios && mix test test/helios_web/live/game_live_ai_test.exs test/helios_web/game_format_test.exs`
Expected: FAIL. Mounting crashes on `&1.user.name` for AI seats, and `last_random/2` is undefined.

- [ ] **Step 3: `GameFormat.last_random/2`**

In `helios/lib/helios_web/game_format.ex`, add after `statuses/1`:

```elixir
  @doc "Players whose play in the latest resolved turn was a random AI fallback."
  def last_random(history, random) do
    case history |> Enum.filter(&match?({:turn, _}, &1)) |> List.last() do
      {:turn, event} ->
        for play <- event.plays,
            Helios.Games.RandomMoves.random?(random, event, play.player),
            into: MapSet.new(),
            do: play.player

      nil ->
        MapSet.new()
    end
  end
```

- [ ] **Step 4: Components**

`helios/lib/helios_web/components/game_components/player_board.ex`:
- add the attributes:

```elixir
  attr :ai, :boolean, default: false, doc: "an AI seat (bot icon)"
  attr :random, :boolean, default: false, doc: "this player's latest play was a random fallback"
```

- as the first child of `<section …>`, add:

```heex
      <span
        :if={@random}
        id={"random-badge-#{@player.name}"}
        data-random-badge
        class="self-start rounded bg-amber-100 px-1.5 py-0.5 text-xs font-semibold text-amber-800"
      >
        random move
      </span>
```

- after each of the two name spans (`<span class="truncate font-semibold text-zinc-900">{@name}</span>` in the phone toggle, and `<span :if={!@mine} class="min-w-0 truncate font-semibold text-zinc-900">{@name}</span>` in the header), add:

```heex
            <span :if={@ai} data-ai-icon title="AI player" class="shrink-0 text-sky-700">
              <.icon name="hero-cpu-chip" class="size-4" />
            </span>
```

`helios/lib/helios_web/components/game_components/top_bar.ex`:
- add `attr :ai_ids, :any, default: MapSet.new(), doc: "engine ids of AI seats"`;
- inside each `<li :for={id <- @seats} …>`, after the name span, add:

```heex
          <span :if={MapSet.member?(@ai_ids, id)} data-ai-icon class="shrink-0 text-white/80">
            <.icon name="hero-cpu-chip" class="size-3.5" />
          </span>
```

`helios/lib/helios_web/components/game_components/history_drawer.ex`, in the `<p :for={play <- event.plays} …>` line, after `{HistoryText.play(play, @ctx)}`, add:

```heex
              <span
                :if={random_play?(@ctx, event, play)}
                data-random
                class="rounded bg-amber-100 px-1 text-xs font-semibold text-amber-800"
              >
                random
              </span>
```

and the helper:

```elixir
  defp random_play?(ctx, event, play),
    do: Helios.Games.RandomMoves.random?(Map.get(ctx, :random, MapSet.new()), event, play.player)
```

- [ ] **Step 5: `GameLive`**

In `helios/lib/helios_web/live/game_live.ex`:
- in `setup/3`, replace the `names = …` line with:

```elixir
    players = Games.players(game)
```

  and add to the `assign(…)` list: `names: Games.display_names(players)`, `ai_ids: Games.ai_player_ids(players)` and `random: MapSet.new()` (in place of the old `names: names`);
- `assign_connected/1`:

```elixir
  # AI seats have no browser: they always count as connected.
  defp assign_connected(socket) do
    humans = Games.connected_user_ids(Games.topic(socket.assigns.game.id))
    assign(socket, :connected, MapSet.union(humans, socket.assigns.ai_ids))
  end
```

- `mount_active/3` and `refresh/1`: match `{:ok, %{view: view, history: history, random: random}}` and add `|> assign(:random, random)` next to `assign(:history, history)`;
- `enter_replay/2`: match `%{view: view, history: history, frames: frames, random: random}` and add `random: random` to the first `assign`;
- `assign_finished_view/3`: replace `ids = for player <- Games.players(game), do: to_string(player.user_id)` with `ids = Enum.map(Games.players(game), &Games.player_id/1)`;
- template:
  - pass `ctx={%{me: @view.me, names: @names, view: @view, random: @random}}` to `<.history_drawer>`;
  - pass `ai_ids={@ai_ids}` to `<.top_bar>`;
  - pass `ai_ids={@ai_ids}` and `random_now={if @replay, do: MapSet.new(), else: GameFormat.last_random(@history, @random)}` to **both** `<.table_area …>` calls;
- `table_area/1`: add `attr :ai_ids, :any, default: MapSet.new()` and `attr :random_now, :any, default: MapSet.new()`. On every `<.player_board …>` inside it, add `ai={MapSet.member?(@ai_ids, <player>.name)}` and `random={MapSet.member?(@random_now, <player>.name)}`, where `<player>` is `@west`, `@me`, `@east` or `player` respectively.

`helios/lib/helios_web/live/past_games_live.ex`, `row/2`: replace the `players:` mapping with:

```elixir
      players:
        (fn ->
           names = Games.display_names(game.players)

           Enum.map(game.players, fn player ->
             id = Games.player_id(player)
             %{name: Map.fetch!(names, id), winner?: get_in(scores, [id, "rank"]) == 1}
           end)
         end).()
```

Or, more idiomatically, compute `names = Games.display_names(game.players)` at the top of `row/2` and use it inside the `Enum.map`.

- [ ] **Step 6: Run the tests**

Run: `cd helios && mix test test/helios_web`
Expected: PASS. That's the new tests plus every existing LiveView and component test, which keep their previous behaviour because the new attrs default to empty.

- [ ] **Step 7: Precommit and commit**

```bash
git add helios/lib/helios_web/live/game_live.ex helios/lib/helios_web/game_format.ex \
  helios/lib/helios_web/components/game_components/player_board.ex \
  helios/lib/helios_web/components/game_components/top_bar.ex \
  helios/lib/helios_web/components/game_components/history_drawer.ex \
  helios/lib/helios_web/live/past_games_live.ex \
  helios/test/helios_web/live/game_live_ai_test.exs helios/test/helios_web/game_format_test.exs
git commit -m "feat(web): AI seats on the table and random-move tags

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 3: Setup screen — the owner chooses for AI seats

**Files:**
- Modify: `helios/lib/helios_web/live/game_live.ex`, `helios/lib/helios_web/components/game_components/wonder_setup.ex`
- Test: `helios/test/helios_web/live/game_live_ai_setup_test.exs`

**Interfaces:**
- Consumes: `Games.submit_for_ai_seat/4`, `Helios.Games.Actor.from_player_id/1`, `Lobbies.fetch_lobby/1`.
- Produces:
  - `wonder_setup/1` attrs: `viewer` (string or nil, defaulting to `me`; it decides who is labelled "You"), `acting` (list of `%{id, name}`, default `[]`; the "Choosing for" switch renders when it has more than one entry) and `ai_ids` (MapSet, default empty);
  - the LiveView event `setup_as` (`%{"seat" => player_id}`);
  - GameLive assigns `@owner?`, `@setup_as` and `@setup_acting`.

- [ ] **Step 1: Write the failing tests**

Create `helios/test/helios_web/live/game_live_ai_setup_test.exs`:

```elixir
defmodule HeliosWeb.GameLiveAISetupTest do
  use HeliosWeb.ConnCase, async: false

  import Phoenix.LiveViewTest
  import Helios.GamesFixtures

  alias Helios.Games

  setup do
    on_exit(&stop_game_servers/0)
    put_ai_config(players: false)
    [{owner, ot}, {guest, gt}] = for _ <- 1..2, do: user_with_token_fixture()
    game = game_fixture([owner, guest, {:ai, "fake-first"}], selection: :choose_board)
    %{owner: owner, ot: ot, guest: guest, gt: gt, game: game}
  end

  defp open(token, game), do: live(log_in_conn(build_conn(), token), ~p"/game/#{game.id}")

  test "the owner switches to the AI seat, claims and confirms for it",
       %{owner: owner, ot: ot, gt: gt, game: game} do
    {:ok, lv, _html} = open(ot, game)
    {:ok, guest_lv, _html} = open(gt, game)

    assert has_element?(lv, "#setup-as-#{owner.id}[aria-pressed='true']", "You")
    assert has_element?(lv, "#setup-as-ai-2", "Fake First")
    refute has_element?(guest_lv, "#setup-as")

    lv |> element("#setup-as-ai-2") |> render_click()
    lv |> element("#claim-gizah") |> render_click()
    assert has_element?(lv, "#setup-wonder-gizah[data-holder='ai-2'][data-mine]")
    assert has_element?(lv, "#setup-wonder-gizah [data-holder-name]", "Fake First")
    lv |> element("#confirm-wonder") |> render_click()

    lv |> element("#setup-as-#{owner.id}") |> render_click()
    refute has_element?(lv, "#setup-wonder-gizah[data-mine]")
    lv |> element("#claim-rhodos") |> render_click()
    assert has_element?(lv, "#setup-wonder-rhodos[data-holder='#{owner.id}'] [data-holder-name]", "You")

    {:ok, %{setup: setup}} = Games.view(game.id, owner.id)
    assert %{wonder: "Gizah", confirmed: true} = Enum.find(setup.seats, &(&1.player == "ai-2"))
    assert %{wonder: "Rhódos", confirmed: false} = Enum.find(setup.seats, &(&1.player == to_string(owner.id)))
  end

  test "a guest cannot act for the AI seat, even with a forged event", %{gt: gt, game: game} do
    {:ok, guest_lv, _html} = open(gt, game)
    render_hook(guest_lv, "setup_as", %{"seat" => "ai-2"})
    guest_lv |> element("#claim-gizah") |> render_click()

    {:ok, %{setup: setup}} = Games.view(game.id, {:ai, 2})
    assert %{wonder: nil} = Enum.find(setup.seats, &(&1.player == "ai-2"))
  end
end
```

- [ ] **Step 2: Run to verify they fail**

Run: `cd helios && mix test test/helios_web/live/game_live_ai_setup_test.exs`
Expected: FAIL (no `#setup-as`).

- [ ] **Step 3: Component**

In `helios/lib/helios_web/components/game_components/wonder_setup.ex`:
- add the attrs:

```elixir
  attr :viewer, :string, default: nil, doc: "the signed-in player's id (\"You\"); defaults to `me`"
  attr :acting, :list, default: [], doc: "seats the viewer may choose for: [%{id, name}]"
  attr :ai_ids, :any, default: MapSet.new(), doc: "engine ids of AI seats"
```

  and update the moduledoc: "`me` is the seat being chosen for: the viewer, or, for the table owner, one of the AI seats (`acting`).";
- in `wonder_setup/1`, set `viewer: assigns.viewer || assigns.me` in the `assign(assigns, …)` call;
- after the `<header>…</header>`, add:

```heex
      <div
        :if={length(@acting) > 1}
        id="setup-as"
        role="group"
        aria-label="Choosing for"
        class="flex flex-wrap gap-2"
      >
        <button
          :for={seat <- @acting}
          id={"setup-as-#{seat.id}"}
          type="button"
          phx-click="setup_as"
          phx-value-seat={seat.id}
          aria-pressed={to_string(seat.id == @me)}
          class={[
            "rounded-lg px-3 py-1.5 text-sm font-semibold ring-1 transition pointer-coarse:min-h-11",
            if(seat.id == @me,
              do: "bg-zinc-900 text-white ring-zinc-900",
              else: "bg-white text-zinc-900 ring-zinc-300 hover:bg-zinc-100"
            )
          ]}
        >
          {seat.name}
        </button>
      </div>
```

- pass `viewer={@viewer}` to `<.board_tile …>`, add `attr :viewer, :string, required: true` to `board_tile/1`, and change its holder label to `{if(@holder.player == @viewer, do: "You", else: GameFormat.player_name(@names, @holder.player))}`;
- in the `choose_side` seat list, change `else: "choosing"` to `else: if(MapSet.member?(@ai_ids, seat.player), do: "owner is choosing", else: "choosing")`.

- [ ] **Step 4: `GameLive`**

In `helios/lib/helios_web/live/game_live.ex`:
- `setup/3`: add

```elixir
    owner? =
      case Lobbies.fetch_lobby(game.lobby_id) do
        {:ok, lobby} -> lobby.owner_id == user.id
        {:error, _reason} -> false
      end
```

  and assign `owner?: owner?, setup_as: nil, setup_acting: []`;
- the setup clause of `assign_view/2`: compute the controllable seats and keep the current choice when it is still valid:

```elixir
  defp assign_view(socket, %{phase: %{kind: :setup}} = view) do
    acting =
      [%{id: view.me, name: "You"}] ++
        if socket.assigns.owner?,
          do:
            for(
              seat <- view.setup.seats,
              MapSet.member?(socket.assigns.ai_ids, seat.player),
              do: %{id: seat.player, name: GameFormat.player_name(socket.assigns.names, seat.player)}
            ),
          else: []

    setup_as =
      if Enum.any?(acting, &(&1.id == socket.assigns[:setup_as])),
        do: socket.assigns.setup_as,
        else: view.me

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
      discard_minimised: false,
      setup_acting: acting,
      setup_as: setup_as
    )
  end
```

- template: change `<.wonder_setup …>` to

```heex
            <.wonder_setup
              setup={@view.setup}
              me={@setup_as}
              viewer={@view.me}
              acting={@setup_acting}
              ai_ids={@ai_ids}
              names={@names}
              wonders={@wonder_names}
              previews={@setup_previews}
            />
```

- add the event (before `show_side`):

```elixir
  # Only seats in @setup_acting (the viewer, plus AI seats for the owner) can be chosen.
  def handle_event("setup_as", %{"seat" => seat}, socket) do
    if Enum.any?(socket.assigns.setup_acting, &(&1.id == seat)),
      do: {:noreply, assign(socket, :setup_as, seat)},
      else: {:noreply, socket}
  end
```

- `submit_setup/2`: replace the `result = …` line with:

```elixir
    result =
      with {:ok, action} <- parsed do
        if socket.assigns.setup_as in [nil, socket.assigns.view.me] do
          Games.submit(game.id, user.id, action)
        else
          {:ai, seat} = Helios.Games.Actor.from_player_id(socket.assigns.setup_as)
          Games.submit_for_ai_seat(socket.assigns.current_scope, game.id, seat, action)
        end
      end
```

`setup_as` only ever holds ids from `@setup_acting`, so `from_player_id/1` only sees `ai-<n>` here. The server re-checks ownership in `submit_for_ai_seat/4` anyway.

- [ ] **Step 5: Run the tests**

Run: `cd helios && mix test test/helios_web`
Expected: PASS, including the existing `game_live_wonder_setup_test.exs` and `wonder_setup_test.exs`, which never pass the new attrs.

- [ ] **Step 6: Precommit and commit**

```bash
git add helios/lib/helios_web/live/game_live.ex helios/lib/helios_web/components/game_components/wonder_setup.ex \
  helios/test/helios_web/live/game_live_ai_setup_test.exs
git commit -m "feat(web): the table owner chooses boards for AI seats during setup

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 4: e2e — AI players end to end

**Files:**
- Create: `e2e/tests/ai-players.spec.ts`

**Interfaces:**
- Consumes: the e2e fake models from phase A (`fake-first` "Fake AI", deadline 2,000 ms; `fake-timeout` "Fake AI (slow)", deadline 500 ms); e2e helpers `newPlayer`, `waitForLive`, `waitForTurn`, `playFirstBuildableOrDiscard` (`e2e/tests/support/game.ts`).

- [ ] **Step 1: Write the spec**

Create `e2e/tests/ai-players.spec.ts`:

```ts
import { expect, test } from "@playwright/test";
import { newPlayer, playFirstBuildableOrDiscard, waitForLive, waitForTurn } from "./support/game";
import { expectNoHorizontalScroll } from "./support/layout";

test("the owner seats AI players; they move, and a slow one is tagged random", async ({ browser }) => {
  test.setTimeout(120_000);
  const owner = await newPlayer(browser, "ai");
  const page = owner.page;
  await waitForLive(page);

  for (const model of ["fake-first", "fake-timeout"]) {
    await page.locator("#ai-seat-form select").selectOption(model);
    await page.locator("#add-ai-seat").click();
    await expect(page.locator(`#members-table [data-ai-seat][data-model='${model}']`)).toHaveCount(1);
  }
  await expectNoHorizontalScroll(page);

  await expect(page.locator("#start-game")).toBeEnabled();
  await page.locator("#start-game").click();
  await expect(page).toHaveURL(/\/game\/[0-9a-f-]{36}$/);
  await waitForLive(page);

  await waitForTurn(page, 1, 1);
  await expect(page.locator("#connection-ai-1[data-connected='true']")).toBeVisible();
  await expect(page.locator("[data-player='ai-1']").first()).toContainText("Fake AI");

  await playFirstBuildableOrDiscard(page);
  await waitForTurn(page, 1, 2);

  await expect(page.locator("#random-badge-ai-2")).toBeVisible();
  await expect(page.locator("#random-badge-ai-1")).toHaveCount(0);
  await expectNoHorizontalScroll(page);
  await page.locator("#history-button").click();
  await expect(page.locator("#history-drawer [data-random]")).toHaveCount(1);

  await owner.context.close();
});
```

Seat order is the owner, then AI seats in the order they were added. So `ai-1` is `fake-first` and `ai-2` is `fake-timeout`, which gets the random tag.

- [ ] **Step 2: Run it**

Run: `cd e2e && npm ci && npx playwright install chromium && npx playwright test tests/ai-players.spec.ts`
Expected: PASS in the `chromium` and `mobile` projects. If `[data-player='ai-1']` isn't visible on the phone project because the board is folded, assert on `#connection-ai-1` text instead. Don't force-open the board.

- [ ] **Step 3: Full checks**

Run: `cd helios && mix precommit`
Run: `cd e2e && npx playwright test`
Expected: both are green.

- [ ] **Step 4: Commit**

```bash
git add e2e/tests/ai-players.spec.ts
git commit -m "test(e2e): AI players seated, moving and tagged random

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```
