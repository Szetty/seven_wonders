# Table Polish — Phase B: Discard Pile, Discard Replay, Discard Animation and Replay URL Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** The Halikarnassós discard turn is visible and animated: a discard-pile marker sits in the status bar, the chooser (and only the chooser) sees the cards they could pick when replaying that turn, a card built from the discard pile flies onto its board live and in replay, and reloading a replay returns to the same frame.

**Architecture:** Helios only, no rule change (`ENGINE_VERSION` unchanged). Replay frames copy `discard_pile` / `discard_count` from the engine view the frame is taken from; the engine already fills `discard_pile` only for the player whose build-from-discard turn it is, so replay secrecy equals live secrecy. `TurnFx.payload/2` names a fly origin (`from`) for `:built_from_discard` plays; a `.DiscardRects` colocated hook snapshots the picker / replay grid card rects into `window.sevenWondersDiscardRects` because those elements are gone when the turn resolves. The replay frame index moves into the URL (`?frame=N`): frame changes `push_patch(replace: true)` and `handle_params/3` shows the frame.

**Tech Stack:** Phoenix 1.8 / LiveView 1.1 (colocated hooks, `handle_params/3`, `push_patch/2`), Web Animations API, Tailwind 4.1, ExUnit + LazyHTML.

**Spec:** `docs/superpowers/specs/2026-10-11-table-polish/2026-10-11-table-polish-design.md` (sections "C — Discard extra turn in replay, discard-pile marker, animation" and "D — Replay position in the URL"). Cross-phase interfaces: `docs/superpowers/plans/2026-10-11-table-polish/README.md`.

**Prerequisite:** phase A (`2026-10-11-phase-a-status-bar.md`) is merged: `top_bar/1` has attr `replay` and renders `#pass-row` (holding `#pass-direction`) when the phase is not `:game_over`; `game_live.ex` passes `replay={@replay != nil}`. If `top_bar.ex` does not look like phase A's Task 2/3 code, stop and ask.

## Global Constraints

- Read `helios/AGENTS.md` before touching `helios/` (colocated hooks: `<script :type={Phoenix.LiveView.ColocatedHook} name=".Name">`, names start with `.`; never inline `<script>` otherwise; `<.icon>` for icons).
- **No rule change.** Nothing in `core/` changes; Elixir only copies what the engine view already contains.
- **Hidden information:** a replay frame may carry `discard_pile` only if the viewer saw it live (their own build-from-discard turn). Never compute the discard contents in Elixir; never read another player's view to build a frame.
- `#discard-pile` is rendered **once** (unique id; it is the fly origin for observers). It lives in `#pass-row`, which phase A places in phone row 2 and inside the desktop row on `sm`+.
- DOM ids tests and e2e use stay: `#discard-picker`, `#discard-pick-<index>`, `#replay-hand`, `#replay-position`, `#replay-first/prev/next/last/play`, `#history-<index>` links (`phx-click="replay_goto"`). New ids: `#discard-pile`, `#replay-discard`, `#replay-discard-<slug>`.
- Animations: skipped under `prefers-reduced-motion: reduce` (the `.TurnFx` `play` early return already covers this); nothing animates on mount; a turn's animation stays ≤ 1.2 s; ghosts live in `#fx-layer` (`pointer-events-none`).
- Autoplay and `show_scores` are not persisted; a reload pauses (spec D).
- Stage explicit paths only (never `git add -A`/`.`); never stage `helios/config/config.exs` or `helios/config/dev.exs`.
- Run `cd helios && mix precommit` before each commit.

## Review Focus

- Secrecy: in a finished game where Halikarnassós built from the discard, **no** frame of any other viewer and none of the chooser's other frames has a non-nil `discard_pile`; the chooser's discard frame has exactly the engine's buildable list. (Task 1 `replay_test.exs`.)
- `#discard-pile` appears exactly once, inside `#pass-row`, is absent at count 0, and keeps `#pass-row` alive on game over only when the count is positive; replay shows the *frame's* count, not the final one. (Task 2 component and LiveView tests.)
- A card built from the discard pile: the builder's payload has `from: "discard-card-<slug>"`, every other viewer's `from: "discard-pile"`, live and in replay; other built plays carry no `from`. (Task 4 `turn_fx_test.exs` and LiveView tests.)
- The chooser's replay frame shows `#replay-discard` with the chosen card marked; other viewers on the same frame index see no `#replay-discard`. (Task 3 LiveView test.)
- `?frame=` with garbage, negative or out-of-range values never crashes (garbage → final, out of range → clamped); the final frame has no param; `replay_next` (and autoplay ticks) patch the URL without `handle_params` re-running `goto` (which would cancel autoplay and the age summary). (Task 5 LiveView tests.)

---

### Task 1: Replay frames keep the discard pile and count

**Files:**
- Modify: `helios/lib/helios/games.ex` (`frame_snapshot/2`, `replay/2` doc)
- Modify: `helios/test/support/games_fixtures.ex` (new `discard_build_game_fixture/1`)
- Test: `helios/test/helios/games/replay_test.exs`

**Interfaces:**
- Consumes: `Helios.GameDriver.find_seed/4`, `play_until/5`, `games_funs/1`, `built_names/2` (existing); `Core.view/2` field `discard_pile :: [String.t()] | nil` (buildable discard names, sorted, only in the viewer's own build-from-discard turn) and `discard_count :: non_neg_integer()`.
- Produces: replay frame shape `%{players, hand, phase, discard_pile: [String.t()] | nil, discard_count: non_neg_integer(), my_choice, event_index}`.
- Produces: `Helios.GamesFixtures.discard_build_game_fixture(users) :: %Helios.Games.Game{status: "finished"}` — 3 users; seat 0 plays Halikarnassós B with strategy `:wonder`, the others Gizah A / Rhódos A with `:build`; the seed is the first one whose playthrough reaches a build-from-discard turn; the game is played to game over through `Helios.Games` and re-read from the DB. Used by Tasks 3, 4 and 5.

- [ ] **Step 1: Write the failing tests**

Add to `helios/test/support/games_fixtures.ex` (after `game_fixture/2`):

```elixir
  @discard_wonders [{"Halikarnassós", :b}, {"Gizah", :a}, {"Rhódos", :a}]

  @doc """
  A finished 3-player game in which the first user (Halikarnassós B) built
  from the discard pile at least once. The seed is searched in a throwaway
  engine, then the game is persisted and played to the end through
  `Helios.Games`, so its action log replays like any real game.
  """
  def discard_build_game_fixture([_, _, _] = users) do
    [first | rest] = ids = Enum.map(users, &to_string(&1.id))
    strategies = Map.new([{first, :wonder} | Enum.map(rest, &{&1, :build})])
    stop? = &match?(%{phase: %{kind: :extra_turn, extra_turn_kind: :build_from_discard}}, &1)
    seed = Helios.GameDriver.find_seed(ids, @discard_wonders, strategies, stop?)
    game = game_fixture(users, seed: seed, wonders: @discard_wonders)
    {view_fun, submit_fun} = Helios.GameDriver.games_funs(game.id)

    {:game_over, _view} =
      Helios.GameDriver.play_until(ids, strategies, view_fun, submit_fun, fn _ -> false end)

    Repo.get!(Game, game.id)
  end
```

(The engine skips a build-from-discard turn when nothing is buildable, and `GameDriver.choose/2` picks the first buildable name, so the playthrough never stalls.)

Append to `helios/test/helios/games/replay_test.exs` (inside the module; the module `setup` still plays its own discard-only game, which this test ignores):

```elixir
  describe "a game where Halikarnassós builds from the discard pile" do
    setup do
      users = for _ <- 1..3, do: player_fixture()
      %{discard_game: discard_build_game_fixture(users), discard_users: users}
    end

    test "only the chooser's own discard frame carries the discard pile", %{
      discard_game: game,
      discard_users: [chooser | others]
    } do
      assert {:ok, %{frames: frames, view: final}} = Games.replay(game, chooser.id)

      discard_frames = Enum.filter(frames, &is_list(&1.discard_pile))
      assert discard_frames != []

      for frame <- discard_frames do
        assert %{kind: :extra_turn, extra_turn_kind: :build_from_discard} = frame.phase
        assert frame.phase.extra_turn_player == to_string(chooser.id)
        assert frame.discard_pile != []
        assert frame.discard_pile == Enum.sort(Enum.uniq(frame.discard_pile))
        assert {:build_from_discard, card} = frame.my_choice
        assert card in frame.discard_pile
        assert frame.hand == []
      end

      for frame <- frames, frame.phase.kind != :extra_turn do
        assert frame.discard_pile == nil
      end

      assert Enum.all?(frames, &(is_integer(&1.discard_count) and &1.discard_count >= 0))
      assert hd(frames).discard_count == 0
      assert List.last(frames).discard_count == final.discard_count

      for other <- others do
        assert {:ok, %{frames: other_frames}} = Games.replay(game, other.id)
        assert length(other_frames) == length(frames)
        assert Enum.all?(other_frames, &is_nil(&1.discard_pile))
        assert Enum.map(other_frames, & &1.discard_count) == Enum.map(frames, & &1.discard_count)
      end
    end
  end
```

- [ ] **Step 2: Run to verify it fails**

Run: `cd helios && mix test test/helios/games/replay_test.exs`
Expected: FAIL — `KeyError` / `key :discard_pile not found` on the frame maps (the existing tests still pass).

- [ ] **Step 3: Implement**

In `helios/lib/helios/games.ex` replace `frame_snapshot/2`:

```elixir
  defp frame_snapshot(ref, me) do
    {:ok, view} = Core.view(ref, me)

    %{
      players: view.players,
      hand: Enum.map(view.hand, & &1.name),
      phase: view.phase,
      # Non-nil only in the viewer's own build-from-discard turn: the engine
      # fills it for that player alone, so the replay shows the discards
      # exactly to whoever saw them live.
      discard_pile: view.discard_pile,
      discard_count: view.discard_count,
      my_choice: nil,
      event_index: nil
    }
  end
```

and extend the `replay/2` `@doc` first paragraph:

```elixir
  @doc """
  Rebuilds a finished game from its log in a throwaway engine: the viewer's
  final view, the public history, and one frame per turn (public boards plus
  the viewer's own hand and final choice that turn, the discard-pile size and,
  in the viewer's own build-from-discard turn only, the buildable discards).
  `{:error, :not_finished}` if the log stops short of game over.
  """
```

- [ ] **Step 4: Run to verify it passes**

Run: `cd helios && mix test test/helios/games/replay_test.exs`
Expected: PASS. (`find_seed` runs in a throwaway engine; expect the new test to take a few seconds.)

- [ ] **Step 5: Precommit and commit**

```bash
cd helios && mix precommit
cd .. && git add helios/lib/helios/games.ex helios/test/support/games_fixtures.ex helios/test/helios/games/replay_test.exs
git commit -m "$(cat <<'EOF'
feat(games): replay frames keep the discard count and the chooser's discard pile

The pile is copied from the engine view, which fills it only for the player
whose build-from-discard turn it is, so replay secrecy equals live secrecy.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
EOF
)"
```

---

### Task 2: Discard-pile marker in the status bar

**Files:**
- Modify: `helios/lib/helios_web/components/game_components/top_bar.ex` (`top_bar/1` `#pass-row`, new `discard_pile/1`)
- Modify: `helios/lib/helios_web/live/game_live.ex` (`render/1`: replay top bar gets the frame's `discard_count`)
- Test: `helios/test/helios_web/components/game_components/top_bar_test.exs`, `helios/test/helios_web/live/game_live_test.exs`

**Interfaces:**
- Consumes: phase A's `#pass-row` / `#pass-direction` markup and `replay` attr; Task 1's `frame.discard_count`.
- Produces: `HeliosWeb.GameComponents.TopBar.discard_pile/1` — attr `count` (`:integer`, required); renders `#discard-pile` (`data-count`, stacked-card-back glyph, visible count, sr-only "Discard pile:") when `count > 0`, nothing otherwise. Task 4's `.TurnFx` uses `#discard-pile`'s rect as the observers' fly origin.
- `#pass-row` is rendered when `phase.kind != :game_over or view.discard_count > 0`; on game over it holds only `#discard-pile`.

- [ ] **Step 1: Write the failing tests**

In `helios/test/helios_web/components/game_components/top_bar_test.exs`, phase A's "game over replaces the turn information" test asserts `count(html, "#pass-row") == 0` with `SampleViews.view()`, whose `discard_count` is 2. Change that test's render line to a game over with an empty pile:

```elixir
    html = render_bar(%{view | phase: %{view.phase | kind: :game_over}, discard_count: 0})
```

and append:

```elixir
  test "the discard pile marker shows the count in the pass row and hides at zero" do
    html = render_bar(SampleViews.view())
    assert count(html, "#discard-pile") == 1
    assert count(html, "#pass-row > #discard-pile") == 1
    assert attrs(html, "#discard-pile", "data-count") == ["2"]
    assert text(html, "#discard-pile") == "Discard pile: 2"
    assert text(html, "#pass-row #pass-direction") == "Pass to Bob"

    empty = render_bar(%{SampleViews.view() | discard_count: 0})
    assert count(empty, "#discard-pile") == 0
    assert count(empty, "#pass-row > #pass-direction") == 1
  end

  test "on game over the pass row holds only the discard pile" do
    view = SampleViews.view()
    html = render_bar(%{view | phase: %{view.phase | kind: :game_over}, discard_count: 21})
    assert count(html, "#pass-row > #discard-pile") == 1
    assert count(html, "#pass-direction") == 0
    assert text(html, "#discard-pile") == "Discard pile: 21"
  end

  test "discard_pile/1 renders nothing for an empty pile" do
    assert render_component(&TopBar.discard_pile/1, count: 0) |> String.trim() == ""
    assert count(render_component(&TopBar.discard_pile/1, count: 5), "#discard-pile") == 1
  end
```

(If phase A's `render_bar` takes options as `render_bar(view, opts)`, keep its signature; these calls use the default options.)

In `helios/test/helios_web/live/game_live_test.exs` append (top level, after "mounting mid-game animates nothing"):

```elixir
  test "the status bar counts the discard pile once cards are discarded", %{
    game: game,
    players: players
  } do
    [{_a, ta} | _] = players
    ids = Enum.map(players, fn {user, _} -> to_string(user.id) end)
    {:ok, view, _html} = open(ta, game.id)
    refute has_element?(view, "#discard-pile")

    {view_fun, submit_fun} = Helios.GameDriver.games_funs(game.id)
    Helios.GameDriver.step(ids, %{}, view_fun, submit_fun)

    assert has_element?(view, "#pass-row #discard-pile[data-count='3']")
  end
```

and inside `describe "replay of a finished game"` add:

```elixir
    test "the status bar shows the frame's discard count", %{
      game: game,
      players: [{_a, ta} | _]
    } do
      {:ok, view, _html} = open(ta, game.id)
      assert has_element?(view, "#discard-pile")

      view |> element("#replay-first") |> render_click()
      refute has_element?(view, "#discard-pile")

      view |> element("#replay-next") |> render_click()
      assert has_element?(view, "#discard-pile[data-count='3']")
    end
```

(The default game is discard-only with 3 players: every turn adds 3 cards to the pile.)

- [ ] **Step 2: Run to verify they fail**

Run: `cd helios && mix test test/helios_web/components/game_components/top_bar_test.exs test/helios_web/live/game_live_test.exs`
Expected: FAIL — `undefined function discard_pile/1`; no `#discard-pile` in the bar.

- [ ] **Step 3: Implement the component and its placement**

In `helios/lib/helios_web/components/game_components/top_bar.ex`, replace phase A's `#pass-row` block:

```heex
        <div
          :if={@phase.kind != :game_over}
          id="pass-row"
          class="flex min-w-0 items-center justify-between gap-3 text-sm sm:justify-start sm:text-base"
        >
          <span id="pass-direction" class="flex min-w-0 items-center gap-1">
            <.icon name={GameFormat.direction_icon(@phase.direction)} class="size-5 shrink-0" />
            <span class="min-w-0 break-words">Pass to {@pass_to}</span>
          </span>
        </div>
```

with:

```heex
        <div
          :if={@phase.kind != :game_over or @view.discard_count > 0}
          id="pass-row"
          class="flex min-w-0 items-center justify-between gap-3 text-sm sm:justify-start sm:text-base"
        >
          <span :if={@phase.kind != :game_over} id="pass-direction" class="flex min-w-0 items-center gap-1">
            <.icon name={GameFormat.direction_icon(@phase.direction)} class="size-5 shrink-0" />
            <span class="min-w-0 break-words">Pass to {@pass_to}</span>
          </span>
          <.discard_pile count={@view.discard_count} />
        </div>
```

On phones `justify-between` puts the marker at the right of row 2 (and alone on game over); on `sm`+ it follows the pass direction inside the single desktop row.

Add the component after `top_bar/1` (before phase A's `waiting_menu/1`), and mention it in the `@moduledoc` ("…then `#pass-row` (the pass direction and the discard-pile marker)…"):

```elixir
  attr :count, :integer, required: true, doc: "cards in the discard pile (public)"

  @doc """
  Stacked card backs plus the discard-pile size. Rendered once, in `#pass-row`;
  it is also where observers see a card built from the discard pile take off
  (`.TurnFx` reads its rect).
  """
  def discard_pile(assigns) do
    ~H"""
    <span
      :if={@count > 0}
      id="discard-pile"
      data-count={@count}
      title={"#{@count} cards in the discard pile"}
      class="flex shrink-0 items-center gap-1.5 whitespace-nowrap text-sm font-semibold text-white/90"
    >
      <span class="relative inline-block h-5 w-4 shrink-0" aria-hidden="true">
        <span class="absolute inset-0 translate-x-1 -translate-y-0.5 rotate-6 rounded-[3px] bg-amber-700 ring-1 ring-black/40">
        </span>
        <span class="absolute inset-0 rounded-[3px] bg-amber-800 ring-1 ring-amber-200/70"></span>
      </span>
      <span class="sr-only">Discard pile:</span>
      <span>{@count}</span>
    </span>
    """
  end
```

- [ ] **Step 4: Replay passes the frame's count**

In `helios/lib/helios_web/live/game_live.ex` `render/1`, the `<.top_bar>` call (as left by phase A) becomes:

```heex
          <.top_bar
            view={
              if @replay,
                do: %{
                  @view
                  | phase: current_frame(@replay).phase,
                    submitted: [],
                    discard_count: current_frame(@replay).discard_count
                },
                else: @view
            }
            names={@names}
            connected={@connected}
            replay={@replay != nil}
          />
```

The finished-game fallback view (`assign_finished_view/3`) already has `discard_count: 0`, so a game whose boards are unavailable shows no marker.

- [ ] **Step 5: Run to verify they pass**

Run: `cd helios && mix test test/helios_web/components/game_components/top_bar_test.exs test/helios_web/live/game_live_test.exs test/helios_web/live/game_live_extra_turns_test.exs`
Expected: PASS. If one of phase A's tests asserts the full text of `#pass-row` (rather than `#pass-direction`), change it to assert `#pass-row #pass-direction`.

- [ ] **Step 6: Precommit and commit**

```bash
cd helios && mix precommit
cd .. && git add helios/lib/helios_web/components/game_components/top_bar.ex helios/lib/helios_web/live/game_live.ex helios/test/helios_web/components/game_components/top_bar_test.exs helios/test/helios_web/live/game_live_test.exs
git commit -m "$(cat <<'EOF'
feat(web): discard-pile marker in the status bar

Shown in the pass row whenever the pile is not empty, live and in replay
(replay shows the frame's count).

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
EOF
)"
```

---

### Task 3: The chooser's discard options in the replay dock

**Files:**
- Modify: `helios/lib/helios_web/components/game_components/extra_turn.ex` (new `replay_discard/1`; `discard_picker/1` gets the `.DiscardRects` hook and `data-fx-id`s; the hook script)
- Modify: `helios/lib/helios_web/live/game_live.ex` (`render/1` replay dock, `frame_discard/1`)
- Test: `helios/test/helios_web/components/game_components/extra_turn_test.exs`, `helios/test/helios_web/live/game_live_test.exs`

**Interfaces:**
- Consumes: Task 1's `frame.discard_pile` and `discard_build_game_fixture/1`; `GameAssets.slug/1`, `GameAssets.card_path/1`; `GameFormat.action_card/1` (`{:build_from_discard, card}` → `card`).
- Produces: `HeliosWeb.GameComponents.ExtraTurn.replay_discard/1` — attrs `cards` (`:list` of names, required) and `chosen` (`:string`, default `nil`); renders `section#replay-discard` (hook `.DiscardRects`) with one `div#replay-discard-<slug>` per card (`data-card`, `data-fx-id="discard-card-<slug>"`, `data-chosen="true"|"false"`), the chosen one ringed and labelled "Chosen".
- Produces: colocated hook `.DiscardRects` (in `extra_turn.ex`): on mount, `beforeUpdate`, and on `pointerdown`/`focusin` inside the element, writes `window.sevenWondersDiscardRects = {"discard-card-<slug>": {left, top, width, height, src}}` from every `[data-fx-id] img`. `#discard-picker` uses it too and its buttons gain `data-fx-id="discard-card-<slug>"` (ids `#discard-pick-<index>` unchanged). Task 4's `.TurnFx` reads the map.

- [ ] **Step 1: Write the failing tests**

Append to `helios/test/helios_web/components/game_components/extra_turn_test.exs`:

```elixir
  test "the discard picker snapshots its card rects for the fly animation" do
    view = extra("1", :build_from_discard, %{discard_pile: ["Altar", "Lumber Yard"], hand: []})
    html = render_picker(view)
    assert [hook] = attrs(html, "#discard-picker", "phx-hook")
    assert hook =~ "DiscardRects"

    assert attrs(html, "#discard-picker [data-fx-id]", "data-fx-id") ==
             ["discard-card-altar", "discard-card-lumberyard"]

    assert attrs(html, "#discard-pick-1", "data-fx-id") == ["discard-card-lumberyard"]
  end

  test "replay_discard shows the options read-only with the chosen card marked" do
    html =
      render_component(&ExtraTurn.replay_discard/1,
        cards: ["Altar", "Lumber Yard"],
        chosen: "Lumber Yard"
      )

    assert attrs(html, "#replay-discard [data-card]", "data-card") == ["Altar", "Lumber Yard"]
    assert attrs(html, "#replay-discard-lumberyard", "data-chosen") == ["true"]
    assert attrs(html, "#replay-discard-altar", "data-chosen") == ["false"]
    assert text(html, "#replay-discard-lumberyard") == "Chosen"
    assert attrs(html, "#replay-discard-altar", "data-fx-id") == ["discard-card-altar"]
    assert [hook] = attrs(html, "#replay-discard", "phx-hook")
    assert hook =~ "DiscardRects"
    assert count(html, "#replay-discard button, #replay-discard [phx-click]") == 0
  end
```

In `helios/test/helios_web/live/game_live_test.exs` add a new describe after `describe "replay of a finished game"`:

```elixir
  describe "replay of a Halikarnassós discard build" do
    setup do
      players = for _ <- 1..3, do: user_with_token_fixture()
      game = discard_build_game_fixture(Enum.map(players, &elem(&1, 0)))
      stop_game_servers()
      [{chooser, _} | _] = players
      {:ok, %{frames: frames}} = Games.replay(game, chooser.id)
      index = Enum.find_index(frames, &is_list(&1.discard_pile))
      %{discard_game: game, discard_players: players, frames: frames, discard_index: index}
    end

    test "the chooser sees the cards they could pick, with their choice marked", %{
      discard_game: game,
      discard_players: [{_chooser, tc}, {_other, to} | _],
      frames: frames,
      discard_index: index
    } do
      frame = Enum.at(frames, index)
      {:build_from_discard, card} = frame.my_choice
      slug = HeliosWeb.GameAssets.slug(card)

      {:ok, view, _html} = open(tc, game.id)
      refute has_element?(view, "#replay-discard")
      render_click(view, "replay_goto", %{"frame" => to_string(index)})
      assert has_element?(view, "#replay-position", "extra turn")

      for name <- frame.discard_pile do
        assert has_element?(view, "#replay-discard #replay-discard-#{HeliosWeb.GameAssets.slug(name)}")
      end

      assert has_element?(view, "#replay-discard-#{slug}[data-chosen='true']", "Chosen")
      refute has_element?(view, "#replay-hand")
      assert has_element?(view, "#replay-choice", "Build #{card} from the discard pile")

      {:ok, other, _html} = open(to, game.id)
      render_click(other, "replay_goto", %{"frame" => to_string(index)})
      assert has_element?(other, "#replay-position", "extra turn")
      refute has_element?(other, "#replay-discard")
      refute has_element?(other, "#replay-hand")
    end
  end
```

- [ ] **Step 2: Run to verify they fail**

Run: `cd helios && mix test test/helios_web/components/game_components/extra_turn_test.exs test/helios_web/live/game_live_test.exs`
Expected: FAIL — `undefined function replay_discard/1`; `#discard-picker` has no `phx-hook`; no `#replay-discard` in the LiveView.

- [ ] **Step 3: Picker hook, replay grid and hook script**

In `helios/lib/helios_web/components/game_components/extra_turn.ex`:

Update the `@moduledoc`: `"Extra-turn notices for the dock (Babylon B last card, waiting for others), the Halikarnassós discard picker and its read-only replay counterpart."`

In `discard_picker/1`, give the `<section id="discard-picker" …>` the hook (add the attribute after `id`):

```heex
        phx-hook=".DiscardRects"
```

and each pick button a fly id (after `data-card={card}`):

```heex
            data-fx-id={"discard-card-#{GameAssets.slug(card)}"}
```

Then, at the end of `discard_picker/1`'s template, **after** the closing `<% end %>` (top level, like `.HandRects` in `hand.ex`), add the hook:

```heex
    <script :type={Phoenix.LiveView.ColocatedHook} name=".DiscardRects">
      // The picker / replay grid is gone by the time the turn resolves, so the
      // card rects are snapshotted here and read by .TurnFx (fx.ex). Pointer
      // and focus snapshots catch the final position after any sheet motion.
      export default {
        mounted() {
          this.snapshot = this.snapshot.bind(this)
          this.snapshot()
          this.el.addEventListener("pointerdown", this.snapshot, true)
          this.el.addEventListener("focusin", this.snapshot, true)
        },
        beforeUpdate() { this.snapshot() },
        snapshot() {
          const rects = {}
          this.el.querySelectorAll("[data-fx-id] img").forEach((img) => {
            const r = img.getBoundingClientRect()
            rects[img.closest("[data-fx-id]").dataset.fxId] =
              {left: r.left, top: r.top, width: r.width, height: r.height, src: img.currentSrc || img.src}
          })
          window.sevenWondersDiscardRects = rects
        },
      }
    </script>
```

Add the replay grid after `discard_restore/1` (styled like `Hand.replay_hand/1`):

```elixir
  attr :cards, :list, required: true, doc: "buildable discard names the viewer saw that turn"
  attr :chosen, :string, default: nil

  @doc "Read-only replay of the viewer's own build-from-discard choice."
  def replay_discard(assigns) do
    ~H"""
    <section
      id="replay-discard"
      phx-hook=".DiscardRects"
      aria-labelledby="replay-discard-title"
    >
      <h2
        id="replay-discard-title"
        class="px-1 text-xs font-semibold uppercase tracking-wide text-zinc-700"
      >
        Discard pile — you could build one for free
      </h2>
      <div class="-mx-2 flex justify-center-safe gap-2 overflow-x-auto px-2 pt-2 pb-1">
        <div
          :for={card <- @cards}
          id={"replay-discard-#{GameAssets.slug(card)}"}
          data-card={card}
          data-fx-id={"discard-card-#{GameAssets.slug(card)}"}
          data-chosen={to_string(card == @chosen)}
          class={["relative shrink-0 rounded-lg", card == @chosen && "ring-4 ring-amber-500"]}
        >
          <img
            src={GameAssets.card_path(card)}
            alt={card}
            class="h-[98px] w-16 rounded-lg object-cover sm:h-[134px] sm:w-22 short:h-[86px] short:w-14"
          />
          <span
            :if={card == @chosen}
            class="absolute left-1 top-1 rounded bg-amber-500 px-1.5 py-0.5 text-xs font-bold text-white"
          >
            Chosen
          </span>
        </div>
      </div>
    </section>
    """
  end
```

- [ ] **Step 4: Render it in the replay dock**

In `helios/lib/helios_web/live/game_live.ex` `render/1`, replace the replay hand block:

```heex
              <div
                :if={@replay && current_frame(@replay).hand != []}
                class="sticky bottom-0 z-30 overflow-x-clip rounded-t-xl bg-antique/95 p-2 shadow"
              >
                <.replay_hand
                  hand={current_frame(@replay).hand}
                  chosen={GameFormat.action_card(current_frame(@replay).my_choice)}
                />
              </div>
```

with:

```heex
              <div
                :if={@replay && (current_frame(@replay).hand != [] or frame_discard(@replay) != [])}
                class="sticky bottom-0 z-30 overflow-x-clip rounded-t-xl bg-antique/95 p-2 shadow"
              >
                <.replay_discard
                  :if={frame_discard(@replay) != []}
                  cards={frame_discard(@replay)}
                  chosen={GameFormat.action_card(current_frame(@replay).my_choice)}
                />
                <.replay_hand
                  :if={current_frame(@replay).hand != []}
                  hand={current_frame(@replay).hand}
                  chosen={GameFormat.action_card(current_frame(@replay).my_choice)}
                />
              </div>
```

and add next to `current_frame/1`:

```elixir
  # Only the viewer's own build-from-discard frame has a pile (Games.replay/2).
  defp frame_discard(replay), do: current_frame(replay).discard_pile || []
```

(`ExtraTurn` is already imported, so `<.replay_discard>` resolves.)

- [ ] **Step 5: Run to verify they pass**

Run: `cd helios && mix test test/helios_web/components/game_components/extra_turn_test.exs test/helios_web/live/game_live_test.exs test/helios_web/live/game_live_extra_turns_test.exs`
Expected: PASS.

- [ ] **Step 6: Precommit and commit**

```bash
cd helios && mix precommit
cd .. && git add helios/lib/helios_web/components/game_components/extra_turn.ex helios/lib/helios_web/live/game_live.ex helios/test/helios_web/components/game_components/extra_turn_test.exs helios/test/helios_web/live/game_live_test.exs
git commit -m "$(cat <<'EOF'
feat(web): replay shows the chooser the discard cards they could build

Read-only #replay-discard grid with the chosen card marked; the live picker
and the grid snapshot their card rects for the fly animation.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
EOF
)"
```

---

### Task 4: A card built from the discard pile flies onto the board

**Files:**
- Modify: `helios/lib/helios_web/turn_fx.ex` (`payload/2`, `play/2`)
- Modify: `helios/lib/helios_web/components/game_components/fx.ex` (`.TurnFx` `play`, new `origin`, `arrive` delay)
- Test: `helios/test/helios_web/turn_fx_test.exs`, `helios/test/helios_web/live/game_live_extra_turns_test.exs`, `helios/test/helios_web/live/game_live_test.exs`

**Interfaces:**
- Consumes: `#discard-pile` (Task 2), `window.sevenWondersDiscardRects` keyed `discard-card-<slug>` (Task 3), Task 3's describe setup.
- Produces: `TurnFx.payload/2` plays for `{:built_from_discard, card}` are `%{player, kind: "built", target: "built-<player>-<slug>", from: "discard-card-<slug>" | "discard-pile"}` — the first when `player == ctx.me`, the second otherwise. Other plays are unchanged (no `from` key).
- `.TurnFx`: a play with `from` flies a ghost of the card from `sevenWondersDiscardRects[from]`, else from `#discard-pile`'s rect (ghost image = the target tile's `<img>`), to the target tile, which then does `arrive`; the used rect is deleted. No origin or no visible target → `arrive` only (today's behaviour).

- [ ] **Step 1: Write the failing tests**

Append to `helios/test/helios_web/turn_fx_test.exs`:

```elixir
  test "a card built from the discard pile flies from the picker for the builder, from the pile for others" do
    turn =
      {:turn,
       %{
         age: 1,
         turn: 3,
         kind: :build_from_discard,
         plays: [
           %{
             player: "1",
             action: {:built_from_discard, "Lumber Yard"},
             coins_to_west: 0,
             coins_to_east: 0
           }
         ],
         coins: []
       }}

    assert %{plays: [builder]} = TurnFx.payload(turn, %{me: "1", my_card: "Lumber Yard", pass: nil})

    assert builder == %{
             player: "1",
             kind: "built",
             target: "built-1-lumberyard",
             from: "discard-card-lumberyard"
           }

    assert %{plays: [observer]} = TurnFx.payload(turn, %{me: "2", my_card: nil, pass: nil})
    assert observer.from == "discard-pile"
    assert observer.target == "built-1-lumberyard"
  end
```

(The existing "plays point at the DOM ids they animate to" test compares whole maps, so it already pins that ordinary builds, stages and discards carry no `from`.)

In `helios/test/helios_web/live/game_live_extra_turns_test.exs`, in "Halikarnassós builds from the discard pile while the others wait": after `assert has_element?(view_a, "#discard-picker")` (the first one) add

```elixir
    assert has_element?(view_a, "#discard-picker[phx-hook] [data-fx-id]")
    assert has_element?(view_b, "#discard-pile")
```

and after `view_a |> element("#discard-pick-#{index}") |> render_click()` add

```elixir
    slug = HeliosWeb.GameAssets.slug(card)
    from = "discard-card-#{slug}"
    target = "built-#{a.id}-#{slug}"
    assert_push_event(view_a, "turn_resolved", %{plays: [%{from: ^from, target: ^target}]})

    assert_push_event(
      view_b,
      "turn_resolved",
      %{plays: [%{from: "discard-pile", target: ^target}]},
      1_000
    )
```

In `helios/test/helios_web/live/game_live_test.exs`, inside `describe "replay of a Halikarnassós discard build"` add:

```elixir
    test "stepping past the discard build flies the card from the right origin", %{
      discard_game: game,
      discard_players: [{_chooser, tc}, {_other, to} | _],
      frames: frames,
      discard_index: index
    } do
      {:build_from_discard, card} = Enum.at(frames, index).my_choice
      from = "discard-card-#{HeliosWeb.GameAssets.slug(card)}"

      {:ok, view, _html} = open(tc, game.id)
      render_click(view, "replay_goto", %{"frame" => to_string(index)})
      view |> element("#replay-next") |> render_click()
      assert_push_event(view, "turn_resolved", %{plays: [%{from: ^from}]})

      {:ok, other, _html} = open(to, game.id)
      render_click(other, "replay_goto", %{"frame" => to_string(index)})
      other |> element("#replay-next") |> render_click()
      assert_push_event(other, "turn_resolved", %{plays: [%{from: "discard-pile"}]})
    end
```

- [ ] **Step 2: Run to verify they fail**

Run: `cd helios && mix test test/helios_web/turn_fx_test.exs test/helios_web/live/game_live_extra_turns_test.exs test/helios_web/live/game_live_test.exs`
Expected: FAIL — plays have no `from` key.

- [ ] **Step 3: Payload**

Replace the body of `helios/lib/helios_web/turn_fx.ex` below `alias HeliosWeb.GameAssets`:

```elixir
  def payload({:turn, turn}, %{me: me, my_card: my_card, pass: pass}) do
    %{
      plays: Enum.map(turn.plays, &play(&1, me)),
      coins: Enum.map(turn.coins, &%{player: &1.player, delta: &1.delta}),
      me: me,
      my_card: my_card,
      pass: pass && Atom.to_string(pass)
    }
  end

  # Halikarnassós: the builder saw the card in the picker (or the replay grid),
  # everyone else sees it leave the discard-pile marker in the status bar.
  defp play(%{player: player, action: {:built_from_discard, card}}, me) do
    slug = GameAssets.slug(card)
    from = if player == me, do: "discard-card-#{slug}", else: "discard-pile"
    %{player: player, kind: "built", target: "built-#{player}-#{slug}", from: from}
  end

  defp play(%{player: player, action: {kind, card}}, _me) when kind in [:built, :built_free],
    do: %{player: player, kind: "built", target: "built-#{player}-#{GameAssets.slug(card)}"}

  defp play(%{player: player, action: {:wonder_stage, n}}, _me),
    do: %{player: player, kind: "wonder_stage", target: "stage-#{player}-#{n}"}

  defp play(%{player: player, action: :discarded}, _me),
    do: %{player: player, kind: "discarded", target: nil}
```

- [ ] **Step 4: `.TurnFx` flies from the discard origin**

In `helios/lib/helios_web/components/game_components/fx.ex`, update the `@moduledoc` second sentence to: "The `.TurnFx` hook flies the player's card from the hand (or a card built from the discard pile from the picker / discard-pile marker) to its tile or stage slot, fades in other players' new tiles, floats coin changes and slides the next hand in."

In the hook, replace `play(...)` and `arrive(...)` with:

```js
        play({plays, coins, me, my_card, pass}) {
          if (this.reduced()) return
          const rects = window.sevenWondersHandRects || {}
          const mine = my_card && rects[my_card]
          for (const play of plays) {
            const target = play.target && document.getElementById(play.target)
            if (play.from) {
              const from = this.origin(play.from, target)
              if (from && this.visible(target)) {
                this.fly(from, target, true)
                this.arrive(target, 550)
              } else if (target) {
                this.arrive(target)
              }
            } else if (play.player === me && mine) {
              const to = target || document.querySelector(`section[data-player="${me}"] [data-stat="coins"]`)
              this.fly(mine, to, !target)
            } else if (target) {
              this.arrive(target)
            }
          }
          for (const {player, delta} of coins) this.float(player, delta)
          if (pass) this.slideHand(pass)
        },
        // Where a card built from the discard pile takes off: the rect the
        // picker / replay grid snapshotted (.DiscardRects), else the status-bar
        // marker with the target tile's image. null → the caller only arrives.
        origin(id, target) {
          const saved = window.sevenWondersDiscardRects || {}
          if (saved[id]) {
            const rect = saved[id]
            delete saved[id]
            return rect
          }
          const pile = this.visible(document.getElementById("discard-pile"))
          const img = target && target.querySelector("img")
          if (!pile || !img) return null
          return {left: pile.left, top: pile.top, width: pile.width, height: pile.height, src: img.currentSrc || img.src}
        },
```

```js
        arrive(target, delay = 250) {
          if (!this.visible(target)) return
          target.animate([{transform: "scale(.6)", opacity: 0}, {transform: "scale(1)", opacity: 1}],
                         {duration: 500, delay, easing: "ease-out", fill: "backwards"})
        },
```

The ghost (700 ms, fading out as it lands) and the tile's `arrive` (550 ms delay + 500 ms) end by 1.05 s, inside the 1.2 s budget. Under reduced motion `play` returns before any of this.

- [ ] **Step 5: Run to verify they pass**

Run: `cd helios && mix test test/helios_web/turn_fx_test.exs test/helios_web/live/game_live_extra_turns_test.exs test/helios_web/live/game_live_test.exs`
Expected: PASS.

- [ ] **Step 6: Precommit and commit**

```bash
cd helios && mix precommit
cd .. && git add helios/lib/helios_web/turn_fx.ex helios/lib/helios_web/components/game_components/fx.ex helios/test/helios_web/turn_fx_test.exs helios/test/helios_web/live/game_live_extra_turns_test.exs helios/test/helios_web/live/game_live_test.exs
git commit -m "$(cat <<'EOF'
feat(web): a card built from the discard pile flies onto the board

The builder's card takes off from the picker or replay grid, everyone else's
from the discard-pile marker; without an origin the tile only arrives.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
EOF
)"
```

---

### Task 5: Replay position in the URL

**Files:**
- Modify: `helios/lib/helios_web/live/game_live.ex` (`handle_params/3`, `patch_frame/2`, `frame_param/2`, `handle_event("replay_" <> …)`, `step_forward/1`)
- Test: `helios/test/helios_web/live/game_live_test.exs`

**Interfaces:**
- Consumes: route `live "/game/:game_id", GameLive` (no change); history drawer links `phx-click="replay_goto"` / `phx-value-frame` (no change); `.ReplayAutoplay` hook pushing `replay_next` every 2.5 s (no change — its ticks go through `step_forward/1`, which now patches).
- Produces: `GameLive.handle_params/3`. `?frame=N` selects frame N clamped to `0..last`; a missing or non-integer `frame` → last frame. Every frame change patches with `replace: true` to `/game/<id>?frame=<i>`, or `/game/<id>` for the last frame.
- Design: `first/prev/last/goto` only pause and patch; `handle_params` performs the `goto` (which also clears `age_summary`). `next` (button and autoplay) runs `step_forward/1` itself — it must push the turn animation, open the age summary and keep autoplay running — and then patches; `handle_params` sees the index already shown and does nothing. `refresh/1` is unchanged (it re-applies the current frame).

- [ ] **Step 1: Write the failing tests**

In `helios/test/helios_web/live/game_live_test.exs` inside `describe "replay of a finished game"` add:

```elixir
    test "?frame=N opens that frame; garbage and out-of-range values fall back or clamp", %{
      game: game,
      players: [{_a, ta} | _]
    } do
      {:ok, view, _html} = live(log_in_conn(build_conn(), ta), ~p"/game/#{game.id}?frame=3")
      assert has_element?(view, "#replay-position", "Age I · Turn 4")
      refute has_element?(view, "#scoreboard")

      {:ok, view, _html} = live(log_in_conn(build_conn(), ta), ~p"/game/#{game.id}?frame=999")
      assert has_element?(view, "#replay-position", "Final")

      {:ok, view, _html} = live(log_in_conn(build_conn(), ta), ~p"/game/#{game.id}?frame=-4")
      assert has_element?(view, "#replay-position", "Age I · Turn 1")

      for garbage <- ["abc", "2x", "", "1.5"] do
        {:ok, view, _html} =
          live(log_in_conn(build_conn(), ta), "/game/#{game.id}?frame=#{garbage}")

        assert has_element?(view, "#replay-position", "Final")
      end
    end

    test "every frame change patches the URL; the final frame drops the param", %{
      game: game,
      players: [{_a, ta} | _]
    } do
      {:ok, view, _html} = open(ta, game.id)

      view |> element("#replay-first") |> render_click()
      assert_patch(view, ~p"/game/#{game.id}?frame=0")

      view |> element("#replay-next") |> render_click()
      assert_patch(view, ~p"/game/#{game.id}?frame=1")
      assert_push_event(view, "turn_resolved", %{plays: [_, _, _]})

      view |> element("#replay-prev") |> render_click()
      assert_patch(view, ~p"/game/#{game.id}?frame=0")

      render_click(view, "replay_goto", %{"frame" => "6"})
      assert_patch(view, ~p"/game/#{game.id}?frame=6")
      assert has_element?(view, "#replay-position", "Age II · Turn 1")

      view |> element("#replay-last") |> render_click()
      assert_patch(view, ~p"/game/#{game.id}")
      assert has_element?(view, "#replay-position", "Final")
    end

    test "autoplay ticks patch the URL and keep playing", %{
      game: game,
      players: [{_a, ta} | _]
    } do
      {:ok, view, _html} = live(log_in_conn(build_conn(), ta), ~p"/game/#{game.id}?frame=1")
      view |> element("#replay-play") |> render_click()
      assert has_element?(view, "#replay-bar[data-playing='true']")

      render_click(view, "replay_next", %{})
      assert_patch(view, ~p"/game/#{game.id}?frame=2")
      assert has_element?(view, "#replay-position", "Age I · Turn 3")
      assert has_element?(view, "#replay-bar[data-playing='true']")
    end

    test "the end-of-age summary survives the patch of the step that reveals it", %{
      game: game,
      players: [{_a, ta} | _]
    } do
      {:ok, view, _html} = live(log_in_conn(build_conn(), ta), ~p"/game/#{game.id}?frame=5")
      view |> element("#replay-next") |> render_click()
      assert_patch(view, ~p"/game/#{game.id}?frame=6")
      assert has_element?(view, "#age-summary")
    end
```

The existing tests in this describe keep passing unchanged: out-of-range/garbage `replay_goto` events are still ignored (no patch), and `render_click` returns after the patch has gone through `handle_params/3` (it runs in the same LiveView process).

- [ ] **Step 2: Run to verify they fail**

Run: `cd helios && mix test test/helios_web/live/game_live_test.exs`
Expected: FAIL — `?frame=3` still opens on "Final"; `assert_patch` times out (no patch is pushed).

- [ ] **Step 3: Implement**

In `helios/lib/helios_web/live/game_live.ex`:

Add after `mount/3`:

```elixir
  # The replay frame lives in the URL (?frame=N) so a reload returns to it.
  # Active games and games without boards have no replay: params are ignored.
  @impl true
  def handle_params(params, _uri, %{assigns: %{replay: %{} = replay}} = socket) do
    index = frame_param(params["frame"], length(replay.frames) - 1)
    {:noreply, if(index == replay.index, do: socket, else: goto(socket, index))}
  end

  def handle_params(_params, _uri, socket), do: {:noreply, socket}
```

Replace `goto_param/3` with these helpers (keep `goto/2` and `step_forward/1` where they are):

```elixir
  # Missing or non-integer → the final frame; integers are clamped to 0..last.
  defp frame_param(value, last) when is_binary(value) do
    case Integer.parse(value) do
      {index, ""} -> index |> max(0) |> min(last)
      _ -> last
    end
  end

  defp frame_param(_value, last), do: last

  # The one place a replay frame change is announced: handle_params/3 then
  # shows the frame. The final frame keeps the plain game URL.
  defp patch_frame(socket, index) do
    %{game: game, replay: replay} = socket.assigns

    to =
      if index == length(replay.frames) - 1,
        do: ~p"/game/#{game.id}",
        else: ~p"/game/#{game.id}?frame=#{index}"

    push_patch(socket, to: to, replace: true)
  end

  defp pause(socket), do: update(socket, :replay, &%{&1 | playing: false})

  # History-drawer links: only real frame numbers move; anything else is ignored.
  defp goto_param(socket, frame, last) do
    case Integer.parse(to_string(frame)) do
      {index, ""} when index >= 0 and index <= last -> socket |> pause() |> patch_frame(index)
      _ -> socket
    end
  end
```

At the end of `step_forward/1`, patch after updating the replay — the pipeline's last lines become:

```elixir
    socket
    |> update(:replay, fn replay ->
      %{
        replay
        | index: replay.index + 1,
          playing: replay.playing and replay.index + 1 < last and not revealed?
      }
    end)
    |> assign_frame()
    |> then(&patch_frame(&1, &1.assigns.replay.index))
```

In `handle_event("replay_" <> action, …)` the frame-changing clauses become:

```elixir
        {"first", _} ->
          socket |> pause() |> patch_frame(0)

        {"prev", _} ->
          socket |> pause() |> patch_frame(max(replay.index - 1, 0))

        {"last", _} ->
          socket |> pause() |> patch_frame(last)
```

(`{"next", _}`, `{"toggle", _}` and `{"goto", %{"frame" => frame}}` → `goto_param(socket, frame, last)` keep their current shape.) `goto/2` is now called only from `handle_params/3`.

On mount, `enter_replay/2` still starts on the last frame; LiveView then calls `handle_params/3` (dead and connected render), which moves to `?frame=N` if given. When an active game finishes in front of the player, `refresh/1` enters the replay on the last frame and the URL already has no param.

- [ ] **Step 4: Run to verify they pass**

Run: `cd helios && mix test test/helios_web/live/game_live_test.exs test/helios_web/live/game_live_extra_turns_test.exs test/helios_web/live/past_games_live_test.exs`
Expected: PASS.

- [ ] **Step 5: Precommit and commit**

```bash
cd helios && mix precommit
cd .. && git add helios/lib/helios_web/live/game_live.ex helios/test/helios_web/live/game_live_test.exs
git commit -m "$(cat <<'EOF'
feat(web): replay position lives in the URL

?frame=N opens that frame (clamped; garbage means the final table); every
frame change patches the URL with replace, and the final frame drops it.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
EOF
)"
```

---

### Task 6: Phase gate

**Files:** none new.

- [ ] **Step 1: Full checks**

```bash
cd helios && mix precommit
cd ../e2e && npm ci && npx playwright install chromium && npx playwright test
```

Expected: all green. Phase B changes no ids e2e uses (`game.spec.ts` replay steps click `#replay-first` / `#replay-next`, which now also patch the URL; `responsive.spec.ts` measures `#pass-row`, which gains the one-line marker without growing taller). If `responsive.spec.ts` reports `#pass-row` taller than phase A's budget, take the measured value from the run and stop and ask before changing the budget.

- [ ] **Step 2: Manual check in a browser (reduced motion off)**

With `cd helios && mix phx.server` and three browser sessions, start games until one seat has Halikarnassós (wonders are dealt at random), then play until its discard turn:
- the status bar shows the discard-pile marker (phone: right of "Pass to …"; desktop: after the pass direction);
- picking a card in the discard picker flies it to the builder's board; the other players see it take off from the marker;
- after the game, opening the replay at that turn shows the chooser the discard grid with the chosen card marked, and ▶ flies the card from the grid; another player sees no grid and the card flies from the marker;
- stepping updates `?frame=N` in the address bar; reloading returns to the same frame (paused); "Final table" clears the param; the browser Back button leaves the game page instead of stepping through frames.

No commit (nothing changed). If a manual check fails, fix it in the owning task's files with a test and commit with explicit paths.

---

## Spec coverage (self-check)

| Spec item | Task |
|---|---|
| C: `#discard-pile` — stacked-card-back glyph + `discard_count`, shown when > 0, live and replay, public | 2 |
| C: marker at the right of phone row 2 (`#pass-row`); on game over the row holds only the marker or is omitted; `sm`+ row also shows it (README: `TopBar.discard_pile/1`, attr `count`) | 2 |
| C: `frame_snapshot/2` keeps `discard_pile` (non-nil only in the chooser's own frame) and `discard_count` | 1 |
| C: `.replay_discard` in `extra_turn.ex`, `#replay-discard`, cards `#replay-discard-<slug>`, chosen card from `{:build_from_discard, card}` highlighted, styled like `replay_hand` | 3 |
| C: `TurnFx.payload` `from`: `discard-card-<slug>` for the actor, `discard-pile` for others | 4 |
| C: discard rects hook (`.DiscardRects`) on the live picker and `#replay-discard`, snapshot on mount/`beforeUpdate` into `window.sevenWondersDiscardRects` | 3 |
| C: `.TurnFx` flies from the saved rect, falls back to `#discard-pile`'s rect, then the tile arrives; no origin → arrive only; reduced motion skipped | 4 |
| D: `handle_params/3`, `?frame=N` clamped to `0..last`, missing/invalid → last | 5 |
| D: first/prev/next/last/goto, autoplay ticks and history-drawer links all patch with `replace: true`; final frame drops the param | 5 |
| D: autoplay and `show_scores` not persisted; `refresh/1` keeps the frame | 5 (unchanged code paths) |
| Testing: `replay_test.exs` chooser frame = buildable names, every other player's frames nil, `discard_count` present | 1 |
| Testing: `game_live_test.exs` `#replay-discard` for the chooser with the chosen card, absent for others; `?frame=N`; clamp/fallback; stepping patches | 3, 5 |
| Testing: `turn_fx_test.exs` `from` per viewer | 4 |
| Testing: `top_bar_test.exs` `#discard-pile` count, absent at 0 | 2 |
