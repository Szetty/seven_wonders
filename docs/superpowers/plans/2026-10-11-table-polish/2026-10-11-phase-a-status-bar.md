# Table Polish — Phase A: Phone Status Bar and Military Icon Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** On phones the status bar gets a second row that shows the full "Pass to ‹name›", the "Waiting for N" chip becomes a button that lists the names, replay frames stop claiming anybody is waiting, and the military icon becomes the card symbol (round red shield with crossed swords).

**Architecture:** Presentation-only changes in `HeliosWeb.GameComponents.TopBar` and `HeliosWeb.GameComponents.Icons`, plus one attr passed from `HeliosWeb.GameLive`. No engine, NIF, context or migration change; `ENGINE_VERSION` untouched. The waiting popover is pure `Phoenix.LiveView.JS` (toggle, click-away, Escape), no server state, following the stage popover in `stage_strip.ex`.

**Tech Stack:** Phoenix 1.8, LiveView 1.1, Tailwind CSS 4.1, ExUnit + LazyHTML (`HeliosWeb.HTMLHelpers`), Playwright.

**Spec:** `docs/superpowers/specs/2026-10-11-table-polish/2026-10-11-table-polish-design.md` (sections "A — Phone status bar" and "B — Military icon"). Cross-phase interfaces: `docs/superpowers/plans/2026-10-11-table-polish/README.md` ("A → B").

## Global Constraints

- Read `helios/AGENTS.md` before touching `helios/`; it takes precedence there (HEEx rules, `<.icon>`, no inline `<script>`, `Layouts.app` first in every LiveView).
- All game rules stay in `core`. `GameFormat.waiting_for/2` is **not** changed; the replay fix lives in `top_bar/1` (spec A, "Replay fix").
- DOM ids that tests and e2e use stay: `#top-bar`, `#age-label`, `#history-button`, `#turn-label`, `#pass-direction`, `#waiting-count`, `#waiting-for`, `#connections`, `#connection-<id>`. New ids: `#pass-row`, `#waiting-menu` (wrapper), `#waiting-list`. New attribute: `#top-bar[data-replay]`.
- Interface for phase B (README "A → B"): `top_bar/1` attr `replay` (`:boolean`, default `false`; when true no waiting UI is rendered) and the container `#pass-row` holding `#pass-direction`. On `sm` and up `#pass-row` sits inside the single desktop row, so phase B appends `#discard-pile` to `#pass-row` once and it shows on both layouts. Do **not** add `#discard-pile` in this phase.
- Touch targets ≥ 44×44 CSS px on touch viewports (`pointer-coarse:min-h-11` on the new button).
- No page may scroll horizontally at ≥ 360px wide. The phone top bar (< 640px) must stay ≤ 120px tall; row 1 stays single-line.
- `military_icon/1` keeps its name, attrs (`class` default `"size-5"`, global `rest`), `data-icon="military"` and `aria-hidden="true"`; only the SVG body changes.
- Stage explicit paths only (never `git add -A`/`.`); never stage `helios/config/config.exs` or `helios/config/dev.exs`.
- Run `cd helios && mix precommit` before each commit that touches `helios/`.

## Review Focus

- A 24-character name without spaces (the e2e maximum) in "Pass to ‹name›" is shown in full on a 360px phone: no `truncate`/`max-w-24`, the "Pass to" prefix is not `sr-only`, no horizontal scroll, top bar ≤ 120px (Task 2 component test, Task 4 e2e).
- `#waiting-list` is hidden on page load, lives only below `sm` (`#waiting-for` stays the desktop text), and the button is ≥ 44px tall on touch (Task 2 component test, Task 4 e2e tap target).
- Tapping the waiting button opens the list; Escape and a tap outside close it; an open list survives a LiveView patch and drops the player who just chose (Task 4 e2e).
- An extra-turn frame in replay shows no waiting UI, while a live extra turn still shows "Waiting for 1"; the live table renders `data-replay="false"` and the replay `data-replay="true"` (Task 3 component and LiveView tests).
- The new military icon reads at `size-3.5`: a round shield (two circles, the inner one a single dashed stroke), exactly two swords, and no stroke thinner than 1.7 viewBox units (≈1px at 14px) (Task 1 test).

---

### Task 1: Military icon as the card symbol

**Files:**
- Modify: `helios/lib/helios_web/components/game_components/icons.ex` (`military_icon/1`)
- Test: `helios/test/helios_web/components/game_components/icons_test.exs`

**Interfaces:**
- Consumes/keeps: `HeliosWeb.GameComponents.Icons.military_icon/1` — attrs `class` (default `"size-5"`), global `rest`; `<svg data-icon="military" aria-hidden="true">`. Callers (`player_board.ex` at `size-3.5`, `player_panels.ex` at `size-5`) are not touched.
- Produces: SVG parts marked `data-part="rim"`, `data-part="border"`, `data-part="sword"` (each a `<g>` with a `data-part="blade"` and a `data-part="hilt"` path) so tests assert structure, not coordinates.

- [ ] **Step 1: Write the failing test**

Append to `helios/test/helios_web/components/game_components/icons_test.exs` (inside the module):

```elixir
  test "the military icon is the card symbol: round red shield, key border, crossed swords" do
    html = render_component(&Icons.military_icon/1, class: "size-3.5")

    assert count(html, "svg[data-icon='military'] circle") == 2
    assert attrs(html, "circle[data-part='rim']", "fill") == ["#b91c1c"]
    assert attrs(html, "circle[data-part='rim']", "stroke") == ["#1c1917"]
    assert attrs(html, "circle[data-part='border']", "fill") == ["none"]
    assert [dashes] = attrs(html, "circle[data-part='border']", "stroke-dasharray")
    assert dashes != ""

    assert count(html, "g[data-part='sword']") == 2
    assert count(html, "g[data-part='sword'] path[data-part='blade']") == 2
    assert count(html, "g[data-part='sword'] path[data-part='hilt']") == 2
    assert attrs(html, "path[data-part='blade']", "stroke") == ["#e7e5e4", "#e7e5e4"]
  end

  test "no stroke of the military icon is thinner than 1px at size-3.5" do
    html = render_component(&Icons.military_icon/1, class: "size-3.5")
    widths = attrs(html, "svg [stroke-width]", "stroke-width")

    assert widths != []
    # 24-unit viewBox drawn at 14px: 1.7 units ≈ 1px.
    assert Enum.all?(widths, fn width -> String.to_float(width) >= 1.7 end)
  end
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cd helios && mix test test/helios_web/components/game_components/icons_test.exs`
Expected: the two new tests FAIL (today's icon is a heater-shield `path` with no circles and no `data-part`; `stroke-width="1.6"` on the cross). The existing "decorative inline svg" test still passes.

- [ ] **Step 3: Implement**

Replace `military_icon/1` in `helios/lib/helios_web/components/game_components/icons.ex` (keep the `attr` lines above it unchanged):

```elixir
  @doc """
  The military symbol printed on military cards: a round red shield with a dark
  rim, a light Greek-key border (one dashed ring) and two crossed swords.
  Drawn on a 24-unit grid with no stroke under 1.7 units so it reads at `size-3.5`.
  """
  def military_icon(assigns) do
    ~H"""
    <svg
      data-icon="military"
      viewBox="0 0 24 24"
      class={@class}
      aria-hidden="true"
      {@rest}
    >
      <circle data-part="rim" cx="12" cy="12" r="10.5" fill="#b91c1c" stroke="#1c1917" stroke-width="2" />
      <circle
        data-part="border"
        cx="12"
        cy="12"
        r="7.75"
        fill="none"
        stroke="#fef3c7"
        stroke-width="1.75"
        stroke-dasharray="1.9 1.4"
      />
      <g data-part="sword" stroke-linecap="round">
        <path data-part="blade" d="M5.5 5.5 L15 15" stroke="#e7e5e4" stroke-width="2.25" />
        <path data-part="hilt" d="M13 17 L17 13 M15.25 15.25 L17.75 17.75" stroke="#44403c" stroke-width="2.25" />
      </g>
      <g data-part="sword" stroke-linecap="round">
        <path data-part="blade" d="M18.5 5.5 L9 15" stroke="#e7e5e4" stroke-width="2.25" />
        <path data-part="hilt" d="M7 13 L11 17 M8.75 15.25 L6.25 17.75" stroke="#44403c" stroke-width="2.25" />
      </g>
    </svg>
    """
  end
```

Geometry notes for the implementer (do not change without re-checking): the blades cross at the centre (12,12); tips and pommels stay within radius ≈ 9.2 so the round caps don't run into the dark rim (inner edge at radius 9.5); the dashed ring is drawn before the swords so the swords sit on top, as on the card.

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cd helios && mix test test/helios_web/components/game_components/icons_test.exs test/helios_web/components/game_components/player_panels_test.exs test/helios_web/components/game_components/player_board_test.exs`
Expected: PASS.

- [ ] **Step 5: Look at it**

Run `cd helios && mix phx.server`, open a game and check the military stat on your board (`size-3.5`) and in the neighbour panel summaries (`size-5`): a red disc with a dark edge, a light dotted ring and a light X of blades with dark hilts at the lower corners. If the dashed ring blurs into a solid ring at `size-3.5`, widen the gap (`stroke-dasharray="1.9 1.8"`), keep `stroke-width` ≥ 1.7, and re-run Step 4.

- [ ] **Step 6: Precommit and commit**

```bash
cd helios && mix precommit
cd .. && git add helios/lib/helios_web/components/game_components/icons.ex helios/test/helios_web/components/game_components/icons_test.exs
git commit -m "$(cat <<'EOF'
feat(web): military icon matches the card symbol

Round red shield, dashed key border and crossed swords, legible at size-3.5.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
EOF
)"
```

---

### Task 2: Phone status bar rows and the waiting popover

**Files:**
- Modify: `helios/lib/helios_web/components/game_components/top_bar.ex` (`top_bar/1`, new private `waiting_menu/1`)
- Test: `helios/test/helios_web/components/game_components/top_bar_test.exs`

**Interfaces:**
- Produces: `#pass-row` (phone row 2; inside the desktop row on `sm`+) containing `#pass-direction` with the visible text "Pass to ‹full name›". Rendered when the phase is not `:game_over` (phase B widens this condition to "or `discard_count > 0`" when it adds `#discard-pile`).
- Produces: `button#waiting-count` (`aria-haspopup="dialog"`, `aria-controls="waiting-list"`, text "Waiting for N") toggling `#waiting-list` (`role="dialog"`, hidden by default, one `<li>` per name). Both live in the `sm:hidden` wrapper `#waiting-menu` (carries `phx-click-away` and the Escape `phx-window-keydown`); `#waiting-for` remains the desktop line.

- [ ] **Step 1: Write the failing tests**

In `helios/test/helios_web/components/game_components/top_bar_test.exs`:

Replace `render_bar/2` with a version that also takes names:

```elixir
  defp render_bar(view, opts \\ []) do
    render_component(&TopBar.top_bar/1,
      view: view,
      names: Keyword.get(opts, :names, SampleViews.names()),
      connected: Keyword.get(opts, :connected, MapSet.new(["1", "2"]))
    )
  end
```

In the first test ("shows age, turn, direction, …") replace the line

```elixir
    assert attrs(html, "#waiting-count", "title") == ["Bob, Dee"]
```

with

```elixir
    assert text(html, "#waiting-list ul") == "Bob Dee"
```

In "game over replaces the turn information" add after the `#pass-direction` assertion:

```elixir
    assert count(html, "#pass-row") == 0
    assert count(html, "#waiting-list") == 0
```

Replace the last test ("row one stays single-line so the phone bar fits 96px") with:

```elixir
  test "row one stays single-line so the phone bar fits 120px" do
    html = render_bar(SampleViews.view())
    assert hd(attrs(html, "#age-label", "class")) =~ "whitespace-nowrap"
    assert hd(attrs(html, "#turn-label", "class")) =~ "whitespace-nowrap"
    assert hd(attrs(html, "#waiting-count", "class")) =~ "whitespace-nowrap"
    assert html =~ "flex min-w-0 items-center gap-3 sm:gap-4"
    # The pass direction moved out of row one into its own row.
    assert count(html, "#age-label ~ #pass-direction") == 0
    assert count(html, "#pass-row > #pass-direction") == 1
  end
```

Append these tests:

```elixir
  test "the phone pass row shows the full name, wrapping instead of truncating" do
    long = "longplayername0abcdefghij"
    names = Map.put(SampleViews.names(), "2", long)
    html = render_bar(SampleViews.view(), names: names)

    assert text(html, "#pass-row #pass-direction") == "Pass to #{long}"
    assert count(html, "#pass-direction .sr-only") == 0
    assert [name_class] = attrs(html, "#pass-direction > span", "class")
    assert name_class =~ "break-words"
    refute name_class =~ ~r/(^|\s)(sm:)?truncate(\s|$)/
    refute name_class =~ "max-w-24"
  end

  test "the waiting button opens a hidden list of who we wait for" do
    html = render_bar(SampleViews.view())

    assert count(html, "button#waiting-count") == 1
    assert attrs(html, "#waiting-count", "type") == ["button"]
    assert attrs(html, "#waiting-count", "aria-haspopup") == ["dialog"]
    assert attrs(html, "#waiting-count", "aria-controls") == ["waiting-list"]
    assert hd(attrs(html, "#waiting-count", "class")) =~ "pointer-coarse:min-h-11"
    assert [js] = attrs(html, "#waiting-count", "phx-click")
    assert js =~ "#waiting-list"

    assert attrs(html, "#waiting-list", "role") == ["dialog"]
    assert hd(attrs(html, "#waiting-list", "class")) =~ "hidden"
    assert count(html, "#waiting-list li") == 2
    assert text(html, "#waiting-list li:first-child") == "Bob"

    # Escape and a tap outside close it; the toggle and the list share one wrapper.
    assert [wrapper_class] = attrs(html, "#waiting-menu", "class")
    assert wrapper_class =~ "sm:hidden"
    assert [away] = attrs(html, "#waiting-menu", "phx-click-away")
    assert away =~ "#waiting-list"
    assert attrs(html, "#waiting-menu", "phx-key") == ["Escape"]
  end
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cd helios && mix test test/helios_web/components/game_components/top_bar_test.exs`
Expected: FAIL — no `#waiting-list`, no `#pass-row`, `#waiting-count` is a `<span>`, the name span has `max-w-24 truncate` and the "Pass to " prefix is `sr-only`.

- [ ] **Step 3: Implement**

Replace `helios/lib/helios_web/components/game_components/top_bar.ex` with:

```elixir
defmodule HeliosWeb.GameComponents.TopBar do
  @moduledoc """
  Age, turn, pass direction, who we are waiting for, and who is connected.

  Below `sm` the bar has three rows: age/history/turn/waiting button, then
  `#pass-row` (the pass direction), then the connection strip. From `sm` up
  everything sits in one wrapping row.
  """
  use HeliosWeb, :html

  alias HeliosWeb.GameFormat

  attr :view, :map, required: true
  attr :names, :map, required: true
  attr :connected, :any, required: true, doc: "MapSet of connected engine player ids"

  def top_bar(assigns) do
    assigns =
      assign(assigns,
        phase: assigns.view.phase,
        waiting: GameFormat.waiting_for(assigns.view, assigns.names),
        statuses: GameFormat.statuses(assigns.view),
        pass_to: GameFormat.player_name(assigns.names, GameFormat.pass_target(assigns.view))
      )

    ~H"""
    <header
      id="top-bar"
      data-phase={@phase.kind}
      data-turn-key={"#{@phase.age}-#{@phase.turn}"}
      class="sticky top-0 z-20 flex flex-col gap-1.5 rounded-xl bg-linear-to-r from-header-from to-header-to px-3 py-2 text-white shadow-lg short:static sm:flex-row sm:flex-wrap sm:items-center sm:justify-between sm:gap-3 sm:px-4 sm:py-3"
    >
      <div class="flex min-w-0 flex-col gap-1.5 sm:flex-row sm:items-center sm:gap-4">
        <div class="flex items-center justify-between gap-3 sm:justify-start">
          <div class="flex min-w-0 items-center gap-3 sm:gap-4">
            <span id="age-label" class="text-lg font-bold tracking-wide whitespace-nowrap sm:text-xl">
              Age {GameFormat.roman(@phase.age)}
            </span>
            <button
              id="history-button"
              type="button"
              phx-click={JS.toggle(to: "#history-drawer", display: "block")}
              aria-controls="history-drawer"
              class="inline-flex items-center justify-center gap-1 rounded-md bg-white/15 px-2 py-1 text-sm font-semibold transition hover:bg-white/25 pointer-coarse:min-h-11 pointer-coarse:min-w-11"
            >
              <.icon name="hero-book-open" class="size-4" />
              <span class="sr-only sm:not-sr-only">
                History
              </span>
            </button>
            <%= if @phase.kind == :game_over do %>
              <span id="turn-label" class="whitespace-nowrap font-semibold">Game over</span>
            <% else %>
              <span id="turn-label" class="whitespace-nowrap">Turn {@phase.turn}/6</span>
            <% end %>
          </div>
          <.waiting_menu :if={@waiting != []} waiting={@waiting} />
        </div>
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
      </div>
      <p :if={@waiting != []} id="waiting-for" class="hidden text-sm text-white/90 sm:block">
        Waiting for: {Enum.join(@waiting, ", ")}
      </p>
      <ul
        id="connections"
        class="-mx-3 flex items-center gap-3 overflow-x-auto px-3 sm:mx-0 sm:flex-wrap sm:overflow-visible sm:px-0"
      >
        <li
          :for={player <- @view.players}
          id={"connection-#{player.name}"}
          data-connected={to_string(MapSet.member?(@connected, player.name))}
          data-status={@statuses[player.name]}
          class="flex shrink-0 items-center gap-1.5 text-sm"
        >
          <span class={[
            "size-2.5 shrink-0 rounded-full transition",
            if(MapSet.member?(@connected, player.name), do: "bg-emerald-400", else: "bg-disconnected")
          ]}>
          </span>
          <span class="max-w-32 truncate sm:max-w-none">
            {GameFormat.player_name(@names, player.name)}
          </span>
          <.icon
            :if={@statuses[player.name] == :chosen}
            name="hero-check"
            class="size-4 shrink-0 text-emerald-200"
          />
        </li>
      </ul>
    </header>
    """
  end

  attr :waiting, :list, required: true, doc: "display names of the players we wait for"

  # Phone-only "Waiting for N" button with a popover listing the names. Same
  # pattern as the stage popover (stage_strip.ex): JS.toggle, click-away and
  # Escape close it, no server state. Click-away sits on the wrapper so tapping
  # the button again toggles the list instead of closing and reopening it.
  defp waiting_menu(assigns) do
    ~H"""
    <div
      id="waiting-menu"
      class="relative sm:hidden"
      phx-click-away={JS.hide(to: "#waiting-list")}
      phx-window-keydown={JS.hide(to: "#waiting-list")}
      phx-key="Escape"
    >
      <button
        id="waiting-count"
        type="button"
        aria-haspopup="dialog"
        aria-controls="waiting-list"
        phx-click={JS.toggle(to: "#waiting-list", display: "block")}
        class="flex items-center gap-1 whitespace-nowrap rounded-md bg-white/15 px-2 py-1 text-sm text-white/90 transition hover:bg-white/25 pointer-coarse:min-h-11"
      >
        <.icon name="hero-clock" class="size-4" /> Waiting for {length(@waiting)}
      </button>
      <div
        id="waiting-list"
        role="dialog"
        aria-label="Waiting for"
        class="absolute top-full right-0 z-30 mt-2 hidden w-max max-w-[calc(100vw-2rem)] rounded-xl bg-white p-3 text-left text-sm text-zinc-800 shadow-2xl ring-1 ring-black/10"
      >
        <p class="text-xs font-semibold text-zinc-500">Waiting for</p>
        <ul class="mt-1 flex flex-col gap-1">
          <li :for={name <- @waiting} class="break-words">{name}</li>
        </ul>
      </div>
    </div>
    """
  end
end
```

Notes:
- The "Pass to" prefix is plain text now (no `sr-only`), on every width.
- `text-sm` on phones keeps row 2 at one 20px line for the longest (24-character) name at 360px; `break-words` wraps an unbroken name instead of overflowing if it ever doesn't fit.
- The `<p>` inside `#waiting-list` sits outside the `<ul>`, so `text(html, "#waiting-list ul")` is just the names.

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cd helios && mix test test/helios_web/components/game_components/top_bar_test.exs test/helios_web/live/game_live_test.exs`
Expected: PASS (no other ExUnit test references `#waiting-count`; checked with `grep -rn "waiting-count" helios/test`).

- [ ] **Step 5: Precommit and commit**

```bash
cd helios && mix precommit
cd .. && git add helios/lib/helios_web/components/game_components/top_bar.ex helios/test/helios_web/components/game_components/top_bar_test.exs
git commit -m "$(cat <<'EOF'
feat(web): phone status bar shows full pass-to name and lists who we wait for

Pass direction moves to its own #pass-row on phones; "Waiting for N" becomes a
button toggling a #waiting-list popover.

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
EOF
)"
```

---

### Task 3: No waiting UI in replay

**Files:**
- Modify: `helios/lib/helios_web/components/game_components/top_bar.ex` (`top_bar/1`: attr `replay`, `data-replay`)
- Modify: `helios/lib/helios_web/live/game_live.ex` (`render/1`, the `<.top_bar>` call)
- Test: `helios/test/helios_web/components/game_components/top_bar_test.exs`, `helios/test/helios_web/live/game_live_test.exs`

**Interfaces:**
- Produces: `top_bar/1` attr `replay` (`:boolean`, default `false`). When `true`, `waiting` is `[]`: no `#waiting-count`, `#waiting-list` or `#waiting-for`. `#top-bar` carries `data-replay="true"|"false"` (makes the wiring observable in LiveView tests; phase B and E may rely on it).
- Consumes: `GameFormat.waiting_for/2` unchanged.

- [ ] **Step 1: Write the failing tests**

In `helios/test/helios_web/components/game_components/top_bar_test.exs`, add the `replay` option to `render_bar/2`:

```elixir
  defp render_bar(view, opts \\ []) do
    render_component(&TopBar.top_bar/1,
      view: view,
      names: Keyword.get(opts, :names, SampleViews.names()),
      connected: Keyword.get(opts, :connected, MapSet.new(["1", "2"])),
      replay: Keyword.get(opts, :replay, false)
    )
  end
```

and append:

```elixir
  test "a replay frame never shows who we wait for, even on an extra turn" do
    view = SampleViews.view()

    extra = %{
      view
      | phase: %{
          view.phase
          | kind: :extra_turn,
            extra_turn_player: "2",
            extra_turn_kind: :build_from_discard
        }
    }

    live = render_bar(extra)
    assert attrs(live, "#top-bar", "data-replay") == ["false"]
    assert text(live, "#waiting-count") == "Waiting for 1"
    assert text(live, "#waiting-list ul") == "Bob"

    replay = render_bar(extra, replay: true)
    assert attrs(replay, "#top-bar", "data-replay") == ["true"]
    assert count(replay, "#waiting-count") == 0
    assert count(replay, "#waiting-list") == 0
    assert count(replay, "#waiting-for") == 0
    assert text(replay, "#pass-direction") == "Pass to Bob"
  end
```

In `helios/test/helios_web/live/game_live_test.exs`:

In "a seated player sees the table", after the `#top-bar[data-turn-key='1-1']` assertion add:

```elixir
    assert has_element?(view, "#top-bar[data-replay='false']")
    assert has_element?(view, "#waiting-count", "Waiting for 3")
```

In the `describe "replay of a finished game"` test "starts on the final table and steps through turns", after `assert has_element?(view, "#scoreboard")` (the first one) add:

```elixir
      assert has_element?(view, "#top-bar[data-replay='true']")
```

and after `assert has_element?(view, "#replay-position", "Age I · Turn 1")` add:

```elixir
      refute has_element?(view, "#waiting-count")
      refute has_element?(view, "#waiting-for")
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cd helios && mix test test/helios_web/components/game_components/top_bar_test.exs test/helios_web/live/game_live_test.exs`
Expected: FAIL — `#top-bar` has no `data-replay`, and the replay render still shows "Waiting for 1".

- [ ] **Step 3: Implement**

`helios/lib/helios_web/components/game_components/top_bar.ex` — add the attr after `connected`:

```elixir
  attr :replay, :boolean,
    default: false,
    doc: "true while stepping through a finished game: nobody is waiting, so no waiting UI"
```

compute `waiting` from it in `top_bar/1`:

```elixir
        waiting:
          if(assigns.replay, do: [], else: GameFormat.waiting_for(assigns.view, assigns.names)),
```

and add the attribute to the `<header>` after `data-turn-key`:

```heex
      data-replay={to_string(@replay)}
```

`helios/lib/helios_web/live/game_live.ex` — in `render/1`, the `<.top_bar>` call becomes:

```heex
          <.top_bar
            view={
              if @replay,
                do: %{@view | phase: current_frame(@replay).phase, submitted: []},
                else: @view
            }
            names={@names}
            connected={@connected}
            replay={@replay != nil}
          />
```

(`submitted: []` stays: it keeps the Chosen/Thinking check marks off the connection strip in replay.)

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cd helios && mix test test/helios_web/components/game_components/top_bar_test.exs test/helios_web/live/game_live_test.exs test/helios_web/live/game_live_extra_turns_test.exs`
Expected: PASS.

- [ ] **Step 5: Precommit and commit**

```bash
cd helios && mix precommit
cd .. && git add helios/lib/helios_web/components/game_components/top_bar.ex helios/lib/helios_web/live/game_live.ex helios/test/helios_web/components/game_components/top_bar_test.exs helios/test/helios_web/live/game_live_test.exs
git commit -m "$(cat <<'EOF'
fix(web): replay frames show no waiting status

top_bar gains a replay attr; extra-turn frames no longer claim "Waiting for 1".

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
EOF
)"
```

---

### Task 4: Phone header budget and waiting popover in e2e; phase gate

**Files:**
- Modify: `e2e/tests/responsive.spec.ts` ("game table" and "seven players on a small phone")

**Interfaces:**
- Consumes: `#pass-row`, `#pass-direction`, `button#waiting-count`, `#waiting-list` (Task 2); helpers `boxOf`, `expectTapTargets`, `expectNoHorizontalScroll` from `./support/layout`; `players` from `setupTable` (names are 24 characters with `LONG_NAME_PREFIX`).

- [ ] **Step 1: Write the failing e2e changes**

In `e2e/tests/responsive.spec.ts`, "game table", replace the `// --- top bar` block:

```ts
      // --- top bar
      if (vp.width < 640) {
        // Phones: three rows (status, pass direction, connections) within 120px.
        expect((await boxOf(page.locator("#top-bar"))).height).toBeLessThanOrEqual(120);
        const age = await boxOf(page.locator("#age-label"));
        const passRow = await boxOf(page.locator("#pass-row"));
        expect(passRow.y).toBeGreaterThanOrEqual(age.y + age.height);

        // The full "Pass to <name>" is readable: prefix visible, name not truncated.
        const pass = (await page.locator("#pass-direction").innerText()).replace(/\s+/g, " ").trim();
        expect(pass).toMatch(/^Pass to /);
        expect(players.some((p) => pass.endsWith(p.name)), pass).toBe(true);
        const direction = await boxOf(page.locator("#pass-direction"));
        expect(direction.x + direction.width).toBeLessThanOrEqual(vp.width);

        // Tapping "Waiting for N" lists the names; Escape closes the list.
        await expect(page.locator("#waiting-count")).toBeVisible();
        await expect(page.locator("#waiting-for")).toBeHidden();
        await expect(page.locator("#waiting-list")).toBeHidden();
        if (vp.touch) await expectTapTargets(page.locator("#waiting-count"));
        await page.locator("#waiting-count").click();
        await expect(page.locator("#waiting-list")).toBeVisible();
        await expect(page.locator("#waiting-list li")).toHaveCount(3);
        await expectNoHorizontalScroll(page);
        await page.keyboard.press("Escape");
        await expect(page.locator("#waiting-list")).toBeHidden();
      } else {
        await expect(page.locator("#waiting-for")).toBeVisible();
        await expect(page.locator("#waiting-count")).toBeHidden();
      }
```

In the `if (vp.width < 768)` table block, replace

```ts
        await playFirstBuildableOrDiscard(players[1].page);
        await expect(page.locator("#waiting-count")).toHaveText(/Waiting for 2/);
        await expect(page.locator("#west-panel-details")).toBeVisible();
```

with

```ts
        // Phones: open the list before the update to check it survives the patch.
        if (vp.width < 640) await page.locator("#waiting-count").click();
        await playFirstBuildableOrDiscard(players[1].page);
        await expect(page.locator("#waiting-count")).toHaveText(/Waiting for 2/);
        if (vp.width < 640) {
          await expect(page.locator("#waiting-list")).toBeVisible();
          await expect(page.locator("#waiting-list li")).toHaveCount(2);
          await page.locator("#age-label").click();
          await expect(page.locator("#waiting-list")).toBeHidden();
        }
        await expect(page.locator("#west-panel-details")).toBeVisible();
```

In "seven players on a small phone" change the budget line to:

```ts
    expect((await boxOf(page.locator("#top-bar"))).height).toBeLessThanOrEqual(120);
```

- [ ] **Step 2: Run the e2e suite for this spec**

Run (needs Tasks 1–3 merged into the working tree; the Playwright config starts Helios with `MIX_ENV=e2e` on port 4004):

```bash
cd e2e && npm ci && npx playwright install chromium && npx playwright test tests/responsive.spec.ts
```

Expected: PASS on every viewport. Predicted phone bar height on touch viewports ≈ 112px (16 padding + 44 row 1, set by the `pointer-coarse:min-h-11` buttons + 6 + 20 row 2 + 6 + 20 connections); take the real value from the run. If it exceeds 120px, first check whether row 2 wrapped (a name wider than the bar) or row 1 wrapped (it must not: every row-1 item is `whitespace-nowrap`); do not raise the budget without asking — report the measured height instead.

To see these steps fail first (TDD sanity check), run the same command on a checkout without Task 2: `#pass-row` and `#waiting-list` do not exist, so the phone viewports fail at `boxOf(page.locator("#pass-row"))`.

- [ ] **Step 3: Phase gate — full checks**

```bash
cd helios && mix precommit
cd ../e2e && npx playwright test
```

Expected: all green (helios ExUnit suite and the full Playwright suite — `game.spec.ts` replay steps are unaffected by the top bar changes).

- [ ] **Step 4: Commit**

```bash
git add e2e/tests/responsive.spec.ts
git commit -m "$(cat <<'EOF'
test(e2e): phone status bar budget 120px, full pass-to name, waiting list

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
EOF
)"
```

---

## Spec coverage (self-check)

| Spec item | Task |
|---|---|
| A: three phone rows, row 1 single-line, `#pass-direction` in row 2 with full wrapping name and visible "Pass to" | 2 |
| A: `#pass-row` ready for the discard-pile marker (C/phase B), omitted on game over | 2 |
| A: `sm`+ unchanged single wrapping row | 2 (`#pass-row` sits in the desktop row) |
| A: height budget 96 → 120px in `top_bar_test.exs` and `responsive.spec.ts` | 2, 4 |
| A: `#waiting-count` button, `aria-haspopup`/`aria-controls`, `#waiting-list` `<li>` per name, toggle/click-away/Escape, `pointer-coarse:min-h-11`, desktop keeps `#waiting-for` | 2, 4 |
| A: replay passes no waiting via a `replay` attr; `GameFormat.waiting_for/2` unchanged | 3 |
| B: round shield, dark rim, red face, dashed light ring, two crossed swords (light blades, dark hilts), readable at `size-3.5`, `icons_test.exs` asserts structure | 1 |
| Testing: e2e tapping the waiting button shows the names | 4 |
