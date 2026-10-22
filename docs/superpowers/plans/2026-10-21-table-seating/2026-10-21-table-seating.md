# Table Seating Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Seat the boards around a ring on wide screens and in ring order down the page on narrow ones. Your board is always full; other boards are fixed-height compact boards whose cards open on demand. Every board names its neighbours with on-screen arrows, and the table uses the width of large monitors up to a measured maximum.

**Architecture:** A new pure module `HeliosWeb.TableLayout` turns `players` + `me` into seats in narrow order (W, You, E, E2…W2). Each seat carries a role, its neighbours, a ring slot (left/width in % of the table, top in rem), arrows and a panel alignment. It also produces the ring height and an SVG path for the ring line. Because compact boards have a fixed height and your board's room is reserved, the geometry depends only on player count, so the server computes it all and no layout JS is needed. `GameLive.table_area/1` renders the seats once, in narrow order: below `xl` they form a single column; from `xl` up the side boards are absolutely positioned from CSS custom properties. A colocated hook `.BoardPanels` handles opening and closing other boards' cards (one floating panel on wide screens, in-place expansion on narrow ones). Helios only: no engine, NIF or persistence change, and `ENGINE_VERSION` is untouched.

**Tech Stack:** Phoenix 1.8, LiveView 1.1 (colocated hooks, `this.js()`), Tailwind CSS 4.1, ExUnit + LazyHTML (`HeliosWeb.HTMLHelpers`), Playwright.

**Spec:** `docs/superpowers/specs/2026-10-21-table-seating/2026-10-21-table-seating-design.md`

## Global Constraints

- Read `helios/AGENTS.md` before touching `helios/`; it takes precedence there (HEEx rules, `<.icon>`, colocated hooks only, no inline `<script>` outside `ColocatedHook`, `Layouts.app` first).
- Seat convention: for the viewer at index `i` in `players`, East is `i+1` and West is `i-1` (mod n), as in `GameFormat.neighbours_of/3`.
- Narrow order is **West, You, East, E2, E3, …, W2**. This reverses the responsive-UI rule "own board first below `lg`": remove `order-first` from `#my-board`.
- Ring breakpoint: `xl` (80rem / 1280px). Below `xl` the single-column list is used, centred and capped at `max-w-2xl`. Today's `md` two-column grid goes away.
- Ring line: `3px` stroke, `rgba(82,82,91,.30)`, `vector-effect="non-scaling-stroke"`, `aria-hidden="true"`, no markers.
- Compass words: only `You`, `West`, `East` and (4 or 6 players) `Opposite` labels. Every other board's label is `""`.
- Your board always shows its built cards. Other boards are compact by default on every screen size, and the compact part has a fixed height (`h-60`). The random-move badge overlays the board and must not add a row.
- Wide screens (`xl`+): opening, closing, building cards and status changes never move any board. Narrow screens: the tapped board never moves, and boards below it may shift down.
- Kept DOM ids: `#table`, `#my-board`, `#west-panel`, `#east-panel`, `section[data-player=…]`, `#my-built`, `#built-cards-<id>`, `#built-<id>-<slug>`, `#wonder-stages`, `#wonder-stages-<id>`, `#status-<id>`, `#<board>-toggle`, `#<board>-details`, `[data-stat=…]`, `[data-tokens]`, `[data-latest]`. Other boards' ids change from `#player-<id>` (article wrapper) / `#board-<id>` to a single `section#board-<id>`. `#other-players` goes away.
- Reduced motion: no animated opening of panels or expanded boards under `prefers-reduced-motion`.
- Touch targets ≥ 44px on coarse pointers (`pointer-coarse:min-h-11`).
- Stage explicit paths only (never `git add -A` / `git add .`). Run `cd helios && mix precommit` before each commit touching `helios/`; run the e2e suite for tasks touching `e2e/`.

## Review Focus

- **A board's cards panel near the right edge** (East side) must open aligned to the board's right edge and stay inside the viewport, never causing horizontal page scroll (Task 3 component test on `data-panel-align`, Task 5 e2e no-horizontal-scroll with the East panel open).
- **The open state survives a LiveView patch.** A panel opened before another player's move stays open after the patch, on both wide and narrow screens (Task 5 e2e).
- **Tapping a card inside an open panel** opens the card explanation, and the panel stays open underneath. Clicks inside `#card-sheet` or on `#card-sheet-scrim` don't count as "outside" (Task 4 hook rule, Task 5 e2e).
- **Six military tokens and a long (24-character) name** on a compact board don't grow its height: the summary stays exactly `h-60` (Task 3 component test asserts classes; Task 5 e2e asserts equal heights for all compact boards with `LONG_NAME_PREFIX`).
- **A tall own board** (more cards in one colour than the reserved room) must not overlap the bottom boards on `xl`: `#my-built` scrolls vertically inside the reserved height instead (Task 6 measurement test with injected tiles).

---

## File Structure

| File | Responsibility |
|---|---|
| `helios/lib/helios_web/table_layout.ex` (new) | Pure seat geometry: order, roles, neighbours, ring slots, arrows, ring path |
| `helios/lib/helios_web/components/game_components/player_board.ex` | Full board (yours) and compact board (others) with neighbour line, category chips, cards panel |
| `helios/lib/helios_web/live/game_live.ex` | `table_area/1` renders seats + ring; container width; assigns `layout` instead of `west/east/others` |
| `helios/lib/helios_web/components/game_components/fx.ex` | Animation fallback onto a compact board's category chip |
| `e2e/tests/seating.spec.ts` (new) | Ring/no-move/phone-order/measurement tests |
| `e2e/tests/responsive.spec.ts` | Updated for compact boards on all sizes and the removed `#other-players` |

---

### Task 1: `HeliosWeb.TableLayout`, pure seat geometry

**Files:**
- Create: `helios/lib/helios_web/table_layout.ex`
- Test: `helios/test/helios_web/table_layout_test.exs`

**Interfaces:**
- Produces:
  ```elixir
  @type arrow :: String.t()   # one of → ↘ ↓ ↙ ← ↖ ↑ ↗ ↥ ↧
  @type seat :: %{
          player: map(),                       # the board map (has :name)
          role: :me | :west | :east | :opposite | nil,
          west: String.t(), east: String.t(),  # neighbour player ids
          ring: %{left: number(), top: number(), width: number()},  # left/width in %, top in rem
          ring_arrows: {arrow, arrow},         # {towards west neighbour, towards east neighbour}
          list_arrows: {arrow, arrow},
          panel_align: :left | :right
        }
  @spec build([map()], String.t()) ::
          %{seats: [seat], ring_height: number(), ring_path: String.t()}
  @spec label(seat) :: String.t()            # "You" | "West" | "East" | "Opposite" | ""
  ```
  `seats` are in narrow order: W, me, E, E2, …, W2.

**Geometry (pinned values, tuned only in Task 6).** Module attributes: `@me_left 30`, `@me_width 40`, `@side_width 22` (%), `@compact_height 15`, `@me_reserve 48`, `@gap 1` (rem), `@nominal_width 96` (rem, used only for arrows). `bottom = @me_reserve + @gap` (49). Slots by ring offset `k` from you (k=0 you, 1 = E, n−1 = W):

| n | k=0 (you) | E side (k=1,2,…) left / top | W side (k=n−1,n−2,…) left / top | bottom |
|---|---|---|---|---|
| 3 | 30 / 0 | E 78 / 4 | W 0 / 4 | — |
| 4 | 30 / 0 | E 78 / 24 | W 0 / 24 | Opp (k=2) 39 / 49 |
| 5 | 30 / 0 | E 78 / 18 | W 0 / 18 | E2 (k=2) 51 / 49, W2 (k=3) 27 / 49 |
| 6 | 30 / 0 | E 78 / 8, E2 78 / 30 | W 0 / 8, W2 0 / 30 | Opp (k=3) 39 / 49 |
| 7 | 30 / 0 | E 78 / 6, E2 78 / 27 | W 0 / 6, W2 0 / 27 | E3 (k=3) 51 / 49, W3 (k=4) 27 / 49 |

Widths: you `@me_width`, everyone else `@side_width`. `ring_height` is `4 + @compact_height` (19) for n=3, and `bottom + @compact_height` (64) otherwise.

Anchors (for arrows and the ring path): you `(50, 7)`; others `(left + 11, top + 7.5)`. Ring arrow from seat A to neighbour B: `dx = (Bx − Ax) * @nominal_width / 100`, `dy = By − Ay`, angle `atan2(dy, dx)`, snapped to 45° sectors: → ↘ ↓ ↙ ← ↖ ↑ ↗ (0°, 45°, … with y pointing down). List arrows: the west neighbour is the previous seat (`↑`), or `↧` for the first seat; the east neighbour is the next seat (`↓`), or `↥` for the last seat. `panel_align` is `:right` when `left > 50`, else `:left`. Roles: k=0 `:me`, k=1 `:east`, k=n−1 `:west`, even n with k=n/2 `:opposite`, else `nil`.

Ring path: closed Catmull-Rom through the anchors in ring order (k = 0…n−1), converted to cubic Béziers (`c1 = Pᵢ + (Pᵢ₊₁ − Pᵢ₋₁)/6`, `c2 = Pᵢ₊₁ − (Pᵢ₊₂ − Pᵢ)/6`), numbers formatted with 2 decimals: `"M 50.00 7.00 C … Z"`.

- [ ] **Step 1: Write the failing tests** in `table_layout_test.exs` (`use ExUnit.Case, async: true`; players built as `%{name: "1"}` … `%{name: "n"}`, me `"1"`):

```elixir
test "narrow order is W, me, E, then on around the table" do
  for n <- 3..7 do
    names = build(players(n), "1").seats |> Enum.map(& &1.player.name)
    ids = Enum.map(1..n, &to_string/1)
    assert names == [List.last(ids) | Enum.take(ids, n - 1)]
  end
end

test "roles: you, west, east, and opposite only for even counts" do
  assert roles(4) == [:west, :me, :east, :opposite]
  assert roles(5) == [:west, :me, :east, nil, nil]
  assert roles(6) == [:west, :me, :east, nil, :opposite, nil]
  assert Enum.map(build(players(4), "1").seats, &TableLayout.label/1) == ["West", "You", "East", "Opposite"]
end

test "every seat's neighbours follow seat order (east = i+1)" do
  seat = Enum.find(build(players(7), "1").seats, &(&1.player.name == "4"))
  assert {seat.west, seat.east} == {"3", "5"}
end

test "seven-player ring arrows point where the neighbours sit" do
  # narrow order W(7), me(1), E(2), E2(3), E3(4), W3(5), W2(6)
  assert ring_arrows(7) == [
           {"↓", "→"}, {"←", "→"}, {"←", "↓"}, {"↑", "↙"}, {"↗", "←"}, {"→", "↖"}, {"↘", "↑"}
         ]
end

test "list arrows wrap at both ends" do
  assert list_arrows(4) == [{"↧", "↓"}, {"↑", "↓"}, {"↑", "↓"}, {"↑", "↥"}]
end

test "slots, heights and panel alignment" do
  %{seats: seats, ring_height: h} = build(players(7), "1")
  assert h == 64
  assert Enum.map(seats, &{&1.ring.left, &1.ring.top}) ==
           [{0, 6}, {30, 0}, {78, 6}, {78, 27}, {51, 49}, {27, 49}, {0, 27}]
  assert Enum.map(seats, & &1.panel_align) == [:left, :left, :right, :right, :right, :left, :left]
  assert build(players(3), "1").ring_height == 19
end

test "ring path is a closed curve through every seat starting at you" do
  path = build(players(5), "1").ring_path
  assert String.starts_with?(path, "M 50.00 7.00 C ")
  assert path |> String.split(" C ") |> length() == 6
  assert String.ends_with?(path, " Z")
end

test "the viewer may sit anywhere in the seat list" do
  names = build(players(5), "3").seats |> Enum.map(& &1.player.name)
  assert names == ["2", "3", "4", "5", "1"]
end
```

(`roles/1`, `ring_arrows/1`, `list_arrows/1` are small private helpers in the test mapping over `build(players(n), "1").seats`.)

- [ ] **Step 2: Run tests to verify they fail**

Run: `cd helios && mix test test/helios_web/table_layout_test.exs`
Expected: FAIL (`HeliosWeb.TableLayout` undefined)

- [ ] **Step 3: Implement `HeliosWeb.TableLayout.build/2` and `label/1`** with the slot table as a module attribute keyed by n, and pure functions for anchors, arrows and the path. Raise `ArgumentError` for n outside 3..7.

- [ ] **Step 4: Run tests to verify they pass**

Run: `cd helios && mix test test/helios_web/table_layout_test.exs`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
cd helios && mix precommit
git add helios/lib/helios_web/table_layout.ex helios/test/helios_web/table_layout_test.exs
git commit -m "feat(table): seat geometry for the ring and narrow order"
```

---

### Task 2: Neighbour line on every board

**Files:**
- Modify: `helios/lib/helios_web/components/game_components/player_board.ex`
- Test: `helios/test/helios_web/components/game_components/player_board_test.exs`

**Interfaces:**
- Consumes: `TableLayout.seat` (Task 1).
- Produces: new `player_board/1` attr `neighbours` (`:map`, default `nil`) with the shape
  `%{west: String.t(), east: String.t(), ring: {arrow, arrow}, list: {arrow, arrow}}` (display names already resolved). A helper `TableLayout.neighbours(seat, names, me) :: map` builds it, using `"You"` for `me` and `GameFormat.player_name/2` otherwise. Add it to `table_layout.ex` with its own test.

- [ ] **Step 1: Write the failing tests**

```elixir
test "the neighbour line shows ring arrows on xl and list arrows below" do
  html = board(SampleViews.player("2"), neighbours: %{west: "You", east: "Cid", ring: {"←", "↓"}, list: {"↑", "↓"}})
  assert attrs(html, "#east-panel [data-neighbours]", "aria-label") == ["Neighbours: You and Cid"]
  assert text(html, "#east-panel [data-neighbours] [data-ring-arrow='west']") == "←"
  assert text(html, "#east-panel [data-neighbours] [data-list-arrow='east']") == "↓"
  assert attrs(html, "[data-ring-arrow='west']", "class") |> hd() =~ "hidden xl:inline"
  assert attrs(html, "[data-list-arrow='west']", "class") |> hd() =~ "xl:hidden"
  assert attrs(html, "[data-ring-arrow]", "aria-hidden") |> Enum.uniq() == ["true"]
end

# table_layout_test.exs
test "neighbours/3 resolves display names, You for the viewer" do
  seat = hd(build(players(3), "1").seats)   # West = "3"
  assert TableLayout.neighbours(seat, %{"2" => "Bob", "3" => "Cid"}, "1") ==
           %{west: "Bob", east: "You", ring: seat.ring_arrows, list: seat.list_arrows}
end
```

- [ ] **Step 2: Run to verify they fail**

Run: `cd helios && mix test test/helios_web/components/game_components/player_board_test.exs test/helios_web/table_layout_test.exs`
Expected: FAIL

- [ ] **Step 3: Implement.** Render `<p data-neighbours aria-label="Neighbours: {west} and {east}">` with the west name, then ` · `, then the east name, each preceded by its two arrow spans (`data-ring-arrow="west|east"` with `hidden xl:inline`, and `data-list-arrow="west|east"` with `xl:hidden`). Render nothing when `neighbours` is `nil`. Place it under the header, on both your board and compact boards.

- [ ] **Step 4: Run to verify they pass** (same command). Expected: PASS

- [ ] **Step 5: Commit**

```bash
cd helios && mix precommit
git add helios/lib/helios_web/components/game_components/player_board.ex helios/lib/helios_web/table_layout.ex helios/test/helios_web/components/game_components/player_board_test.exs helios/test/helios_web/table_layout_test.exs
git commit -m "feat(table): neighbour line with on-screen arrows"
```

---

### Task 3: Compact boards for everyone but you

**Files:**
- Modify: `helios/lib/helios_web/components/game_components/player_board.ex`
- Modify: `helios/lib/helios_web/components/game_components/player_panels.ex` (`player_stats/1`: `data-stats`, `class` attr)
- Test: `helios/test/helios_web/components/game_components/player_board_test.exs`

**Interfaces:**
- Produces, for non-`mine` boards:
  - `#<id>-summary`: fixed `h-60` block, containing the header (label, name, AI icon, `#status-<id>`), the neighbour line, the wonder image (`h-20 w-full object-cover`) with `stage_strip`, `player_stats` (single line, `flex-nowrap overflow-hidden`), and the toggle.
  - `#<id>-toggle` (`<button type="button" data-board-toggle aria-controls="<id>-details" aria-expanded="false">`, `pointer-coarse:min-h-11`): one chip per category from `GameFormat.category_counts/1`, as `<span data-chip={category}>` coloured with `GameFormat.category_class/1` and showing the count, then a `hero-chevron-down` icon. It carries **no** `phx-click`; Task 4's hook owns it.
  - `#<id>-details` (`hidden group-[.is-open]:block`) containing `#built-cards-<id>` (tiles as today). On `xl`: `xl:absolute xl:top-full xl:mt-2 xl:z-40 xl:w-max xl:max-w-[min(40rem,calc(100vw-2rem))] xl:shadow-xl`, plus `xl:left-0` or `xl:right-0` from a new attr `panel_align` (`:left | :right`, default `:left`), also rendered as `data-panel-align`.
  - The random-move badge becomes `absolute top-2 right-2 z-10` (overlay).
  - The section stays `group relative` with `data-player`.
- `mine` boards are unchanged apart from the neighbour line (Task 2): always full, no toggle, `#my-built`. On `xl` only, `#my-built` gets `xl:max-h-[var(--me-built-max)] xl:overflow-y-auto` so a very tall board can't spill past its reserved room. `--me-built-max` is set by `table_area` in Task 4.
- Remove the old `md:hidden` summary button and the `md:flex` details rule.

- [ ] **Step 1: Rewrite the failing tests.** Replace "non-own boards fold into a summary toggle below md" and update "a neighbour's full board…" so header and stats are read from `#east-panel-summary`, and tiles from `#built-cards-2` inside `#east-panel-details`. Add:

```elixir
test "compact board: fixed-height summary, category chips, hidden cards panel" do
  html = board(SampleViews.player("2"), panel_align: :right)
  assert attrs(html, "#east-panel-summary", "class") |> hd() =~ "h-60"
  assert attrs(html, "#east-panel-toggle [data-chip]", "data-chip") == ["raw_material", "civilian", "military"]
  assert text(html, "#east-panel-toggle [data-chip='civilian']") == "1"
  assert attrs(html, "#east-panel-toggle", "phx-click") == []
  assert attrs(html, "#east-panel-toggle", "aria-expanded") == ["false"]
  assert attrs(html, "#east-panel-details", "data-panel-align") == ["right"]
  assert attrs(html, "#east-panel-details", "class") |> hd() =~ "xl:right-0"
  assert attrs(html, "#east-panel-details", "class") |> hd() =~ "hidden group-[.is-open]:block"
  assert attrs(html, "#east-panel-summary [data-stats]", "class") |> hd() =~ "flex-nowrap"
end

test "the random badge overlays instead of adding a row" do
  html = board(SampleViews.player("2"), random: true)
  assert attrs(html, "#random-badge-2", "class") |> hd() =~ "absolute"
end
```

Give `player_stats/1` a `data-stats` root attribute and a `class` attr (default today's classes), so the compact board can pass `flex-nowrap overflow-hidden`.

- [ ] **Step 2: Run to verify they fail**

Run: `cd helios && mix test test/helios_web/components/game_components/player_board_test.exs`
Expected: FAIL

- [ ] **Step 3: Implement the compact board** as specified in Interfaces. Keep `#built-<id>-<slug>` tiles and their `show_card` click unchanged.

- [ ] **Step 4: Run to verify they pass** (same command). Expected: PASS

- [ ] **Step 5: Commit**

```bash
cd helios && mix precommit
git add helios/lib/helios_web/components/game_components/player_board.ex helios/lib/helios_web/components/game_components/player_panels.ex helios/test/helios_web/components/game_components/player_board_test.exs
git commit -m "feat(table): compact boards with category chips and a cards panel"
```

---

### Task 4: The table: ring, narrow order, width, panel behaviour

**Files:**
- Modify: `helios/lib/helios_web/live/game_live.ex` (container at line 52, `table_area/1`, `assign_frame/1`, `assign_view/2`, setup assigns ~line 810)
- Test: `helios/test/helios_web/live/game_live_test.exs`

**Interfaces:**
- Consumes: `TableLayout.build/2`, `TableLayout.neighbours/3`, `TableLayout.label/1` (Tasks 1–2); `player_board` attrs `neighbours`, `panel_align` (Tasks 2–3).
- Produces:
  - Assign `layout` (the `build/2` result, or `nil` during setup), replacing `west`, `east` and `others`. `me` stays.
  - `table_area/1` attrs: `layout`, `me_id`, `names`, `statuses`, `ai_ids`, `random_now`.
  - DOM: `#table` > `#seats`, with the colocated hook `phx-hook=".BoardPanels"`. Inside it, `<svg id="table-ring" class="hidden xl:block absolute inset-x-0 top-0 pointer-events-none" style="height: {ring_height}rem" viewBox="0 0 100 {ring_height}" preserveAspectRatio="none" aria-hidden="true">` containing one `<path d={ring_path}>`. Then one `player_board` per seat in `layout.seats` order, with ids `#west-panel`, `#my-board`, `#east-panel` for the three roles and `#board-<id>` for the rest.

- [ ] **Step 1: Update the failing tests** in `game_live_test.exs`:
  - "a seated player sees the table": keep the three panel assertions, and add `assert has_element?(view, "#table-ring path")` and `refute has_element?(view, "#other-players")`.
  - Replace "every board is on the table; others continue from the east neighbour" with:

```elixir
test "boards are in ring order: W, me, E, then on around the table" do
  {players, game} = table_of(5)
  [{_a, ta} | _] = players
  ids = Enum.map(players, fn {user, _} -> to_string(user.id) end)
  {:ok, view, _html} = open(ta, game.id)

  order =
    view |> render() |> LazyHTML.from_fragment()
    |> LazyHTML.query("#seats > section[data-player]") |> LazyHTML.attribute("data-player")

  assert order == Enum.map([4, 0, 1, 2, 3], &Enum.at(ids, &1))
  assert has_element?(view, "#board-#{Enum.at(ids, 2)} [id^='wonder-stages-']")
  assert has_element?(view, "#board-#{Enum.at(ids, 3)} [data-neighbours]")
  assert has_element?(view, "#seats[style*='--ring-height: 64rem']")
  assert has_element?(view, "#east-panel[style*='--ring-left: 78%']")
  refute has_element?(view, "#my-board.order-first")
end
```

  - Any other test using `#player-<id>` switches to `#board-<id>`.

- [ ] **Step 2: Run to verify they fail**

Run: `cd helios && mix test test/helios_web/live/game_live_test.exs`
Expected: FAIL

- [ ] **Step 3: Implement the assigns and markup.**
  - Container (line 52): `max-w-7xl` → `max-w-[96rem]`. The top bar, replay bar and dock live in this container and follow automatically. The scoreboard keeps its own `max-w-4xl`.
  - `#seats`: `mx-auto flex max-w-2xl flex-col gap-3 sm:gap-4 xl:relative xl:block xl:max-w-none xl:min-h-(--ring-height)`, with `style="--ring-height: {h}rem; --me-built-max: {me_reserve − 14}rem"`. Expose `TableLayout.me_built_max/0` returning the rem number so it isn't duplicated.
  - Your board: `xl:relative xl:z-10 xl:ml-[30%] xl:w-[40%]` (in flow).
  - Other boards: `style="--ring-left: {left}%; --ring-top: {top}rem; --ring-width: {width}%"` and `xl:absolute xl:z-10 xl:left-(--ring-left) xl:top-(--ring-top) xl:w-(--ring-width)`.
  - `assign_view/2` and `assign_frame/1` call `TableLayout.build(players, me)` (the frame's players in replay).

- [ ] **Step 4: Implement the colocated hook `.BoardPanels`** on `#seats` (behaviour pinned; implementation free):
  - A click on `[data-board-toggle]` toggles `is-open` on its `section` and flips `aria-expanded`, via `this.js().toggleClass` / `this.js().setAttribute` so the state is sticky across LiveView patches.
  - Wide (`matchMedia("(min-width: 80rem)")`): opening one board closes any other open one. A click outside every open section, ignoring clicks inside `#card-sheet` or on `#card-sheet-scrim`, closes it. `Escape` closes it, unless `#card-sheet` or `#action-panel` is open (those handle Escape themselves).
  - Narrow: toggle only. Several boards may be open, and clicking outside doesn't close them.
  - Reduced motion: no transition classes on open.

- [ ] **Step 5: Run to verify they pass**

Run: `cd helios && mix test test/helios_web/live/game_live_test.exs && mix test`
Expected: PASS (whole suite: other LiveView tests still find `#west-panel`, `#east-panel`, `#table`)

- [ ] **Step 6: Commit**

```bash
cd helios && mix precommit
git add helios/lib/helios_web/live/game_live.ex helios/test/helios_web/live/game_live_test.exs
git commit -m "feat(table): ring layout, ring-order list and wider table"
```

---

### Task 5: Animations land on compact boards; e2e for seating

**Files:**
- Modify: `helios/lib/helios_web/components/game_components/fx.ex`
- Create: `e2e/tests/seating.spec.ts`
- Modify: `e2e/tests/responsive.spec.ts`

**Interfaces:**
- Consumes: DOM from Tasks 3–4 (`[data-chip]`, `[data-board-toggle]`, `#seats`, `#table-ring`).

- [ ] **Step 1: fx fallback.** In `play/1`, when `target` exists but isn't visible (collapsed panel), arrive at ``section[data-player="${play.player}"] [data-chip="${category}"]``, where `category = target.closest("[data-category]")?.dataset.category`. If there's no chip, use the board's `[data-stat="coins"]`. Same fallback for `fly` when `play.from` is set. Check it manually: play a turn with the West board closed and confirm the chip pulses.

- [ ] **Step 2: Write `seating.spec.ts`** (uses `setupTable`, `startGame`, `waitForTurn`, `playFirstBuildableOrDiscard`, `boxOf`, `expectNoHorizontalScroll`, `LONG_NAME_PREFIX`):
  - `desktop 1280x800, 4 players: ring and nothing moves`:
    - `#table-ring` visible.
    - Labels: West, East and Opposite appear once each.
    - Record the boxes of all `#seats > section:not(#my-board)`, then open `#west-panel-toggle`: `#west-panel-details` is visible and every recorded box is unchanged.
    - Open `#east-panel-toggle`: the west panel closes, the east panel stays inside the viewport, and `expectNoHorizontalScroll` passes.
    - Click a tile in the east panel: `#card-sheet` opens and the east panel stays open.
    - Press Escape to close the sheet. Click on `#age-label`: the panel closes.
    - Open the west panel, then all players play a turn (`waitForTurn(page, 1, 2)`): the panel is still open and the boxes are unchanged.
    - All compact summaries have equal heights.
  - `phone 390x844, 4 players: ring order and in-place expansion`:
    - `#seats > section` ids are `[west-panel, my-board, east-panel, board-<opp>]`.
    - `#table-ring` hidden.
    - Tap `#west-panel-toggle`: the west board's `y` is unchanged, `#my-board`'s `y` increases, and the details are visible.
    - Tap `#east-panel-toggle`: both are open.
    - After another player's move, both are still open.
    - Tap targets ≥ 44px.

- [ ] **Step 3: Update `responsive.spec.ts`:**
  - "game table": the `< 768` branch now applies to every viewport. Details are hidden until the toggle is pressed on all sizes. Delete the `else` branch that expected visible details and a hidden toggle.
  - "seven players on a small phone": replace the `#other-players` assertions with `#seats > section` count 7 and order starting `west-panel, my-board, east-panel`, with every section's right edge ≤ 360.

- [ ] **Step 4: Run e2e**

Run: `cd e2e && npx playwright test seating.spec.ts responsive.spec.ts game.spec.ts`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
cd helios && mix precommit
git add helios/lib/helios_web/components/game_components/fx.ex e2e/tests/seating.spec.ts e2e/tests/responsive.spec.ts
git commit -m "test(e2e): seating ring, panels and phone order; fx lands on compact boards"
```

---

### Task 6: Measure the maximum width and tune the geometry

**Files:**
- Modify: `e2e/tests/seating.spec.ts`
- Modify (values only): `helios/lib/helios_web/table_layout.ex`, `helios/lib/helios_web/live/game_live.ex`
- Modify: `docs/superpowers/specs/2026-10-21-table-seating/2026-10-21-table-seating-design.md` (append a "Measured" section)

- [ ] **Step 1: Write the measurement test** `7 players, fully built own board fits at the max width`:
  - Set up 7 players.
  - Use `page.evaluate` to inject a worst-case board: replace `#my-built`'s children with 7 `[data-category]` columns × 10 clones of an existing tile. For an empty board, clone the first `#hand [data-card] img` into a `h-10 w-16` tile.
  - Then, for viewport widths from 1280 to 2400 in steps of 32px (height 1100), record the first width where all of these hold:
    - `#my-built` `scrollWidth <= clientWidth`
    - no two `#seats > section` boxes intersect
    - every compact board's toggle chips are fully visible
  - Log that width with `console.log`. Assert that the configured container (`#game > div` max width, read through `getComputedStyle`) plus horizontal padding is ≥ that width, and that at the configured max width the three checks hold.

- [ ] **Step 2: Run and tune.** Run `cd e2e && npx playwright test seating.spec.ts -g "fully built"`. If it fails, adjust `max-w-[96rem]` in `game_live.ex` and/or `@me_reserve`, `@side_width` and the slot tops in `table_layout.ex`, keeping the Task 1 pinned values in sync (update those tests to the new numbers). Re-run until it passes. If the layout looks wrong after tuning, stop and ask the user.

- [ ] **Step 3: Record it.** Append to the spec a short "Measured (YYYY-MM-DD)" section: the measured first-fit width, the chosen max width, the final `@me_reserve` / `@side_width` values, and the command that measures it.

- [ ] **Step 4: Full checks**

Run: `cd helios && mix precommit` and `cd e2e && npx playwright test`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add e2e/tests/seating.spec.ts helios/lib/helios_web/table_layout.ex helios/lib/helios_web/live/game_live.ex helios/test/helios_web/table_layout_test.exs docs/superpowers/specs/2026-10-21-table-seating/2026-10-21-table-seating-design.md
git commit -m "feat(table): measured max table width and tuned ring geometry"
```
