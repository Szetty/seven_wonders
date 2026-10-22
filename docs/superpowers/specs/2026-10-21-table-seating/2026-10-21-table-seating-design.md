# Table seating — boards around a ring, compact neighbours, full-width table

Status: approved design (2026-10-21). Builds on `docs/superpowers/specs/2026-09-30-responsive-ui/` and `docs/superpowers/specs/2026-10-04-game-table-ux/`. Helios only; no engine change, `ENGINE_VERSION` unchanged.

## Why

Two problems seen while playing:

1. **On a monitor the table wastes space.** The game page is capped at `max-w-7xl` (~1280px) and centred, so on a ~2000px-wide screen there are big empty margins on both sides, while the three expanded boards squeeze each other and their built cards scroll sideways.
2. **It isn't obvious who sits next to whom.** Only your two neighbours are labelled West/East. Everyone else goes into a flat "other players" grid, so you can't tell, for example, who the player opposite you neighbours. In 7 Wonders that matters: trading, military, and guilds all depend on neighbours.

The goal is a table that reads like a real table: players around a circle, and every player's neighbours obvious at a glance.

## Success looks like

- On any screen, for any player count from 3 to 7, a player can say who any board's two neighbours are without counting seats.
- On a wide monitor the table fills the width until there is no more content to spread out; there are no horizontal scrollbars on your own board, and nothing is squeezed.
- On wide screens, tapping or clicking another player's board to see their cards never moves any board. On phones the tapped board stays put and only the boards below it shift down.

## Goals

- Seat boards in table order (around a ring) on wide screens and in ring order down the page on narrow ones.
- Your board always shows your built cards; other boards are compact until you open them.
- Every board names its two neighbours.
- Use the width of large screens, up to a measured maximum.

## Non-goals

- No rule, engine, NIF, or persistence change.
- The wonder setup screen (`wonder_setup`) keeps its current layout.
- No hover highlight of neighbours (considered and rejected: the labels are enough).
- No direction-of-play markers (chevrons/arrows) on the ring line (tried and rejected as clutter).
- Not using the empty middle of the ring for the hand, discard pile, or anything else. That's a possible later idea, out of scope here.

## Anti-goals (would make this a failure even if it technically works)

- **Boards move (wide screens).** In the ring, opening or closing someone's cards must not shift, resize, or reflow any board. The user's words: moving boards "will feel hard to use". Boards also stay put as the game goes on: building cards, status changes and badges never move them. Only a viewport resize may re-lay out the ring. On phones, expanding a board in place may push the boards below it down (user decision); the tapped board itself must not jump.
- **Compass words that contradict the screen.** On a ring, the direction of play flips at the bottom, so a bottom board's east neighbour can be on its screen-left. Labelling it "East" there is confusing. Only use West/East where they match the screen.
- **Horizontal scrolling inside your own board** on a screen wide enough to avoid it.

## Seat order (all layouts)

Players are in seat order (`view.players`). For the viewer at index `i`, East is `i+1` and West is `i-1` (mod n), the same convention as `GameFormat.neighbours_of/3`. Moving "east" from you goes E, E2, E3, …; moving "west" goes W, W2, W3, …. The two directions meet at the far side of the table: one "opposite" player when n is even, or two players who neighbour each other when n is odd.

When nobody is "you" (a past-game replay viewed by a participant is still "you"; there is currently no spectator view), the same rules apply with the viewing seat as the anchor.

## Wide screens: the ring

On screens wide enough for it (see "Breakpoints"), boards sit around an oval "table":

- **You** at the top centre.
- **East** side down the right, **West** side down the left.
- Each step around the table moves a board down and a little in or out, following the oval. The bottom is where the two sides meet.

Shapes by player count (as agreed with mockups):

| Players | Shape | Bottom |
|---|---|---|
| 3 | triangle: You on top, W lower-left, E lower-right | W and E (they neighbour each other) |
| 4 | rhombus: You, W/E, Opposite | one Opposite board at the bottom point |
| 5 | pentagon | the pair W2, E2, side by side |
| 6 | hexagon | one Opposite board |
| 7 | heptagon | the pair W3, E3, side by side |

3 players changes from today's flat row to a triangle. This was decided on purpose: one rule for every player count.

**The ring line.** A thin, light-grey oval runs behind the boards through their centres, so the loop reads as a table. The agreed look, checked on the real `paper.jpg` background: about `3px solid rgba(82,82,91,.30)` (zinc-600 at 30%). It is decoration only (`aria-hidden`), with no chevrons or markers on it.

**Real board sizes.** The mockups used equal boxes. In the app, your board is wide and grows taller as you build cards, while the other boards are compact (see below). The builder decides how to position boards so the order and oval shape hold up with real sizes. Requirements:
- **Room for a fully built board is reserved from turn 1** (user decision). Your board's built cards hang down into the middle of the ring. The ring is sized from the start for your fullest possible board (all 7 categories, ~20 cards, the tallest category column at its realistic maximum) without overlapping any other board. So boards never move between turns as you build. The accepted cost: a mostly empty ring middle in Age I, and a taller table with 3–4 players.
- **Other boards have a fixed height** (designer's call). Status-chip changes, the "random move" badge, a growing stats row or category summary must not change a compact board's size. The badge overlays and does not add a row, for example. The ring geometry then depends only on player count and viewport width, never on game state.
- So the ring line can follow the known geometry. It only needs recomputing when the viewport is resized.
- Opening another board's cards (below) must not change the layout.

## Narrow screens: ring order down the page

Below the ring breakpoint, boards are one column in this order:

**West, You, East, E2, E3, …, W2.**

So each board's west neighbour is directly above it and its east neighbour directly below, except the last board, whose east neighbour wraps back to West at the top. `GameFormat.others/1` already returns E2…W2 in this order.

This deliberately reverses the responsive-UI spec's "own board (`#my-board`) first on every size below `lg`". Today `#my-board` has `order-first`, so phones show You, West, East, …. The user asked for West first, so that the "above = west, below = east" rule holds for every board, including yours. Your board stays the second item, right after West's compact board.

## Neighbour labels (every board, every layout)

Every board shows a line naming its two neighbours, each with an arrow pointing to where that neighbour is **on screen**:

- Ring: e.g. Cy's board in a 7-player game shows `↖ Bob · ← Dee`. Arrows are one of the 8 directions (→ ↘ ↓ ↙ ← ↖ ↑ ↗), worked out from where the neighbour sits relative to this board.
- Narrow list: the neighbour above gets ↑ and the one below gets ↓. The last board's wrap-around neighbour gets ↑ too (West is above, at the top of the page); my call, the builder may use a clearer wrap marker such as ↥.
- Your own board's line names your West and East neighbours.

Compass words: only your two neighbours keep the **West** / **East** tag (where it matches the screen), plus **You** on your board. In 4 and 6 player games the far board may be tagged **Opposite** (shown in the mockups; optional). No other board gets a compass word.

Names use `GameFormat.player_name/2` ("You" for the viewer). The arrow line should also be readable by screen readers, e.g. `aria-label="Neighbours: Bob and Dee"`; arrows themselves `aria-hidden`.

## Compact boards and opening them

**Your board** always shows everything as today: wonder with stage strip, stats, all built cards.

**Every other board is compact by default**, on every screen size:
- header: name, status chip, AI icon, random-move badge (as today)
- the neighbour line
- wonder image with its stage strip
- the stats row (coins, military, stages, tokens; the existing `player_stats`)
- a built-card summary: one coloured chip per card category with its count (e.g. brown 3, grey 2, blue 4…), using `GameFormat.category_counts/1` and the category colours

**Opening a board's cards:** tap/click the compact board (or a clear "cards" control on it).
- **Wide screens:** the built cards open in a floating panel anchored just below that board, on top of whatever is underneath. Nothing else moves. It closes when you tap the same board again, tap outside, or press Esc. Only one panel is open at a time (opening another closes the first).
- **Narrow screens:** the board expands in place to show its built cards, pushing the boards below it down (user decision: acceptable in a scrolling list, and the neighbours above and below stay in context). This is today's `is-open` toggle behaviour. Several boards may be open at once; tapping the board again collapses it. The tapped board itself must not jump: it expands downward, and the scroll position keeps it in view.
- Inside the panel or expanded board, cards behave as today: grouped by category, the latest one glowing, tapping a card opens its explanation (`show_card`).
- Opening a board is purely client-side state, with no server round trip needed. The builder chooses how (JS commands / a colocated hook). It should survive LiveView re-renders while open, or close cleanly; it must not flicker.

Animations (`fx_layer`) that fly a card or tokens onto another player's board must still land somewhere sensible when that board is compact, e.g. the category-summary chip or the stats row.

## Using the width

- Remove the `max-w-7xl` cap on the game page container. The table grows with the window, with small side margins.
- **Maximum width is measured, not guessed.** The table stops growing at the width where the busiest case fits comfortably: 7 players, your board fully built (all 7 card categories, about 20 cards), the side boards at their natural compact width, and no horizontal scrollbar or squeezing anywhere. Beyond that width the table is centred. The builder measures this in a real browser and records the chosen value and how it was measured. A rough estimate from today's sizes (64px card tiles, 8px gaps, 7 columns, ~550px for your board, ~320px per compact board) is about 1,300–1,500px. Use it as a sanity check only.
- The top bar, replay bar and bottom dock (hand) use the same width as the table.
- The final-score card stays a compact, centred card above the table, not stretched.

## Where it applies

- Live game (all phases except wonder setup).
- Game over and the replay of a finished game (`game_live.ex` game-over branch): the same table layout, with the scoreboard/replay bar above as today.
- Past games use the same page (`GameLive`) and get it automatically.

## Breakpoints

- **Ring:** when the screen is wide enough to fit the ring for the current player count with compact side boards (roughly laptop width, `lg` and up). The builder picks the exact breakpoint, possibly per player count, and may shrink compact boards a little to fit a smaller ring.
- **Narrow list:** below that. Phones and portrait tablets.
- Today's `md` two-column grid goes away.

## Constraints

- Follow `helios/AGENTS.md` (Phoenix 1.8 / LiveView conventions, Tailwind v4, colocated hooks, `mix precommit`).
- Hidden information is unchanged: boards show only what the player's view already contains.
- `prefers-reduced-motion`: no animated opening of panels or expanded boards.
- Touch targets stay ≥44px on coarse pointers (`pointer-coarse:min-h-11`), as today.
- Keep the existing DOM ids where tests and the fx layer rely on them (`#my-board`, `#west-panel`, `#east-panel`, `section[data-player=…]`, `#my-built`, `built-cards-<id>`), or update tests and fx together. `#other-players` is expected to go away; tests that rely on it change.

## Tests (expected, details left to the builder)

- ExUnit: seat ordering for 3–7 players (ring positions and narrow order), neighbour labels/arrows per position, compact vs full boards (yours full, others compact), category-count chips.
- Playwright (`e2e/`): on a wide viewport, no board moves when another board's cards are opened and closed, nor across a turn in which you build a card (compare bounding boxes); a 7-player-sized table has no horizontal scrollbar at the measured max width; on a phone viewport, the order is W, You, E, … and tapping a board expands it in place without moving the tapped board. Update `responsive.spec.ts` and `game.spec.ts` where ids or layout changed.

## Left to the builder

- How to position boards around the ring with real, varying sizes (CSS grid with staggered rows, measured absolute positioning, SVG, …), as long as order, the oval feel and "nothing moves" hold.
- How the ring line is drawn.
- Exact breakpoints, compact board width, panel styling and close animation.
- The exact maximum width (measured as described above).
- How arrows are computed (server-side from seat index and layout, or client-side from positions).

## Decisions made during design (for review)

Taken by the designer, not explicitly confirmed by the user:
- Wide screens: only one cards panel open at a time; it closes on tap again / tap outside / Esc.
- Game-over and replay screens use the same layout; wonder setup is unchanged.
- Your hand stays in the existing sticky bottom dock.
- Compact boards have a fixed height (badges overlay rather than add rows).
- The final-score card stays compact and centred.
- The compact board's category-count chips replace today's per-card colour squares.
- The narrow list's wrap-around neighbour arrow (↑ vs a special marker).

## Measured (2026-10-22)

Measured with the Task-6 test in a real browser (`cd e2e && npx playwright test seating.spec.ts -g "fully built"`; this session ran it against a Playwright-spawned server via a session-only `playwright.4005.config.ts` because the worktree's usual 4004 server was started before these edits). Setup: 7 players, turn 1, `#my-built` injected with the worst case (7 category columns × 10 tiles = 496px of content) and all 6 compact boards' toggles filled with their 7 category chips.

- **Measured first-fit width: 1376px.** First viewport width in the 1280→2400 (step 32) sweep where the built board fits without horizontal scrolling, no two boards intersect, and every toggle chip is fully visible.
- **Chosen maximum width: 1600px** (unchanged `max-w-[96rem]` = 1536px container + 64px `lg:px-8` padding) ≥ 1376px, with headroom to spare; the spec's 1,300–1,500px estimate holds.
- **Final geometry values: `@me_reserve` 48rem, `@side_width` 22rem** — unchanged from Task 1; no ring-value tuning was needed.
- **Required sizing fix (deviation from "values only"): `xl:w-full` added to `#seats`.** Before it, `#seats` shrink-wrapped to its max-content (824px — set by the wonder image's intrinsic width through `#my-board-summary`, with `mx-auto` suppressing flex stretch), so ring geometry never changed with viewport width and *no* width in 1280..2400 could fit the worst case. With `xl:w-full` the ring fills its container up to the 96rem cap, as the percentage-based slot geometry assumes.
