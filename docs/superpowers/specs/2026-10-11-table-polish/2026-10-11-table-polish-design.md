# Table polish — phone status bar, military icon, discard replay, replay position, recent tables, invite auto-accept

Status: approved design (2026-10-11). Builds on the game table UX initiative (`docs/superpowers/specs/2026-10-04-game-table-ux/2026-10-04-game-table-ux-design.md`). The companion spec `2026-10-11-wonder-modes-design.md` (same folder) covers the board-selection modes added in the same session.

## Goal

Fix the rough edges found while playing on phones and replaying games: the status bar says who you pass to and who you wait for, the military icon matches the cards, the Halikarnassós discard turn is visible and animated, a refresh keeps your place in a replay, friends' tables are one click away, and arriving at a table you were invited to clears the invite.

Success criteria:
- On a 375px-wide phone the full "Pass to ‹name›" is readable and tapping "Waiting for N" lists the names.
- The military icon is recognisably the card symbol (round red shield, crossed swords) at `size-3.5`.
- In a replay, the player who built from the discard pile sees the cards they could choose from; nobody else does.
- A card built from the discard pile flies onto the board (live and in replay); other players see it leave a discard-pile marker.
- Reloading a replay page returns to the same frame.
- A "Recent tables" header menu lists up to 5 tables the user can still enter.
- Opening a table with a pending invite accepts it and removes the notification in every tab.
- All existing ExUnit and Playwright suites pass (updated where ids/text change), plus the new tests below.

## Requested changes → where they land

| # | Request | Section |
|---|---|---|
| 1 | More lines for the status banner on phones; can't see who I pass to | A |
| 2 | Phones only show "Waiting for N"; tapping should show who | A |
| 3 | Military symbol should be the one on the military cards | B |
| 4 | Discard-pile extra turn: chooser can't see the options in history; no card animation | C |
| 5 | Refreshing a replay jumps to the end | D |
| 6 | Menu item for previous tables | E |
| 7 | Entering the inviting table should count as accepting the invite | F |

## Decisions

- **Helios only, no rule change.** Item 4 reuses the `discard_pile` the engine already puts in the chooser's view. `ENGINE_VERSION` is unchanged.
- **Replay secrecy = live secrecy** (unchanged principle). The chooser saw the buildable discard cards live, so their own replay frame may contain them. The 2026-10-04 spec's "discarded cards are never sent" is refined to "never sent to anyone who did not see them live".
- **One migration:** `lobby_invites.last_visited_at` (E).
- **Replay frame lives in the URL** (`?frame=N`), not in the session or localStorage.
- **JS:** colocated hooks only; animations skipped under `prefers-reduced-motion`.

## A — Phone status bar (`top_bar.ex`)

Layout below `sm` becomes three rows:

1. `#age-label` · `#history-button` · `#turn-label` · waiting button (right-aligned).
2. New row: `#pass-direction` — direction icon + "Pass to ‹full name›". The name wraps (`break-words`, no `truncate`/`max-w-24`); the "Pass to" prefix is visible on phones too. The discard-pile marker (C) sits at the right of this row. On game over the row holds only the marker (or is omitted if the marker is absent).
3. `#connections` strip (unchanged).

`sm` and up: unchanged single wrapping row, plus the discard-pile marker.

The phone bar's height budget grows from 96px to about 120px; the "row one stays single-line" test and `e2e/tests/responsive.spec.ts` ("game table") are updated to the new budget. Row 1 stays single-line.

**Waiting button.** `#waiting-count` becomes a `<button>` (`aria-haspopup="dialog"`, `aria-controls="waiting-list"`) toggling a panel `#waiting-list` with one `<li>` per waiting player name. Same pattern as the stage popover (`stage_strip.ex`): `JS.toggle`, `phx-click-away` and Escape close it, no server state. Touch target `pointer-coarse:min-h-11`. Desktop keeps `#waiting-for`.

**Replay fix.** In replay the top bar receives `waiting: []` explicitly (today extra-turn frames wrongly show "Waiting for 1"). Implemented by a `replay` attr on `top_bar` (or by passing the computed list), not by changing `GameFormat.waiting_for/2`.

## B — Military icon (`icons.ex`)

`military_icon/1` keeps its name, attrs and `data-icon="military"`; only the SVG changes, modelled on the card symbol:

- Round shield: dark grey/black rim, red face (`#b91c1c` family).
- A thin inner ring of short light marks standing in for the Greek-key border.
- Two crossed swords (light grey blades, darker hilts) drawn corner to corner over the shield.

It must read at `size-3.5` (14px): no detail thinner than ~1px at that size; the inner ring is a single dashed stroke. `icons_test.exs` is updated (it asserts structure, e.g. a circle and two sword paths, not exact coordinates).

## C — Discard extra turn in replay, discard-pile marker, animation

### Discard-pile marker
`#discard-pile` in the top bar: a small stacked-card-back glyph plus `view.discard_count`. Shown whenever `discard_count > 0` (live and replay). Public information. It is the fly origin for observers.

### Replay frames
`Games.frame_snapshot/2` (`helios/lib/helios/games.ex`) additionally keeps:
- `discard_pile: view.discard_pile` — non-nil only in the chooser's own `ExtraTurn{BuildFromDiscard}` frame, because the engine view only fills it for that player.
- `discard_count: view.discard_count`.

The replay dock (`game_live.ex`) renders, when `current_frame.discard_pile` is a non-empty list, a read-only `.replay_discard` grid (new function in `extra_turn.ex`, styled like `replay_hand`): one card per name, the card in `my_choice` (`{:build_from_discard, card}`) highlighted as "chosen". Id: `#replay-discard`, cards `#replay-discard-<slug>`.

### Animation
`TurnFx.payload` (`turn_fx.ex`) for `:built_from_discard` adds a `from` field:
- the acting player sees `from: "discard-card-<slug>"` (rect taken from the picker/replay grid),
- everyone else sees `from: "discard-pile"`.

Rects: the live discard picker and `#replay-discard` get a hook (reusing `.HandRects` or a sibling `.DiscardRects`) that snapshots card rects into `window.sevenWondersDiscardRects` on mount/beforeUpdate, because the picker is gone by the time the turn resolves. `.TurnFx` (`fx.ex`) `fly`: when `from` is set it looks up that id in the discard rects, falling back to the live element's rect (`#discard-pile`), and flies a ghost of the card image to the target tile, then the target tile does its normal `arrive`. If no origin rect is found it falls back to today's `arrive` only. Reduced motion: skipped.

## D — Replay position in the URL (`game_live.ex`)

- Add `handle_params/3`. `?frame=N` (integer) selects the frame, clamped to `0..last`; missing or invalid → last frame (today's behaviour).
- Every frame change (`replay_first/prev/next/last/goto`, autoplay ticks, history-drawer links) goes through one function that `push_patch(to: ~p"/game/#{id}?frame=#{i}", replace: true)`; `handle_params` performs the actual `goto`. The final frame drops the param (`/game/:id`).
- Autoplay state and `show_scores` are not persisted (a reload pauses).
- The server-side `refresh/1` keeps the frame as today.

## E — Recent tables menu

### Data
Migration: `alter table(:lobby_invites), add :last_visited_at, :utc_datetime` (nullable). Set by `Lobbies.touch_visit(scope, lobby_id)`, called from `LobbyLive` mount (connected) for non-owners.

`Lobbies.recent_tables(%User{}) :: [%{lobby_id, owner_name, last_active_at}]`:
- candidate lobbies = those where the user has an `accepted` invite (so every entry is enterable; own lobby excluded — it has "My table"),
- `last_active_at` = the greatest of `invite.last_visited_at` and the latest `games.inserted_at` for games in that lobby where the user has a `game_players` row,
- order by `last_active_at` desc (nulls last), limit 5.
Portable Ecto query (no SQLite-only SQL); the max-of-two may be done in Elixir after fetching the per-lobby game maximum with a grouped subquery.

### UI (`layouts.ex` `site_header/1`)
A "Recent tables" button `#recent-tables-button` next to `#my-table-link` / `#past-games-link`, toggling `#recent-tables-menu` (`JS.toggle`, click-away, Escape). Entries `#recent-table-<lobby_id>` link to `/lobby/<id>` with the owner's name ("‹Owner›'s table"). On phones the button shows only an icon (`hero-user-group`) with an sr-only label. Hidden when the list is empty. The list is loaded for logged-in users in the same on_mount that loads notifications (`HeliosWeb.Notifications` or a sibling hook) and refreshed on `{:invite_resolved}` / `{:uninvited, _}`.

## F — Arriving at a table accepts its invite

`LobbyLive.mount` (connected, authorized, not the owner): if the user's invite for this lobby is `pending`, call `Lobbies.accept/2`. It already broadcasts `{:invite_resolved, lobby_id}` on `user:<id>`, which `HeliosWeb.Notifications` handles by removing `invite-<lobby_id>` — including in the current LiveView, which is subscribed by then. `GameLive.mount` does the same for the game's `lobby_id` (a player opening a game directly from that table also "arrives"). Decline is unchanged.

## Testing

- `top_bar_test.exs`: phone row 2 contains the full name without `truncate`; `#waiting-count` is a button controlling `#waiting-list`, which lists the names; replay passes no waiting; `#discard-pile` shows the count and is absent at 0.
- `icons_test.exs`: new SVG structure.
- `replay_test.exs`: the chooser's extra-turn frame has `discard_pile` = buildable names; **every other player's frames have `discard_pile: nil`**; `discard_count` is present.
- `game_live_test.exs` (replay): `#replay-discard` rendered for the chooser with the chosen card marked, absent for others; `?frame=N` mounts on frame N; out-of-range/garbage clamps or falls back to last; stepping patches the URL.
- `turn_fx_test.exs`: `from` is `discard-card-<slug>` for the actor, `discard-pile` for others.
- `lobbies_test.exs`: `touch_visit`, `recent_tables` ordering, limit, own lobby excluded, uninvited/pending lobbies excluded, played-only and visited-only lobbies both appear.
- `layouts_test.exs`: menu rendered with entries; hidden when empty.
- `notifications_test.exs` / `lobby_live_test.exs`: opening the lobby with a pending invite accepts it and removes `#accept-invite-<id>` in the current and another tab; same via `GameLive`.
- e2e: `responsive.spec.ts` new phone header budget; tapping the waiting button shows the names.

## Out of scope

- Showing the discard pile's contents to anyone who didn't see them live.
- Persisting autoplay or the "final scores" toggle across reloads.
- Removing tables from the recent list manually.
