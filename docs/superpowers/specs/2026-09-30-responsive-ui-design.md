# Responsive UI — play full games on a phone

Status: approved design (2026-09-30). Builds on Phase 4 (`2026-09-23-phase-4-game-design.md`). Independent of Phase 5 (release).

## Goal

A phone in portrait (360–430px wide) is a first-class way to play a complete game: log in, join a table, play all three ages, read the scoreboard. Landscape phones (≈667–932 × 360–430) must be playable; tablets and desktop use the same structure with more room.

Success criteria:
- No page scrolls horizontally at any viewport ≥ 360px wide.
- During a turn the hand is visible without scrolling, and after tapping a card every action button is on screen.
- Every interactive control is ≥ 44×44 CSS px on touch viewports.
- The existing Playwright suites pass on a phone-sized device project, including a full 3-player game.

## Audit (2026-09-30, 375×667 / 667×375 / 768×1024)

No horizontal overflow today. Problems, worst first:
1. The hand renders ~1600px down the page (below own board and both neighbour panels); tapping a card opens `#action-panel` *below* the hand, off-screen — the tap appears to do nothing.
2. Landscape phone: hand cards jump to 180×275 at `sm` (≥640px), taller than the 375px viewport.
3. Tablet (768): the three-column table starts at `lg` (1024), so tablets get the long phone column with oversized panels.
4. Built cards are 64×40 crops; neighbours' cards are anonymous colour squares.
5. Top bar wraps to three rows (age/turn, "Waiting for…", connections).
6. Lobby: fixed notifications cover the site header (including Logout).
7. Card affordances are `hover:` only; no touch feedback.

The same off-screen action panel also happens on desktop at 1080px height (hand is 275px tall).

## Decisions

- **Layout pattern: hand dock + action sheet, at every size.** One structure for phone, tablet and desktop: sticky top bar, scrolling table, hand docked at the bottom, actions in a sheet. The table reflows (1 column phone, 2 tablet, 3 from `lg`).
- **Server-driven, CSS-only.** The sheet's open state is the existing `@selected` assign; `#action-panel` already renders only while a card is selected. No JS hooks, no client-side state. Closing uses LiveView events.
- **All existing DOM ids stay** (`#hand`, `#hand-card-N`, `#action-panel`, `#build-option-N`, `#wonder-option-N`, `#build-free-button`, `#discard-button`, `#pending-choice`, `#change-choice`, `#discard-picker`, `#top-bar`, `#my-board`, `#west-panel`, `#east-panel`, `#other-players`, `#scoreboard`, `#score-row-*`, lobby/login ids, …). ExUnit and Playwright tests select by them.
- Tailwind v4 breakpoints: `sm` 640, `md` 768, `lg` 1024. A custom variant `short` = `@media (max-height: 500px)` handles landscape phones.
- Out of scope: card zoom/inspect viewer, swipe gestures, PWA/install prompts, any rule or NIF change.

## Design

### Game table (`HeliosWeb.GameLive` + `GameComponents.*`)

**Page structure.** `#game`'s content column becomes `min-h-dvh flex flex-col`: sticky `#top-bar` → table (`flex-1`) → dock as the **last child with `sticky bottom-0`**. Because the dock stays in normal flow, the table never scrolls underneath it (no padding bookkeeping), and on short pages `flex-1` pushes the dock to the bottom of the screen. The dock pads by `env(safe-area-inset-bottom)`. Game over: no dock; the scoreboard replaces the table as today. The paper background drops `bg-fixed` below `lg` (iOS Safari renders fixed backgrounds zoomed/janky).

**Top bar (`TopBar`).** Sticky `top-0`. Phones: row 1 = `Age I · Turn 3/6 · ←` plus a new `#waiting-count` ("Waiting for N", `sm:hidden`); row 2 = player chips (name, truncated, with connection dot) in a single horizontally scrolling line (`overflow-x-auto`, no wrap). From `sm` up: `#waiting-for` ("Waiting for: a, b", `hidden sm:block`, text unchanged — `top_bar_test.exs` asserts it exactly) and today's spacing. `#age-label`, `#turn-label`, `#pass-direction`, `#waiting-for`, `#connections`, `#connection-<id>` and their `data-*` attributes keep their exact content; `#waiting-count` carries the full names in `title`.

**Table.**
- Own board (`#my-board`) first on every size below `lg`. Built-card thumbnails grow (≈48×32 phone → 64×40 `sm`), still grouped by colour column, still in `#my-built` with `data-card`.
- Neighbour panels (`#west-panel`, `#east-panel`) on phones: a one-line summary (small wonder image ≈72px wide, name, coins/shields/stages, colour squares) inside a native `<details>`; expanding shows today's full panel content. From `md` up: open by default, side by side (`md:grid-cols-2`). At `lg`: today's `West | Me | East` grid (`lg:grid-cols-[1fr_2fr_1fr]`).
- Other players (`#other-players`): stays a horizontal strip; cards narrow to `min-w-48` on phones.

**Dock (`Hand.hand`, `#hand`).** `sticky bottom-0 z-30`, full width of the content column, antique background, top shadow. Contents, top to bottom:
1. The status line when relevant: `#pending-choice` ("You chose … / Change"), `#waiting-extra-turn`, `#play-last-card`. These move from the table into the dock so they are always visible.
2. The hand: one horizontal row, `overflow-x-auto`, `snap-x`, cards `snap-start`. Card sizes: 64×98 base, 88×134 `sm`, 120×183 `lg`; `short:` forces 56×86 so a landscape phone's dock is ≤ 40% of the viewport height. Selected card: ring + lift (as today); also `active:` scale feedback.
When `GameFormat.show_hand?/1` is false and there is no status line, the dock is not rendered.

**Action sheet (`Hand.action_panel`, `#action-panel`).** Rendered while `@selected_card` is set:
- A scrim (`#action-scrim`, `fixed inset-0 z-40 bg-black/35`) with `phx-click="deselect"`.
- The sheet (`fixed z-50`): phones = bottom sheet (`inset-x-0 bottom-0`, `rounded-t-2xl`, `max-h-[85dvh] overflow-y-auto`, safe-area padding); `lg` = centred modal panel (`max-w-xl`, vertically and horizontally centred, `rounded-2xl`).
- Content: grab handle (decorative) and close button `#close-action-panel` (`phx-click="deselect"`, `aria-label="Close"`); card image (≈96×147 phone, 120×183 `sm`+, now shown on phones too); name; option groups with full-width buttons on phones (`w-full sm:w-auto`); Discard last.
- `phx-window-keydown="deselect"` with `phx-key="Escape"` on the sheet.
- Tapping the selected card in the dock still toggles it off (existing `select_card` behaviour).
- `role="dialog"`, `aria-modal="true"`, `aria-labelledby` the card name heading.

**New event.** `GameLive.handle_event("deselect", _params, socket)` → `assign_selection(socket, nil)`. Idempotent (no-op when nothing is selected).

**Halikarnassós discard picker (`ExtraTurn`, `#discard-picker`).** Uses the same sheet styling but **no scrim close, no close button, no Escape** — the choice is mandatory. Cards in a 3-column grid on phones (`grid-cols-3`, card width 100%), `sm:flex sm:flex-wrap` with 120×183 cards above. `max-h-[85dvh] overflow-y-auto`.

**Touch.** Every button and link in the game table is ≥ 44px tall on touch viewports (`min-h-11`). Every `hover:` affordance gets a matching `active:` state.

### Shared layout (`HeliosWeb.Layouts`, `root.html.heex`)

- `min-h-screen` → `min-h-dvh` (root body, `Layouts.app`, login, game).
- Viewport meta: `width=device-width, initial-scale=1, viewport-fit=cover`.
- Site header (`#site-header`): `px-3 py-2 sm:px-6 sm:py-3`; name `truncate min-w-0` with `text-base sm:text-lg`; `#my-table-link` and `#logout-link` `min-h-11` with centred content.
- Notifications (`#notifications`): below `sm`, in normal flow directly under the header (`static`, full width); from `sm`, today's fixed overlay (`sm:fixed sm:inset-x-0 sm:top-4`). Message text wraps; Accept/Decline/OK `min-h-11`.
- Flash group (`#flash-group`): `fixed inset-x-2 top-2` below `sm`; `sm:inset-x-auto sm:right-4 sm:top-4 sm:w-96` above.

### Lobby (`LobbyLive`, `LobbyGamePanel`)

- Invite form (`#invite-form`): `flex-col sm:flex-row`; select and `#invite-button` full width below `sm`; drop the `mb-2` alignment hack on phones.
- `#game-in-progress` and the Start row wrap (`flex-wrap`); `#rejoin-game` and `#start-game` `min-h-11`.
- `#uninvite-*`: `size-11 sm:size-8`.

### Login (`LoginLive`)

- `min-h-dvh`; background must cover the whole viewport on phones. The audit screenshot (375×667, `isMobile`) showed thin black bars at the top and bottom; reproduce at 360×740 and 390×844, find the cause, and fix it so the image reaches all four edges.
- Title `text-4xl sm:text-5xl` so "7 WONDERS" never wraps at 360px.

### Scoreboard (`GameComponents.Scoreboard`)

- Table and columns unchanged. Card padding `p-3 sm:p-6`.
- Player column sticky (`sticky left-0` with the row's background and a right shadow) while the score columns scroll inside the existing `overflow-x-auto`; a right-edge fade hints at more columns on phones.
- `#back-to-lobby` full width below `sm`, `min-h-11`.

## Testing

### Playwright (`e2e/`)

Projects in `playwright.config.ts`:
- `chromium` — `devices["Desktop Chrome"]` (unchanged), ignores `responsive.spec.ts`.
- `mobile` — `devices["Pixel 7"]` (412×915, `isMobile`, `hasTouch`, chromium engine so CI installs nothing new), ignores `responsive.spec.ts`. Runs every existing spec, including the full 3-player game to the scoreboard.
- `responsive` — `testMatch: /responsive\.spec\.ts/`, desktop base; the spec sets viewports itself.
- `workers: 1` stays (SQLite contention).

Context creation: every manual `browser.newContext()` (support `lobby.ts`, `game.ts`, `auth.spec.ts`) goes through one helper in `tests/support/` that explicitly passes the current project's device options (`viewport`, `deviceScaleFactor`, `isMobile`, `hasTouch`, `userAgent`) from `test.info().project.use`, merged with per-call overrides. A guard test asserts the `mobile` project really runs at 412px wide so a silent desktop fallback fails.

`responsive.spec.ts` — for each viewport in `360×740`, `390×844`, `740×360` (landscape), `768×1024`, `1280×800`, one 3-player table (all three players at that viewport; touch enabled when width < 1024):
- No horizontal scroll (`documentElement.scrollWidth <= innerWidth`) on login, lobby, game table, game with sheet open.
- `#hand` fully in viewport without scrolling (`toBeInViewport({ ratio: 1 })`); landscape: dock height ≤ 40% of viewport height.
- Tap a hand card → `#action-panel` and each of its buttons fully in viewport; scrim tap, `#close-action-panel` and Escape each close it (`#action-panel` count 0).
- Scrolled to the bottom, the last table panel's bottom edge ≤ the dock's top edge.
- On touch viewports: action-sheet buttons, `#change-choice`, `#my-table-link`, `#logout-link`, `#uninvite-*`, notification buttons are ≥ 44×44.
- Phones: `#top-bar` height ≤ 2 text rows (assert ≤ 96px); `#waiting-count` visible and `#waiting-for` hidden.

`game.spec.ts`'s full game additionally asserts no horizontal scroll on `#scoreboard`; since it runs in `chromium` and `mobile`, the scoreboard is checked at phone size without another full game.

A shared helper `expectNoHorizontalScroll(page)` lives in `tests/support/`.

### ExUnit (`helios/`)

- `deselect` via scrim click, close button and Escape keydown clears the selection and removes `#action-panel`; `deselect` with nothing selected is a no-op.
- `#discard-picker` has no `#close-action-panel`, no scrim and no `phx-window-keydown`.
- Pending-choice and extra-turn notices render inside `#hand`.
- `#waiting-count` shows "Waiting for N" with the names in `title`; `#waiting-for` text is unchanged.
- Existing component/LiveView tests pass unchanged (ids preserved).

### Manual

Before declaring done: screenshot login, lobby, game (turn, sheet open, discard picker if reachable) and scoreboard at 360×740, 740×360 and 1280×800 and review them visually.

### Commands

`cd helios && mix precommit`; `cd e2e && npx playwright test` (all three projects).
