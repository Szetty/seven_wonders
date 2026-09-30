# Responsive UI Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make Helios fully playable on phones (360–430px portrait, landscape too) with one layout pattern at every size — sticky top bar, scrolling table, hand docked at the bottom, actions in a sheet — and cover small screens in Playwright.

**Architecture:** Server-driven and CSS-only. The action sheet's open state is the existing `@selected` assign in `HeliosWeb.GameLive`; closing it is a new `deselect` LiveView event (scrim click, ✕ button, Escape). Layout changes are Tailwind v4 utility classes plus one custom variant (`short`, landscape phones) and the built-in `pointer-coarse:` variant for 44px touch targets. No JS hooks, no client state, every existing DOM id kept. Playwright gains a `mobile` project (Pixel 7) that replays every existing spec at phone size and a `responsive` project that checks layout rules over a viewport matrix.

**Tech Stack:** Phoenix 1.8 / LiveView 1.1.30, Tailwind CSS 4.1.12, ExUnit + `Phoenix.LiveViewTest` + LazyHTML, Playwright 1.63 (chromium only).

**Spec:** `docs/superpowers/specs/2026-09-30-responsive-ui/2026-09-30-responsive-ui-design.md` (read it first; this plan argues from it).

## Global Constraints

- Every existing DOM id and `data-*` attribute keeps its exact content: `#hand`, `#hand-card-N`, `#action-panel`, `#build-options`, `#wonder-options`, `#build-option-N`, `#wonder-option-N`, `#build-unavailable`, `#wonder-unavailable`, `#build-free-button`, `#discard-button`, `#pending-choice`, `#change-choice`, `#discard-picker`, `#discard-pick-N`, `#waiting-extra-turn`, `#play-last-card`, `#top-bar` (`data-phase`, `data-turn-key`), `#age-label`, `#turn-label`, `#pass-direction`, `#waiting-for`, `#connections`, `#connection-<id>` (`data-connected`), `#my-board`, `#my-built`, `#wonder-stages`, `#stage-N`, `#built-<slug>`, `#west-panel`, `#east-panel`, `#other-players`, `#player-<id>`, `#scoreboard`, `#score-row-<id>` (`data-rank`), `#back-to-lobby`, `#site-header`, `#current-user-name`, `#my-table-link`, `#logout-link`, `#notifications`, `#notification-*`, `#accept-invite-*`, `#decline-invite-*`, `#dismiss-*`, `#flash-group`, `#lobby`, `#lobby-title`, `#invite-form`, `#invite_user_id`, `#invite-button`, `#game-panel`, `#game-in-progress`, `#rejoin-game`, `#start-game`, `#start-blocker`, `#members-table`, `#member-*`, `#uninvite-*`, `#free-slot-*`, `#login-title`, `#login-card`, `#login_form`, `#login-submit`, `#login-error`. Existing tests select by them.
- Breakpoints: Tailwind defaults `sm` 640, `md` 768, `lg` 1024. Landscape phones: custom variant `short` = `@media (max-height: 500px)`.
- Touch targets: every button/link ≥ 44×44 CSS px on touch screens via `pointer-coarse:min-h-11` (icon-only buttons `pointer-coarse:size-11`); desktop sizes unchanged. Every `hover:` effect gets a matching `active:` state.
- Tailwind rules from `helios/AGENTS.md`: keep the `@import "tailwindcss" source(none)` / `@source` header of `app.css`; no `@apply`; no daisyUI; no inline `<script>`; no JS hooks (none are needed).
- HEEx class attributes with more than one conditional part use list syntax `class={[...]}`.
- No game-rule, NIF, `core/` or `ENGINE_VERSION` changes.
- Stage explicit paths only (never `git add -A` / `git add .`). One commit per task (Task 1 may commit twice).
- Toolchain comes from `mise.toml`. If `mix`/`node` are not on PATH in your shell, prefix commands with `mise exec --`.
- The e2e server runs with `code_reloader: false` and Playwright reuses a server already listening on port 4004 locally. **Before every Playwright run, stop any server on 4004** (`lsof -ti :4004 | xargs kill 2>/dev/null; true`) so Playwright rebuilds assets and restarts Helios with your changes.
- Checks before a task is done: `cd helios && mix precommit` (compile with warnings as errors, format, full test suite) and the Playwright runs named in the task.

## Review Focus

1. **24-character usernames on a 360px phone** (the maximum `uniqueName` produces): the site header, top-bar chips, neighbour summaries and lobby must truncate, never push Logout or a button off-screen, and never make the page scroll sideways. Pinned by the `LONG_NAME_PREFIX` players in Tasks 2, 3, 4 and 5.
2. **Seven players on a 360px phone:** six connection chips and four "other players" cards must scroll inside their own strips, not widen the page, and the top bar stays ≤ 96px. Pinned by the seven-player test in Task 5.
3. **Landscape phone (≤ 500px tall) with the action sheet open:** the sheet must fit in the viewport and scroll internally so Discard is always reachable. Pinned in Task 7 (landscape branch).
4. **Keys other than Escape, and stray `deselect` events,** must not close the sheet or crash; `deselect` with nothing selected is a no-op. Pinned by ExUnit and e2e in Task 7.
5. **State that must survive:** an expanded neighbour panel stays expanded across LiveView updates (another player submitting), and rotating a phone mid-turn keeps the dock on screen. Pinned in Task 5 (patch) and Task 6 (rotation).

---

## File Structure

| File | Change | Responsibility |
|---|---|---|
| `e2e/playwright.config.ts` | Modify | `chromium`, `mobile` (Pixel 7), `responsive` projects |
| `e2e/tests/support/layout.ts` | Create | Viewport matrix, layout assertions (`expectViewport`, `expectNoHorizontalScroll`, `boxOf`, `expectTapTargets`) |
| `e2e/tests/support/game.ts` | Modify | `setupTable` name prefix parameter; `closeAll` |
| `e2e/tests/device.spec.ts` | Create | Guard: the `mobile` project really is a 412px touch screen |
| `e2e/tests/responsive.spec.ts` | Create | Layout rules per viewport (grown task by task) |
| `e2e/tests/game.spec.ts` | Modify | Close the sheet before switching cards; scoreboard fits |
| `helios/assets/css/app.css` | Modify | `short` custom variant |
| `helios/lib/helios_web/components/layouts/root.html.heex` | Modify | `viewport-fit=cover`, `min-h-dvh` |
| `helios/lib/helios_web/components/layouts.ex` | Modify | Compact header, in-flow notifications on phones, flash position |
| `helios/lib/helios_web/live/login_live.ex` | Modify | `min-h-dvh`, title size |
| `helios/priv/static/images/7_wonders.jpg` | Modify | Crop the baked-in 21–22px black frame |
| `helios/lib/helios_web/live/lobby_live.ex` | Modify | Stacked invite form, touch-size remove buttons |
| `helios/lib/helios_web/components/lobby_game_panel.ex` | Modify | Wrapping banner, touch-size Start/Rejoin |
| `helios/lib/helios_web/components/game_components/top_bar.ex` | Modify | Two-row phone bar, `#waiting-count`, scrolling chips |
| `helios/lib/helios_web/components/game_components/player_panels.ex` | Modify | Neighbour summary toggle, narrower others strip |
| `helios/lib/helios_web/components/game_components/board.ex` | Modify | Grid spans, padding |
| `helios/lib/helios_web/live/game_live.ex` | Modify | Page structure, `#dock`, `deselect` event |
| `helios/lib/helios_web/game_format.ex` | Modify | `dock?/1`, `sheet_class/0` |
| `helios/lib/helios_web/components/game_components/extra_turn.ex` | Modify | Split into `extra_turn_notice/1` + `discard_picker/1`; picker as sheet |
| `helios/lib/helios_web/components/game_components/hand.ex` | Modify | Dock hand sizes, action sheet, compact pending banner |
| `helios/lib/helios_web/components/game_components/scoreboard.ex` | Modify | Sticky player column, fade, full-width button |
| `helios/test/helios_web/live/login_live_test.exs` | Modify | Viewport meta |
| `helios/test/helios_web/components/game_components/*_test.exs` | Modify | Component expectations |
| `helios/test/helios_web/live/game_live_test.exs`, `game_live_extra_turns_test.exs` | Modify | Dock and sheet behaviour |
| `helios/test/helios_web/game_format_test.exs` | Modify | `dock?/1` |

---

### Task 1: Playwright device projects and layout helpers

**Files:**
- Modify: `e2e/playwright.config.ts`
- Create: `e2e/tests/support/layout.ts`
- Create: `e2e/tests/device.spec.ts`
- Create: `e2e/tests/responsive.spec.ts`
- Modify: `e2e/tests/support/game.ts`

**Interfaces:**
- Produces (`e2e/tests/support/layout.ts`):
  - `type Viewport = { name: string; width: number; height: number; touch: boolean }`
  - `const VIEWPORTS: Viewport[]` — `small-android` 360×740, `iphone` 390×844, `landscape-phone` 740×360, `tablet` 768×1024 (all touch), `desktop` 1280×800 (no touch)
  - `const LONG_NAME_PREFIX = "longplayername"`
  - `expectViewport(page: Page, vp: Viewport): Promise<void>`
  - `expectNoHorizontalScroll(page: Page): Promise<void>`
  - `boxOf(locator: Locator): Promise<{ x: number; y: number; width: number; height: number }>`
  - `expectTapTargets(locator: Locator): Promise<void>`
- Produces (`e2e/tests/support/game.ts`): `setupTable(browser: Browser, count: number, prefix = "g")`, `closeAll(players: Player[]): Promise<void>`.

Background: in Playwright 1.63, contexts made with a bare `browser.newContext()` inherit the project's and `test.use`'s device options (checked 2026-09-30: a Pixel 7 project gave 412px, `pointer: coarse` and an Android UA in both fixture and manual contexts). The existing helpers therefore need no change; the guard test below keeps that true.

- [ ] **Step 1: Add the projects**

In `e2e/playwright.config.ts` replace the `projects` line with:

```ts
  projects: [
    {
      name: "chromium",
      use: { ...devices["Desktop Chrome"] },
      testIgnore: /responsive\.spec\.ts/,
    },
    {
      // Every existing spec again on a phone-sized touch screen (chromium engine,
      // so CI needs no extra browser).
      name: "mobile",
      use: { ...devices["Pixel 7"] },
      testIgnore: /responsive\.spec\.ts/,
    },
    {
      // Layout rules over a viewport matrix; the spec sets viewports itself.
      name: "responsive",
      use: { ...devices["Desktop Chrome"] },
      testMatch: /responsive\.spec\.ts/,
    },
  ],
```

- [ ] **Step 2: Write the layout helpers**

Create `e2e/tests/support/layout.ts`:

```ts
import { expect, type Locator, type Page } from "@playwright/test";

export type Viewport = { name: string; width: number; height: number; touch: boolean };

/** The viewport matrix from the responsive UI spec. `touch` means a coarse pointer. */
export const VIEWPORTS: Viewport[] = [
  { name: "small-android", width: 360, height: 740, touch: true },
  { name: "iphone", width: 390, height: 844, touch: true },
  { name: "landscape-phone", width: 740, height: 360, touch: true },
  { name: "tablet", width: 768, height: 1024, touch: true },
  { name: "desktop", width: 1280, height: 800, touch: false },
];

/** With this prefix `uniqueName` returns the maximum 24 characters. */
export const LONG_NAME_PREFIX = "longplayername";

/** Fails if the page's context is not the viewport the test asked for (e.g. a silent desktop fallback). */
export async function expectViewport(page: Page, vp: Viewport): Promise<void> {
  const env = await page.evaluate(() => ({
    width: window.innerWidth,
    height: window.innerHeight,
    coarse: window.matchMedia("(pointer: coarse)").matches,
  }));
  expect(env, "context does not match the requested viewport").toEqual({
    width: vp.width,
    height: vp.height,
    coarse: vp.touch,
  });
}

/**
 * Compares against the configured viewport width, not `innerWidth`: with `isMobile`
 * an overflowing page zooms out and `innerWidth` grows with it.
 */
export async function expectNoHorizontalScroll(page: Page): Promise<void> {
  const width = page.viewportSize()!.width;
  const scrollWidth = await page.evaluate(() => document.documentElement.scrollWidth);
  expect(scrollWidth, "page scrolls horizontally").toBeLessThanOrEqual(width);
}

export async function boxOf(locator: Locator) {
  const box = await locator.boundingBox();
  expect(box, `${locator} has no layout box`).not.toBeNull();
  return box!;
}

/** Every matched element must be at least 44×44 CSS px. */
export async function expectTapTargets(locator: Locator): Promise<void> {
  const elements = await locator.all();
  expect(elements.length, `${locator} matched nothing`).toBeGreaterThan(0);
  for (const element of elements) {
    const box = await boxOf(element);
    expect(box.height, `${element} height`).toBeGreaterThanOrEqual(44);
    expect(box.width, `${element} width`).toBeGreaterThanOrEqual(44);
  }
}
```

- [ ] **Step 3: Extend the game support helpers**

In `e2e/tests/support/game.ts` replace `setupTable` with:

```ts
export async function setupTable(browser: Browser, count: number, prefix = "g") {
  const players: Player[] = [];
  for (let i = 0; i < count; i++) players.push(await newPlayer(browser, `${prefix}${i}`));
  const [owner, ...guests] = players;
  for (const guest of guests) await inviteAndAccept(owner, guest);
  return { owner, guests, players };
}

export async function closeAll(players: Player[]): Promise<void> {
  for (const player of players) await player.context.close();
}
```

- [ ] **Step 4: Write the guard and the first responsive test**

Create `e2e/tests/device.spec.ts`:

```ts
import { expect, test } from "@playwright/test";

test("the mobile project runs on a 412px touch screen, in fixture and manual contexts", async ({
  page,
  browser,
}) => {
  test.skip(test.info().project.name !== "mobile", "guards the mobile project only");
  const manual = await browser.newContext();
  try {
    for (const target of [page, await manual.newPage()]) {
      await target.goto("/login");
      const env = await target.evaluate(() => ({
        width: window.innerWidth,
        coarse: window.matchMedia("(pointer: coarse)").matches,
      }));
      expect(env).toEqual({ width: 412, coarse: true });
    }
  } finally {
    await manual.close();
  }
});
```

Create `e2e/tests/responsive.spec.ts`:

```ts
import { expect, test } from "@playwright/test";
import { VIEWPORTS, expectNoHorizontalScroll, expectViewport } from "./support/layout";

for (const vp of VIEWPORTS) {
  test.describe(`${vp.name} ${vp.width}x${vp.height}`, () => {
    test.use({
      viewport: { width: vp.width, height: vp.height },
      hasTouch: vp.touch,
      isMobile: vp.touch,
    });

    test("login fits", async ({ page }) => {
      await page.goto("/login");
      await expectViewport(page, vp);
      await expectNoHorizontalScroll(page);
      await expect(page.locator("#login-submit")).toBeVisible();
    });
  });
}
```

- [ ] **Step 5: Run the new specs**

Run: `lsof -ti :4004 | xargs kill 2>/dev/null; cd e2e && npx playwright test device.spec.ts responsive.spec.ts`
Expected: PASS. `device.spec.ts` runs in `mobile` and is skipped in `chromium`; `responsive.spec.ts` runs 5 "login fits" tests in `responsive` only.

- [ ] **Step 6: Commit**

```bash
git add e2e/playwright.config.ts e2e/tests/support/layout.ts e2e/tests/support/game.ts e2e/tests/device.spec.ts e2e/tests/responsive.spec.ts
git commit -m "test(e2e): add mobile and responsive Playwright projects with layout helpers"
```

- [ ] **Step 7: Baseline the existing suite at phone size**

Run: `lsof -ti :4004 | xargs kill 2>/dev/null; cd e2e && npx playwright test --project=mobile`
Expected: PASS (Playwright scrolls to off-screen controls). The only failure the audit predicts is a click on `#logout-link` intercepted by a fixed notification (`#notifications`), which Task 2 fixes. If that happens, leave it failing and name the failing test in the Task 2 commit message. Any other failure: stop and investigate with superpowers:systematic-debugging before continuing.

---

### Task 2: Shared layout — viewport, header, notifications, flash

**Files:**
- Modify: `helios/assets/css/app.css`
- Modify: `helios/lib/helios_web/components/layouts/root.html.heex`
- Modify: `helios/lib/helios_web/components/layouts.ex`
- Test: `helios/test/helios_web/live/login_live_test.exs`
- Test: `e2e/tests/responsive.spec.ts`

**Interfaces:**
- Consumes: `VIEWPORTS`, `LONG_NAME_PREFIX`, `expectViewport`, `expectNoHorizontalScroll`, `boxOf`, `expectTapTargets` (Task 1); `newPlayer` (`e2e/tests/support/game.ts`), `notification` (`e2e/tests/support/lobby.ts`).
- Produces: the `short:` Tailwind variant, available to every template from here on.

- [ ] **Step 1: Write the failing ExUnit test**

Add to `helios/test/helios_web/live/login_live_test.exs` (inside the module; the file already uses `HeliosWeb.ConnCase`, which provides `conn`):

```elixir
  test "the root layout lets pages extend under phone notches and home bars", %{conn: conn} do
    doc = conn |> get(~p"/login") |> html_response(200) |> LazyHTML.from_document()

    assert doc |> LazyHTML.query("meta[name='viewport']") |> LazyHTML.attribute("content") ==
             ["width=device-width, initial-scale=1, viewport-fit=cover"]
  end
```

- [ ] **Step 2: Write the failing e2e test**

In `e2e/tests/responsive.spec.ts` change the imports to:

```ts
import { expect, test } from "@playwright/test";
import { newPlayer } from "./support/game";
import {
  LONG_NAME_PREFIX,
  VIEWPORTS,
  boxOf,
  expectNoHorizontalScroll,
  expectTapTargets,
  expectViewport,
} from "./support/layout";
import { notification } from "./support/lobby";
```

and add this test after "login fits", inside the `describe`:

```ts
    test("lobby header and notifications fit", async ({ browser }) => {
      const owner = await newPlayer(browser, LONG_NAME_PREFIX);
      const guest = await newPlayer(browser, LONG_NAME_PREFIX);
      const page = owner.page;
      await expectViewport(page, vp);

      const note = notification(page, `User ${guest.name} got online!`);
      await expect(note).toBeVisible();
      await expectNoHorizontalScroll(page);
      await expect(page.locator("#logout-link")).toBeInViewport({ ratio: 1 });
      await expect(page.locator("#my-table-link")).toBeInViewport({ ratio: 1 });

      if (vp.width < 640) {
        // Phones: notifications sit under the header instead of covering it.
        const header = await boxOf(page.locator("#site-header"));
        expect((await boxOf(note)).y).toBeGreaterThanOrEqual(header.y + header.height);
      }
      if (vp.touch) {
        await expectTapTargets(page.locator("#my-table-link, #logout-link"));
        await expectTapTargets(note.getByRole("button"));
      }

      for (const player of [owner, guest]) await player.context.close();
    });
```

- [ ] **Step 3: Run both to verify they fail**

Run: `cd helios && mix test test/helios_web/live/login_live_test.exs`
Expected: FAIL — content is `width=device-width, initial-scale=1`.

Run: `lsof -ti :4004 | xargs kill 2>/dev/null; cd e2e && npx playwright test --project=responsive -g "lobby header"`
Expected: FAIL on phones (notification overlaps the header) and on touch viewports (tap targets under 44px).

- [ ] **Step 4: Add the `short` variant**

In `helios/assets/css/app.css`, directly after the three `@custom-variant phx-…` lines, add:

```css
/* Landscape phones: too short for full-size cards and a sticky top bar. */
@custom-variant short (@media (max-height: 500px));
```

- [ ] **Step 5: Update the root layout**

In `helios/lib/helios_web/components/layouts/root.html.heex`:
- viewport meta → `<meta name="viewport" content="width=device-width, initial-scale=1, viewport-fit=cover" />`
- body class `min-h-screen` → `min-h-dvh`

- [ ] **Step 6: Update `Layouts.app`, `notifications/1`, `site_header/1`, `flash_group/1`**

In `helios/lib/helios_web/components/layouts.ex`:

`app/1` body:

```heex
    <div class="min-h-dvh">
      <div :if={@current_scope && @current_scope.user} class="px-2 pt-2 sm:px-6 sm:pt-4 lg:px-8">
        <.site_header current_scope={@current_scope} />
        <.notifications items={@notifications} />
      </div>

      <main class="w-full">
        {render_slot(@inner_block)}
      </main>

      <.flash_group flash={@flash} />
    </div>
```

`notifications/1` — the container and each item become (buttons keep their ids, events and colours; only classes change):

```heex
    <div
      id="notifications"
      aria-live="polite"
      class="flex flex-col sm:pointer-events-none sm:fixed sm:inset-x-0 sm:top-4 sm:z-50 sm:items-center sm:gap-2 sm:px-4"
    >
      <div
        :for={n <- HeliosWeb.Notifications.visible(@items)}
        id={"notification-#{n.id}"}
        data-notification={Atom.to_string(n.kind)}
        class="pointer-events-auto mt-2 flex w-full flex-wrap items-center justify-between gap-x-4 gap-y-2 rounded-lg bg-teal-600 px-4 py-2 text-white shadow-lg ring-1 ring-teal-900/30 transition sm:mt-0 sm:max-w-xl"
      >
        <span class="min-w-0 flex-1 text-sm font-medium">{n.message}</span>
        <div class="flex shrink-0 gap-2">
```

Append ` pointer-coarse:min-h-11` to the class of the Accept, Decline and OK buttons, and add `active:bg-teal-100` to Accept and `active:bg-zinc-800` to Decline and OK.

`site_header/1`:

```heex
    <header
      id="site-header"
      class="flex items-center justify-between gap-2 rounded-2xl bg-linear-to-b from-header-from to-header-to px-3 py-2 text-white shadow-md sm:px-6 sm:py-3"
    >
      <span
        id="current-user-name"
        class="min-w-0 truncate text-base font-semibold tracking-wide sm:text-lg"
      >
        {@current_scope.user.name}
      </span>
      <.link
        href={~p"/"}
        id="my-table-link"
        class="inline-flex shrink-0 items-center justify-center rounded-md px-3 py-1.5 text-sm font-semibold text-white/90 transition hover:bg-white/15 hover:text-white active:bg-white/25 pointer-coarse:min-h-11"
      >
        My table
      </.link>
      <.link
        id="logout-link"
        href={~p"/session"}
        method="delete"
        class="inline-flex shrink-0 items-center justify-center rounded-lg px-3 py-1.5 font-medium text-white/90 transition hover:bg-white/15 hover:text-white active:bg-white/25 pointer-coarse:min-h-11"
      >
        Logout
      </.link>
    </header>
```

`flash_group/1` container class → `"fixed inset-x-2 top-2 z-50 flex flex-col gap-2 sm:inset-x-auto sm:top-4 sm:right-4 sm:w-96"`.

- [ ] **Step 7: Run the tests to verify they pass**

Run: `cd helios && mix test test/helios_web/live/login_live_test.exs test/helios_web/components/layouts_test.exs`
Expected: PASS.

Run: `lsof -ti :4004 | xargs kill 2>/dev/null; cd e2e && npx playwright test --project=responsive && npx playwright test --project=mobile auth.spec.ts lobby.spec.ts presence.spec.ts`
Expected: PASS (including any logout failure recorded in Task 1, Step 7).

- [ ] **Step 8: Precommit and commit**

Run: `cd helios && mix precommit` — Expected: PASS.

```bash
git add helios/assets/css/app.css helios/lib/helios_web/components/layouts/root.html.heex helios/lib/helios_web/components/layouts.ex helios/test/helios_web/live/login_live_test.exs e2e/tests/responsive.spec.ts
git commit -m "feat(web): phone-friendly header, notifications and flash; short variant"
```

---

### Task 3: Login and lobby on small screens

**Files:**
- Modify: `helios/lib/helios_web/live/login_live.ex`
- Modify: `helios/priv/static/images/7_wonders.jpg`
- Modify: `helios/lib/helios_web/live/lobby_live.ex`
- Modify: `helios/lib/helios_web/components/lobby_game_panel.ex`
- Test: `e2e/tests/responsive.spec.ts`

**Interfaces:**
- Consumes: `setupTable`, `closeAll` (Task 1), layout helpers (Task 1).

- [ ] **Step 1: Write the failing e2e tests**

In `e2e/tests/responsive.spec.ts`, change the `./support/game` import to `import { closeAll, newPlayer, setupTable } from "./support/game";`. Replace the body of "login fits" with:

```ts
      await page.goto("/login");
      await expectViewport(page, vp);
      await expectNoHorizontalScroll(page);
      await expect(page.locator("#login-submit")).toBeVisible();
      // "7 WONDERS" stays on one line.
      const title = page.locator("#login-title");
      const lineHeight = await title.evaluate((el) => parseFloat(getComputedStyle(el).lineHeight));
      expect((await boxOf(title)).height).toBeLessThanOrEqual(lineHeight * 1.2);
```

Add after "lobby header and notifications fit":

```ts
    test("lobby controls fit", async ({ browser }) => {
      const { owner, players } = await setupTable(browser, 2, LONG_NAME_PREFIX);
      const page = owner.page;
      await expectViewport(page, vp);
      await expectNoHorizontalScroll(page);
      await expect(page.locator("#invite-button")).toBeInViewport({ ratio: 1 });
      await expect(page.locator("#start-game")).toBeVisible();

      if (vp.width < 640) {
        // Phones: the invite button sits under the select, full width.
        const select = await boxOf(page.locator("#invite_user_id"));
        const button = await boxOf(page.locator("#invite-button"));
        expect(button.y).toBeGreaterThanOrEqual(select.y + select.height);
        expect(button.width).toBeGreaterThanOrEqual(select.width - 1);
      }
      if (vp.touch) {
        await expectTapTargets(page.locator("[id^='uninvite-']"));
        await expectTapTargets(page.locator("#start-game, #invite-button"));
      }

      await closeAll(players);
    });
```

- [ ] **Step 2: Run to verify failure**

Run: `lsof -ti :4004 | xargs kill 2>/dev/null; cd e2e && npx playwright test --project=responsive -g "login fits|lobby controls"`
Expected: FAIL — phones: the invite button sits beside the select; touch: `#uninvite-*` is 32px.

- [ ] **Step 3: Login page**

In `helios/lib/helios_web/live/login_live.ex`: outer div class `min-h-screen` → `min-h-dvh`; `#login-title` class `text-5xl` → `text-4xl sm:text-5xl`.

- [ ] **Step 4: Crop the login art's black frame**

`7_wonders.jpg` (900×900) has a 21–22px black border baked in, which shows as bars at the top and bottom of a portrait phone. Crop 24px from every side:

Run: `sips --cropToHeightWidth 852 852 helios/priv/static/images/7_wonders.jpg && sips -g pixelWidth -g pixelHeight helios/priv/static/images/7_wonders.jpg`
Expected: `pixelWidth: 852`, `pixelHeight: 852`. Open the file and check that no black edge remains. (`sips` crops around the centre. On Linux use `magick 7_wonders.jpg -gravity center -crop 852x852+0+0 +repage 7_wonders.jpg`.)

- [ ] **Step 5: Lobby invite form and remove buttons**

In `helios/lib/helios_web/live/lobby_live.ex`:
- `#invite-form` class `"flex items-end gap-3"` → `"flex flex-col gap-2 sm:flex-row sm:items-end sm:gap-3"`.
- `#invite-button` class → `"w-full rounded-lg bg-zinc-900 px-5 py-2 font-semibold text-white shadow-sm transition hover:bg-zinc-700 disabled:cursor-not-allowed disabled:opacity-40 sm:mb-2 sm:w-auto pointer-coarse:min-h-11"`.
- `#uninvite-*` class → `"inline-flex size-8 items-center justify-center rounded-md bg-zinc-900 text-white transition hover:bg-red-700 active:bg-red-800 pointer-coarse:size-11"`.
- `<section id="lobby">` class `py-10` → `py-6 sm:py-10`.

- [ ] **Step 6: Game panel**

In `helios/lib/helios_web/components/lobby_game_panel.ex`:
- `#game-in-progress` class: add `flex-wrap`.
- `#rejoin-game` class → `"inline-flex items-center rounded-lg bg-zinc-900 px-3 py-1.5 text-sm font-semibold text-white transition hover:bg-zinc-700 active:bg-zinc-800 pointer-coarse:min-h-11"`.
- `#start-game` class: append ` active:bg-zinc-800 pointer-coarse:min-h-11`.

- [ ] **Step 7: Run to verify pass**

Run: `cd helios && mix test test/helios_web/live/lobby_live_test.exs test/helios_web/live/lobby_live_game_test.exs test/helios_web/live/login_live_test.exs`
Expected: PASS.

Run: `lsof -ti :4004 | xargs kill 2>/dev/null; cd e2e && npx playwright test --project=responsive`
Expected: PASS.

- [ ] **Step 8: Precommit and commit**

Run: `cd helios && mix precommit` — Expected: PASS.

```bash
git add helios/lib/helios_web/live/login_live.ex helios/priv/static/images/7_wonders.jpg helios/lib/helios_web/live/lobby_live.ex helios/lib/helios_web/components/lobby_game_panel.ex e2e/tests/responsive.spec.ts
git commit -m "feat(web): fit login and lobby on phones; crop login art frame"
```

---

### Task 4: Compact top bar

**Files:**
- Modify: `helios/lib/helios_web/components/game_components/top_bar.ex`
- Test: `helios/test/helios_web/components/game_components/top_bar_test.exs`
- Test: `e2e/tests/responsive.spec.ts`

**Interfaces:**
- Consumes: `setupTable`, `closeAll`, `startGame`, `waitForTurn` (`e2e/tests/support/game.ts`).
- Produces: `#waiting-count` (phones only; text `Waiting for N`, `title` = names joined by `", "`); the `responsive.spec.ts` test **"game table"**, whose body later tasks extend by inserting sections **just before its final `await closeAll(players);` line**.

- [ ] **Step 1: Write the failing ExUnit tests**

In `helios/test/helios_web/components/game_components/top_bar_test.exs`, add to the first test (after the `#waiting-for` assertion):

```elixir
    assert text(html, "#waiting-count") == "Waiting for 2"
    assert attrs(html, "#waiting-count", "title") == ["Bob, Dee"]
```

(The existing `text(html, "#pass-direction") == "Pass east"` assertion must keep passing: on phones the words become `sr-only`, not removed.)

and to "game over replaces the turn information":

```elixir
    assert count(html, "#waiting-count") == 0
```

- [ ] **Step 2: Write the failing e2e test**

In `e2e/tests/responsive.spec.ts`, change the game import to `import { closeAll, newPlayer, setupTable, startGame, waitForTurn } from "./support/game";` and add inside the `describe`:

```ts
    test("game table", async ({ browser }) => {
      test.setTimeout(120_000);
      const { owner, players } = await setupTable(browser, 3, LONG_NAME_PREFIX);
      await startGame(owner, players);
      const page = owner.page;
      await waitForTurn(page, 1, 1);
      await expectViewport(page, vp);
      await expectNoHorizontalScroll(page);

      // --- top bar
      if (vp.width < 640) {
        expect((await boxOf(page.locator("#top-bar"))).height).toBeLessThanOrEqual(96);
        await expect(page.locator("#waiting-count")).toBeVisible();
        await expect(page.locator("#waiting-for")).toBeHidden();
      } else {
        await expect(page.locator("#waiting-for")).toBeVisible();
        await expect(page.locator("#waiting-count")).toBeHidden();
      }

      await closeAll(players);
    });
```

- [ ] **Step 3: Run to verify failure**

Run: `cd helios && mix test test/helios_web/components/game_components/top_bar_test.exs`
Expected: FAIL — no `#waiting-count`.

Run: `lsof -ti :4004 | xargs kill 2>/dev/null; cd e2e && npx playwright test --project=responsive -g "game table"`
Expected: FAIL on phones (top bar taller than 96px; no `#waiting-count`).

- [ ] **Step 4: Implement**

Replace the `~H` in `HeliosWeb.GameComponents.TopBar.top_bar/1` with:

```heex
    <header
      id="top-bar"
      data-phase={@phase.kind}
      data-turn-key={"#{@phase.age}-#{@phase.turn}"}
      class="sticky top-0 z-20 flex flex-col gap-1.5 rounded-xl bg-linear-to-r from-header-from to-header-to px-3 py-2 text-white shadow-lg short:static sm:flex-row sm:flex-wrap sm:items-center sm:justify-between sm:gap-3 sm:px-4 sm:py-3"
    >
      <div class="flex items-center justify-between gap-3 sm:justify-start">
        <div class="flex items-center gap-3 sm:gap-4">
          <span id="age-label" class="text-lg font-bold tracking-wide sm:text-xl">
            Age {GameFormat.roman(@phase.age)}
          </span>
          <%= if @phase.kind == :game_over do %>
            <span id="turn-label" class="font-semibold">Game over</span>
          <% else %>
            <span id="turn-label">Turn {@phase.turn}/6</span>
            <span id="pass-direction" class="flex items-center gap-1">
              <.icon name={GameFormat.direction_icon(@phase.direction)} class="size-5" />
              <span class="sr-only sm:not-sr-only">
                Pass {GameFormat.direction_label(@phase.direction)}
              </span>
            </span>
          <% end %>
        </div>
        <span
          :if={@waiting != []}
          id="waiting-count"
          title={Enum.join(@waiting, ", ")}
          class="flex items-center gap-1 text-sm text-white/90 sm:hidden"
        >
          <.icon name="hero-clock" class="size-4" /> Waiting for {length(@waiting)}
        </span>
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
        </li>
      </ul>
    </header>
```

- [ ] **Step 5: Run to verify pass**

Run: `cd helios && mix test test/helios_web/components/game_components/top_bar_test.exs test/helios_web/live/game_live_test.exs`
Expected: PASS (`#waiting-for` keeps its exact text; `has_element?(view, "#waiting-for", name)` still matches).

Run: `lsof -ti :4004 | xargs kill 2>/dev/null; cd e2e && npx playwright test --project=responsive -g "game table"`
Expected: PASS.

- [ ] **Step 6: Precommit and commit**

Run: `cd helios && mix precommit` — Expected: PASS.

```bash
git add helios/lib/helios_web/components/game_components/top_bar.ex helios/test/helios_web/components/game_components/top_bar_test.exs e2e/tests/responsive.spec.ts
git commit -m "feat(web): two-row sticky top bar with waiting count on phones"
```

---

### Task 5: Table reflow — page structure, neighbour summaries, board

**Files:**
- Modify: `helios/lib/helios_web/live/game_live.ex` (`render/1` only)
- Modify: `helios/lib/helios_web/components/game_components/player_panels.ex`
- Modify: `helios/lib/helios_web/components/game_components/board.ex`
- Test: `helios/test/helios_web/components/game_components/player_panels_test.exs`
- Test: `e2e/tests/responsive.spec.ts`

**Interfaces:**
- Consumes: "game table" test (Task 4); `playFirstBuildableOrDiscard` (`e2e/tests/support/game.ts`).
- Produces: `#table` (the scrolling table region in `GameLive`); per neighbour panel `#<id>-toggle` (button, `aria-expanded`, `aria-controls="<id>-details"`) and `#<id>-details`; the `is-open` class on `#<id>` while expanded.

- [ ] **Step 1: Write the failing ExUnit test**

Add to `helios/test/helios_web/components/game_components/player_panels_test.exs`:

```elixir
  test "neighbour panel folds into a one-line summary toggle on phones" do
    html =
      render_component(&PlayerPanels.neighbour_panel/1,
        id: "east-panel",
        label: "East",
        player: SampleViews.player("2", %{wonder: "Rhódos", side: :b}),
        names: SampleViews.names()
      )

    assert attrs(html, "#east-panel-toggle", "aria-controls") == ["east-panel-details"]
    assert attrs(html, "#east-panel-toggle", "aria-expanded") == ["false"]
    assert text(html, "#east-panel-toggle") =~ "East"
    assert text(html, "#east-panel-toggle") =~ "Bob"
    assert attrs(html, "#east-panel-toggle img", "alt") |> Enum.all?(&(&1 == ""))
    assert [js] = attrs(html, "#east-panel-toggle", "phx-click")
    assert js =~ "toggle_class" and js =~ "is-open"
    assert count(html, "#east-panel-details header") == 1
    # The summary must not duplicate what the existing test counts.
    assert count(html, "#east-panel-toggle [data-stat], #east-panel-toggle [data-card]") == 0
  end
```

- [ ] **Step 2: Write the failing e2e tests**

In "game table", insert before the final `await closeAll(players);`:

```ts
      // --- table: neighbours fold into summaries below md, and stay open across updates
      if (vp.width < 768) {
        await expect(page.locator("#west-panel-details")).toBeHidden();
        await page.locator("#west-panel-toggle").click();
        await expect(page.locator("#west-panel-details")).toBeVisible();
        await expect(page.locator("#west-panel-toggle")).toHaveAttribute("aria-expanded", "true");
        if (vp.touch) await expectTapTargets(page.locator("#west-panel-toggle, #east-panel-toggle"));

        await playFirstBuildableOrDiscard(players[1].page);
        await expect(page.locator("#waiting-count")).toHaveText(/Waiting for 2/);
        await expect(page.locator("#west-panel-details")).toBeVisible();
      } else {
        await expect(page.locator("#west-panel-toggle")).toBeHidden();
        await expect(page.locator("#west-panel-details")).toBeVisible();
        await playFirstBuildableOrDiscard(players[1].page);
      }
      await expectNoHorizontalScroll(page);
```

Add `playFirstBuildableOrDiscard` to the `./support/game` import. After the `for (const vp of VIEWPORTS) { … }` loop, add:

```ts
test.describe("seven players on a small phone", () => {
  test.use({ viewport: { width: 360, height: 740 }, hasTouch: true, isMobile: true });

  test("the table still fits", async ({ browser }) => {
    test.setTimeout(180_000);
    const { owner, players } = await setupTable(browser, 7, LONG_NAME_PREFIX);
    await startGame(owner, players);
    const page = owner.page;
    await waitForTurn(page, 1, 1);
    await expectViewport(page, { name: "small-android", width: 360, height: 740, touch: true });
    await expectNoHorizontalScroll(page);
    expect((await boxOf(page.locator("#top-bar"))).height).toBeLessThanOrEqual(96);
    await expect(page.locator("#other-players article")).toHaveCount(4);
    const strip = await boxOf(page.locator("#other-players"));
    expect(strip.x + strip.width).toBeLessThanOrEqual(360);
    await closeAll(players);
  });
});
```

- [ ] **Step 3: Run to verify failure**

Run: `cd helios && mix test test/helios_web/components/game_components/player_panels_test.exs`
Expected: FAIL — no `#east-panel-toggle`.

Run: `lsof -ti :4004 | xargs kill 2>/dev/null; cd e2e && npx playwright test --project=responsive -g "game table|seven players"`
Expected: FAIL — no `#west-panel-details`.

- [ ] **Step 4: Neighbour panel and others strip**

In `helios/lib/helios_web/components/game_components/player_panels.ex` add `alias Phoenix.LiveView.JS` and replace `neighbour_panel/1`'s `~H` with:

```heex
    <section
      id={@id}
      data-player={@player.name}
      class="group flex flex-col gap-2 rounded-xl bg-antique/90 p-2 shadow sm:p-3"
    >
      <button
        id={"#{@id}-toggle"}
        type="button"
        aria-expanded="false"
        aria-controls={"#{@id}-details"}
        phx-click={
          JS.toggle_class("is-open", to: "##{@id}")
          |> JS.toggle_attribute({"aria-expanded", "true", "false"})
        }
        class="flex w-full items-center gap-2 text-left transition active:opacity-70 md:hidden pointer-coarse:min-h-11"
      >
        <img
          src={GameAssets.wonder_path(@player.wonder, @player.side)}
          alt=""
          class="aspect-[16/5] w-18 shrink-0 rounded object-cover"
        />
        <span class="flex min-w-0 flex-1 flex-col">
          <span class="flex items-baseline gap-1.5">
            <span class="text-xs font-semibold uppercase tracking-wide text-zinc-500">
              {@label}
            </span>
            <span class="truncate font-semibold text-zinc-900">
              {GameFormat.player_name(@names, @player.name)}
            </span>
          </span>
          <span class="flex items-center gap-2 text-xs text-zinc-700">
            <span class="flex items-center gap-0.5">
              <img src={GameAssets.token_path(:coin)} alt="" class="size-3.5" />{@player.coins}
            </span>
            <span class="flex items-center gap-0.5">
              <.icon name="hero-shield-check" class="size-3.5 text-red-700" />{@player.shields}
            </span>
            <span class="flex items-center gap-0.5">
              <img src={GameAssets.token_path(:pyramid)} alt="" class="size-3.5" />{@player.stages_built}/{@player.stages_total}
            </span>
            <span class="flex min-w-0 flex-wrap gap-0.5">
              <span
                :for={card <- @player.built}
                class={["size-2.5 rounded-sm", GameFormat.category_class(card.category)]}
              >
              </span>
            </span>
          </span>
        </span>
        <.icon
          name="hero-chevron-down"
          class="size-4 shrink-0 text-zinc-500 transition group-[.is-open]:rotate-180"
        />
      </button>
      <div id={"#{@id}-details"} class="hidden flex-col gap-2 group-[.is-open]:flex md:flex">
        <header class="flex items-center justify-between gap-2">
          <span class="text-xs font-semibold uppercase tracking-wide text-zinc-500">{@label}</span>
          <span class="truncate font-semibold text-zinc-900">
            {GameFormat.player_name(@names, @player.name)}
          </span>
        </header>
        <img
          src={GameAssets.wonder_path(@player.wonder, @player.side)}
          alt={"#{@player.wonder} #{GameFormat.side_label(@player.side)}"}
          class="aspect-[16/5] w-full rounded object-cover"
        />
        <.player_stats player={@player} />
        <div class="flex flex-wrap gap-1">
          <span
            :for={card <- @player.built}
            title={card.name}
            data-card={card.name}
            class={["size-4 rounded-sm ring-1 ring-black/10", GameFormat.category_class(card.category)]}
          >
          </span>
        </div>
      </div>
    </section>
```

In `others_strip/1` change the article class `min-w-56` → `min-w-48 sm:min-w-56` and `p-3` → `p-2 sm:p-3`.

- [ ] **Step 5: Board**

In `helios/lib/helios_web/components/game_components/board.ex`, `#my-board` class → `"order-first flex flex-col gap-3 rounded-xl bg-antique/90 p-2 shadow-lg sm:p-3 md:col-span-2 lg:order-none lg:col-span-1"`.

- [ ] **Step 6: GameLive page structure**

Replace `render/1` in `helios/lib/helios_web/live/game_live.ex` with the version below. This is the interim structure; Task 6 moves the notices and hand into the dock.

```elixir
  @impl true
  def render(assigns) do
    ~H"""
    <Layouts.app flash={@flash} current_scope={@current_scope} notifications={@notifications}>
      <div
        id="game"
        class="flex min-h-dvh flex-col bg-[url('/images/paper.jpg')] bg-cover lg:bg-fixed"
      >
        <div class="mx-auto flex w-full max-w-7xl flex-1 flex-col gap-3 px-2 pt-3 sm:gap-4 sm:px-4 sm:pt-4 lg:px-8">
          <.top_bar view={@view} names={@names} connected={@connected} />
          <%= if @view.phase.kind == :game_over do %>
            <div class="pb-4">
              <.scoreboard
                scores={@view.scores}
                names={@names}
                me={@view.me}
                lobby_id={@game.lobby_id}
              />
            </div>
          <% else %>
            <div id="table" class="flex flex-1 flex-col gap-3 pb-1 sm:gap-4">
              <.others_strip :if={@others != []} players={@others} names={@names} />
              <div class="grid items-start gap-3 sm:gap-4 md:grid-cols-2 lg:grid-cols-[1fr_2fr_1fr]">
                <.neighbour_panel id="west-panel" label="West" player={@west} names={@names} />
                <.my_board player={@me} />
                <.neighbour_panel id="east-panel" label="East" player={@east} names={@names} />
              </div>
              <.extra_turn view={@view} names={@names} />
              <.pending_choice
                :if={@view.my_pending && is_nil(@selected)}
                action={@view.my_pending}
              />
              <.hand
                :if={GameFormat.show_hand?(@view)}
                hand={@view.hand}
                selected={@selected}
                pending={@view.my_pending}
              />
              <.action_panel :if={@selected_card} card={@selected_card} />
            </div>
          <% end %>
        </div>
      </div>
    </Layouts.app>
    """
  end
```

- [ ] **Step 7: Run to verify pass**

Run: `cd helios && mix test test/helios_web/components test/helios_web/live`
Expected: PASS (the existing panel test still counts exactly one wonder `img[alt='Rhódos B']`, one set of `data-stat` and three `data-card`).

Run: `lsof -ti :4004 | xargs kill 2>/dev/null; cd e2e && npx playwright test --project=responsive -g "game table|seven players"`
Expected: PASS. If the expanded panel collapses after the other player's submission, the JS toggle was lost in a patch: check that the toggle targets `#west-panel` by id and that the section keeps a stable `id` (LiveView keeps JS-applied classes across patches only on the same element).

- [ ] **Step 8: Precommit and commit**

Run: `cd helios && mix precommit` — Expected: PASS.

```bash
git add helios/lib/helios_web/live/game_live.ex helios/lib/helios_web/components/game_components/player_panels.ex helios/lib/helios_web/components/game_components/board.ex helios/test/helios_web/components/game_components/player_panels_test.exs e2e/tests/responsive.spec.ts
git commit -m "feat(web): reflow the game table; fold neighbours into summaries on phones"
```

---

### Task 6: The hand dock

**Files:**
- Modify: `helios/lib/helios_web/game_format.ex`
- Modify: `helios/lib/helios_web/components/game_components/extra_turn.ex`
- Modify: `helios/lib/helios_web/components/game_components/hand.ex` (`hand/1`, `pending_choice/1`)
- Modify: `helios/lib/helios_web/live/game_live.ex` (`render/1`)
- Test: `helios/test/helios_web/game_format_test.exs`
- Test: `helios/test/helios_web/components/game_components/extra_turn_test.exs`
- Test: `helios/test/helios_web/live/game_live_test.exs`
- Test: `helios/test/helios_web/live/game_live_extra_turns_test.exs`
- Test: `e2e/tests/responsive.spec.ts`

**Interfaces:**
- Consumes: `GameFormat.show_hand?/1` (existing); `#table` (Task 5).
- Produces:
  - `GameFormat.dock?(view) :: boolean` — `true` when `show_hand?(view)` or `view.phase.kind == :extra_turn`.
  - `ExtraTurn.extra_turn_notice/1` (attrs `view`, `names`) — renders `#waiting-extra-turn` or `#play-last-card` during an extra turn, nothing otherwise.
  - `ExtraTurn.discard_picker/1` (attr `view`) — renders `#discard-picker` only on my own build-from-discard turn. (Task 7 restyles it as a sheet.)
  - `ExtraTurn.extra_turn/1` is removed.
  - `#dock` — sticky bottom container holding the status line and `#hand`.

- [ ] **Step 1: Write the failing ExUnit tests**

Add to `helios/test/helios_web/game_format_test.exs`:

```elixir
  test "dock? shows the dock for a hand or an extra turn, never at game over" do
    view = SampleViews.view()
    assert GameFormat.dock?(view)
    refute GameFormat.dock?(%{view | hand: []})

    assert GameFormat.dock?(%{
             view
             | hand: [],
               phase: %{view.phase | kind: :extra_turn, extra_turn_player: "2"}
           })

    refute GameFormat.dock?(%{view | hand: [], phase: %{view.phase | kind: :game_over}})
  end
```

In `helios/test/helios_web/components/game_components/extra_turn_test.exs` replace `render_extra/1` and the four tests with:

```elixir
  defp render_notice(view),
    do: render_component(&ExtraTurn.extra_turn_notice/1, view: view, names: SampleViews.names())

  defp render_picker(view), do: render_component(&ExtraTurn.discard_picker/1, view: view)

  test "my build-from-discard turn shows a picker over the discard pile and no notice" do
    view = extra("1", :build_from_discard, %{discard_pile: ["Altar", "Loom"], hand: []})
    html = render_picker(view)
    assert attrs(html, "#discard-picker [data-card]", "data-card") == ["Altar", "Loom"]
    assert attrs(html, "#discard-pick-1", "phx-value-kind") == ["build_from_discard"]
    assert attrs(html, "#discard-pick-1", "phx-value-card") == ["Loom"]
    assert count(render_notice(view), "#waiting-extra-turn, #play-last-card") == 0
  end

  test "my play-last-card turn shows the notice and no picker" do
    view = extra("1", :play_last_card)
    assert text(render_notice(view), "#play-last-card") =~ "Play your last card"
    assert count(render_picker(view), "#discard-picker") == 0
  end

  test "other players wait for the extra-turn player" do
    view = extra("2", :build_from_discard)
    assert text(render_notice(view), "#waiting-extra-turn") =~ "Waiting for Bob"
    assert count(render_picker(view), "#discard-picker") == 0
  end

  test "renders nothing outside extra turns" do
    view = SampleViews.view()
    assert count(render_notice(view), "#waiting-extra-turn, #play-last-card") == 0
    assert count(render_picker(view), "#discard-picker") == 0
  end
```

Add to `helios/test/helios_web/live/game_live_test.exs`:

```elixir
  test "the dock holds the hand and the pending choice", %{game: game, players: [{_a, ta} | _]} do
    {:ok, view, _html} = open(ta, game.id)
    assert has_element?(view, "#dock #hand #hand-card-0")

    view |> element("#hand-card-0") |> render_click()
    view |> element("#discard-button") |> render_click()
    assert has_element?(view, "#dock #pending-choice")
  end
```

In `helios/test/helios_web/live/game_live_extra_turns_test.exs`:
- Halikarnassós test: after `assert has_element?(view_a, "#discard-picker")` add `refute has_element?(view_a, "#dock #discard-picker")`; change the `#waiting-extra-turn` assertion to `assert has_element?(view_b, "#dock #waiting-extra-turn", a.name)`. Keep `refute has_element?(view_b, "#hand")`.
- Babylon test: `assert has_element?(view_a, "#play-last-card")` → `assert has_element?(view_a, "#dock #play-last-card")`; `#hand-card-0` → `#dock #hand-card-0`.
- Scoreboard test: after `refute has_element?(view_a, "#hand")` add `refute has_element?(view_a, "#dock")`.

- [ ] **Step 2: Write the failing e2e test**

In "game table", insert before the final `await closeAll(players);`:

```ts
      // --- dock: the hand is always on screen and never covers the table
      await page.evaluate(() => window.scrollTo(0, 0));
      await expect(page.locator("#hand")).toBeInViewport({ ratio: 1 });
      if (vp.height <= 500) {
        expect((await boxOf(page.locator("#dock"))).height).toBeLessThanOrEqual(vp.height * 0.4);
      }
      await page.evaluate(() => window.scrollTo(0, document.documentElement.scrollHeight));
      const table = await boxOf(page.locator("#table"));
      expect(table.y + table.height).toBeLessThanOrEqual((await boxOf(page.locator("#dock"))).y + 1);

      if (vp.name === "iphone") {
        // Rotating mid-turn keeps the dock on screen and compact.
        await page.setViewportSize({ width: vp.height, height: vp.width });
        await expect(page.locator("#hand")).toBeInViewport({ ratio: 1 });
        await expectNoHorizontalScroll(page);
        expect((await boxOf(page.locator("#dock"))).height).toBeLessThanOrEqual(vp.width * 0.4);
        await page.setViewportSize({ width: vp.width, height: vp.height });
      }
```

- [ ] **Step 3: Run to verify failure**

Run: `cd helios && mix test test/helios_web/game_format_test.exs test/helios_web/components/game_components/extra_turn_test.exs test/helios_web/live`
Expected: FAIL — `dock?/1`, `extra_turn_notice/1` and `#dock` are undefined or missing.

Run: `lsof -ti :4004 | xargs kill 2>/dev/null; cd e2e && npx playwright test --project=responsive -g "game table"`
Expected: FAIL — no `#dock`.

- [ ] **Step 4: `GameFormat.dock?/1`**

Add to `helios/lib/helios_web/game_format.ex` after `show_hand?/1`:

```elixir
  def dock?(%{phase: %{kind: :extra_turn}}), do: true
  def dock?(view), do: show_hand?(view)
```

- [ ] **Step 5: Split `ExtraTurn`**

Replace the body of `HeliosWeb.GameComponents.ExtraTurn` (keep `use HeliosWeb, :html` and the aliases) with:

```elixir
  @moduledoc "Extra-turn notices for the dock (Babylon B last card, waiting for others) and the Halikarnassós discard picker."

  attr :view, :map, required: true
  attr :names, :map, required: true

  def extra_turn_notice(%{view: %{phase: %{kind: :extra_turn}}} = assigns) do
    ~H"""
    <%= cond do %>
      <% @view.phase.extra_turn_player != @view.me -> %>
        <div
          id="waiting-extra-turn"
          class="flex items-center gap-2 rounded-lg bg-white/90 px-3 py-2 text-sm shadow-sm"
        >
          <.icon name="hero-clock" class="size-5 shrink-0 animate-pulse text-sky-600" />
          <span>
            Waiting for {GameFormat.player_name(@names, @view.phase.extra_turn_player)} ({GameFormat.extra_turn_label(
              @view.phase.extra_turn_kind
            )})
          </span>
        </div>
      <% @view.phase.extra_turn_kind == :play_last_card -> %>
        <div id="play-last-card" class="rounded-lg bg-amber-50 px-3 py-2 text-sm ring-1 ring-amber-300">
          <strong>Play your last card:</strong> build it, build a wonder stage with it, or discard it.
        </div>
      <% true -> %>
    <% end %>
    """
  end

  def extra_turn_notice(assigns), do: ~H""

  attr :view, :map, required: true

  def discard_picker(assigns) do
    ~H"""
    <section :if={my_discard_turn?(@view)} id="discard-picker" class="rounded-xl bg-antique/95 p-4 shadow-xl">
      <h2 class="mb-3 font-semibold text-zinc-900">
        Build one card from the discard pile for free
      </h2>
      <div class="flex flex-wrap gap-2">
        <button
          :for={{card, index} <- Enum.with_index(@view.discard_pile || [])}
          id={"discard-pick-#{index}"}
          type="button"
          phx-click="submit"
          phx-value-card={card}
          phx-value-kind="build_from_discard"
          phx-value-option="0"
          data-card={card}
          class="rounded-lg transition hover:-translate-y-1 hover:ring-4 hover:ring-sky-400"
        >
          <img src={GameAssets.card_path(card)} alt={card} class="h-[183px] w-[120px] rounded-lg" />
        </button>
      </div>
    </section>
    """
  end

  defp my_discard_turn?(%{
         me: me,
         phase: %{kind: :extra_turn, extra_turn_kind: :build_from_discard, extra_turn_player: me}
       }),
       do: true

  defp my_discard_turn?(_view), do: false
```

- [ ] **Step 6: Dock-sized hand and compact pending banner**

In `helios/lib/helios_web/components/game_components/hand.ex`, replace `hand/1`'s `~H` with:

```heex
    <section id="hand" aria-label="Your hand">
      <h2 class="sr-only text-xs font-semibold uppercase tracking-wide text-zinc-700 sm:not-sr-only short:sr-only">
        Your hand
      </h2>
      <div class="-mx-2 flex snap-x gap-2 overflow-x-auto px-2 pt-2 pb-1 sm:pt-3">
        <button
          :for={{card, index} <- Enum.with_index(@hand)}
          type="button"
          id={"hand-card-#{index}"}
          phx-click="select_card"
          phx-value-card={card.name}
          data-card={card.name}
          data-buildable={to_string(GameFormat.available?(card.build))}
          aria-pressed={to_string(@selected == card.name)}
          class={[
            "relative shrink-0 snap-start rounded-lg transition duration-150 hover:-translate-y-1 active:scale-95 focus:outline-none focus-visible:ring-4 focus-visible:ring-sky-300",
            @selected == card.name && "-translate-y-2 ring-4 ring-sky-500",
            @pending_card == card.name && "ring-4 ring-amber-500"
          ]}
        >
          <img
            src={GameAssets.card_path(card.name)}
            alt={card.name}
            class="h-[98px] w-16 rounded-lg object-cover sm:h-[134px] sm:w-22 lg:h-[183px] lg:w-[120px] short:h-[86px] short:w-14"
          />
          <span
            :if={@pending_card == card.name}
            class="absolute left-1 top-1 rounded bg-amber-500 px-1.5 py-0.5 text-xs font-bold text-white"
          >
            Chosen
          </span>
        </button>
      </div>
    </section>
```

In `pending_choice/1`: container class → `"flex flex-wrap items-center justify-between gap-2 rounded-lg bg-amber-50 px-3 py-2 text-sm ring-1 ring-amber-300"`; `#change-choice` class → `"inline-flex items-center rounded-lg bg-white px-3 py-1.5 text-sm font-semibold text-zinc-900 ring-1 ring-amber-300 transition hover:bg-amber-100 active:bg-amber-200 pointer-coarse:min-h-11"`.

- [ ] **Step 7: GameLive render with the dock**

In `helios/lib/helios_web/live/game_live.ex` replace the `<% else %>` branch of `render/1` (from `<div id="table"` through the `.action_panel` line) with:

```heex
            <div id="table" class="flex flex-1 flex-col gap-3 pb-1 sm:gap-4">
              <.others_strip :if={@others != []} players={@others} names={@names} />
              <div class="grid items-start gap-3 sm:gap-4 md:grid-cols-2 lg:grid-cols-[1fr_2fr_1fr]">
                <.neighbour_panel id="west-panel" label="West" player={@west} names={@names} />
                <.my_board player={@me} />
                <.neighbour_panel id="east-panel" label="East" player={@east} names={@names} />
              </div>
            </div>
            <div
              :if={GameFormat.dock?(@view)}
              id="dock"
              class="sticky bottom-0 z-30 -mx-2 flex flex-col gap-2 rounded-t-xl bg-antique/95 px-2 pt-2 pb-[max(0.5rem,env(safe-area-inset-bottom))] shadow-[0_-4px_12px_rgba(0,0,0,0.2)] sm:mx-0 sm:px-3"
            >
              <.extra_turn_notice view={@view} names={@names} />
              <.pending_choice
                :if={@view.my_pending && is_nil(@selected)}
                action={@view.my_pending}
              />
              <.hand
                :if={GameFormat.show_hand?(@view)}
                hand={@view.hand}
                selected={@selected}
                pending={@view.my_pending}
              />
            </div>
            <.discard_picker view={@view} />
            <.action_panel :if={@selected_card} card={@selected_card} />
```

Do not put `backdrop-blur`, `transform` or `filter` classes on `#dock`: they would turn it into the containing block for the fixed sheets in Task 7.

- [ ] **Step 8: Run to verify pass**

Run: `cd helios && mix test test/helios_web`
Expected: PASS.

Run: `lsof -ti :4004 | xargs kill 2>/dev/null; cd e2e && npx playwright test --project=responsive -g "game table"`
Expected: PASS. If the landscape dock is over 40% of the height, `short:` is losing to `sm:`/`lg:` in CSS order: open `helios/priv/static/assets/css/app.css`, check whether the `@media (max-height: 500px)` rules come after the `@media (width >= 40rem)` ones, and if not, move the `@custom-variant short` line to the end of `app.css`.

- [ ] **Step 9: Precommit and commit**

Run: `cd helios && mix precommit` — Expected: PASS.

```bash
git add helios/lib/helios_web/game_format.ex helios/lib/helios_web/components/game_components/extra_turn.ex helios/lib/helios_web/components/game_components/hand.ex helios/lib/helios_web/live/game_live.ex helios/test/helios_web/game_format_test.exs helios/test/helios_web/components/game_components/extra_turn_test.exs helios/test/helios_web/live/game_live_test.exs helios/test/helios_web/live/game_live_extra_turns_test.exs e2e/tests/responsive.spec.ts
git commit -m "feat(web): dock the hand and turn notices at the bottom of the table"
```

---

### Task 7: The action sheet, `deselect`, and the discard-picker sheet

**Files:**
- Modify: `helios/lib/helios_web/game_format.ex`
- Modify: `helios/lib/helios_web/components/game_components/hand.ex` (`action_panel/1`, `option_group/1`, button classes)
- Modify: `helios/lib/helios_web/components/game_components/extra_turn.ex` (`discard_picker/1`)
- Modify: `helios/lib/helios_web/live/game_live.ex` (`handle_event/3`)
- Test: `helios/test/helios_web/components/game_components/hand_test.exs`
- Test: `helios/test/helios_web/components/game_components/extra_turn_test.exs`
- Test: `helios/test/helios_web/live/game_live_test.exs`
- Test: `e2e/tests/responsive.spec.ts`
- Test: `e2e/tests/game.spec.ts`

**Interfaces:**
- Consumes: `#dock`, `ExtraTurn.discard_picker/1` (Task 6).
- Produces:
  - `GameFormat.sheet_class() :: String.t()` — shared classes for both sheets.
  - `#action-scrim` (`phx-click="deselect"`), `#action-panel` (`role="dialog"`, `aria-modal="true"`, `aria-labelledby="action-panel-title"`, `phx-window-keydown="deselect"`, `phx-key="Escape"`), `#action-panel-title`, `#close-action-panel` (`phx-click="deselect"`).
  - `#discard-scrim` (no `phx-click`); `#discard-picker` gains `role="dialog"`, `aria-modal="true"`, `aria-labelledby="discard-picker-title"`.
  - LiveView event `"deselect"`: clears the selection; ignores keydown payloads whose `"key"` is not `"Escape"`.

- [ ] **Step 1: Write the failing ExUnit tests**

Add to `helios/test/helios_web/components/game_components/hand_test.exs`:

```elixir
  test "the action panel is a dismissable dialog" do
    html = render_component(&Hand.action_panel/1, card: card("Tavern"))
    assert attrs(html, "#action-panel", "role") == ["dialog"]
    assert attrs(html, "#action-panel", "aria-modal") == ["true"]
    assert attrs(html, "#action-panel", "aria-labelledby") == ["action-panel-title"]
    assert text(html, "#action-panel-title") == "Tavern"
    assert attrs(html, "#action-panel", "phx-window-keydown") == ["deselect"]
    assert attrs(html, "#action-panel", "phx-key") == ["Escape"]
    assert attrs(html, "#close-action-panel", "phx-click") == ["deselect"]
    assert attrs(html, "#close-action-panel", "aria-label") == ["Close"]
    assert attrs(html, "#action-scrim", "phx-click") == ["deselect"]
  end
```

Add to `helios/test/helios_web/components/game_components/extra_turn_test.exs`:

```elixir
  test "the discard picker is a dialog that cannot be dismissed" do
    html = render_picker(extra("1", :build_from_discard, %{discard_pile: ["Altar"], hand: []}))
    assert attrs(html, "#discard-picker", "role") == ["dialog"]
    assert attrs(html, "#discard-picker", "aria-labelledby") == ["discard-picker-title"]
    assert count(html, "#discard-scrim") == 1
    assert count(html, "#discard-scrim[phx-click]") == 0
    assert count(html, "#discard-picker[phx-window-keydown]") == 0
    assert count(html, "#close-action-panel") == 0
  end
```

Add to `helios/test/helios_web/live/game_live_test.exs`:

```elixir
  test "the action sheet closes from the scrim, the close button and Escape", %{
    game: game,
    players: [{_a, ta} | _]
  } do
    {:ok, view, _html} = open(ta, game.id)

    closers = [
      fn view -> view |> element("#action-scrim") |> render_click() end,
      fn view -> view |> element("#close-action-panel") |> render_click() end,
      fn view -> view |> element("#action-panel") |> render_keydown(%{"key" => "Escape"}) end
    ]

    for close <- closers do
      view |> element("#hand-card-0") |> render_click()
      assert has_element?(view, "#action-panel[role='dialog']")
      close.(view)
      refute has_element?(view, "#action-panel")
      refute has_element?(view, "#action-scrim")
      assert has_element?(view, "#hand-card-0[aria-pressed='false']")
    end
  end

  test "other keys and a deselect with nothing selected change nothing", %{
    game: game,
    players: [{_a, ta} | _]
  } do
    {:ok, view, _html} = open(ta, game.id)
    render_click(view, "deselect", %{})
    refute has_element?(view, "#action-panel")

    view |> element("#hand-card-0") |> render_click()
    view |> element("#action-panel") |> render_keydown(%{"key" => "a"})
    assert has_element?(view, "#action-panel")
  end
```

- [ ] **Step 2: Write the failing e2e tests**

In "game table" (`e2e/tests/responsive.spec.ts`), insert before the final `await closeAll(players);`:

```ts
      // --- action sheet
      const firstCard = page.locator("#hand [data-card]").first();
      const sheet = page.locator("#action-panel");
      await firstCard.click();
      await expect(sheet).toBeVisible();
      expect((await boxOf(sheet)).height).toBeLessThanOrEqual(vp.height);
      if (vp.height > 500) {
        for (const button of await sheet.locator("button:visible").all()) {
          await expect(button).toBeInViewport({ ratio: 1 });
        }
      } else {
        // Landscape phones: the sheet scrolls internally; Discard must be reachable.
        await page.locator("#discard-button").scrollIntoViewIfNeeded();
        await expect(page.locator("#discard-button")).toBeInViewport({ ratio: 1 });
      }
      if (vp.touch) await expectTapTargets(sheet.locator("button:visible"));
      await expectNoHorizontalScroll(page);

      await page.locator("#action-scrim").click({ position: { x: 5, y: 5 } });
      await expect(sheet).toHaveCount(0);

      await firstCard.click();
      await page.keyboard.press("a");
      await expect(sheet).toBeVisible();
      await page.keyboard.press("Escape");
      await expect(sheet).toHaveCount(0);

      await firstCard.click();
      await page.locator("#close-action-panel").click();
      await expect(sheet).toHaveCount(0);

      // --- the pending choice lives in the dock
      await firstCard.click();
      await page.locator("#discard-button").click();
      await expect(page.locator("#dock #pending-choice")).toBeInViewport({ ratio: 1 });
      if (vp.touch) await expectTapTargets(page.locator("#change-choice"));
```

In `e2e/tests/game.spec.ts`, "a player can change their choice before the turn resolves", replace

```ts
  if (built !== discarded) await buildable.click();
```

with

```ts
  if (built !== discarded) {
    // The sheet's scrim covers the dock: close it before tapping another card.
    await a.page.locator("#close-action-panel").click();
    await buildable.click();
  }
```

- [ ] **Step 3: Run to verify failure**

Run: `cd helios && mix test test/helios_web/components/game_components test/helios_web/live/game_live_test.exs`
Expected: FAIL — no `role`, no `#action-scrim`, no `deselect` handler (`FunctionClauseError`).

Run: `lsof -ti :4004 | xargs kill 2>/dev/null; cd e2e && npx playwright test --project=responsive -g "game table"`
Expected: FAIL — no `#action-scrim`.

- [ ] **Step 4: `GameFormat.sheet_class/0`**

Add to `helios/lib/helios_web/game_format.ex`:

```elixir
  @sheet_class "fixed inset-x-0 bottom-0 z-50 max-h-[85dvh] overflow-y-auto rounded-t-2xl bg-white p-4 pb-[max(1rem,env(safe-area-inset-bottom))] shadow-2xl ring-1 ring-black/5 lg:inset-x-auto lg:bottom-auto lg:left-1/2 lg:top-1/2 lg:w-full lg:max-w-xl lg:-translate-x-1/2 lg:-translate-y-1/2 lg:rounded-2xl lg:pb-4"

  @doc "Bottom sheet on phones and tablets, centred modal from `lg`."
  def sheet_class, do: @sheet_class
```

- [ ] **Step 5: The action sheet**

In `helios/lib/helios_web/components/game_components/hand.ex`:

Module attributes:

```elixir
  @action_button "inline-flex w-full items-center justify-center rounded-lg bg-zinc-900 px-3 py-2 text-sm font-semibold text-white shadow transition hover:-translate-y-0.5 hover:bg-zinc-700 active:translate-y-0 active:bg-zinc-800 sm:w-auto pointer-coarse:min-h-11"
  @disabled_button "inline-flex w-full cursor-not-allowed items-center justify-center rounded-lg bg-zinc-200 px-3 py-2 text-sm text-zinc-500 sm:w-auto pointer-coarse:min-h-11"
```

Replace `action_panel/1`'s `~H` with:

```heex
    <div id="action-scrim" phx-click="deselect" class="fixed inset-0 z-40 bg-black/35" aria-hidden="true">
    </div>
    <section
      id="action-panel"
      role="dialog"
      aria-modal="true"
      aria-labelledby="action-panel-title"
      phx-window-keydown="deselect"
      phx-key="Escape"
      class={GameFormat.sheet_class()}
    >
      <div class="mx-auto mb-3 h-1 w-10 rounded-full bg-zinc-300 lg:hidden" aria-hidden="true"></div>
      <button
        id="close-action-panel"
        type="button"
        phx-click="deselect"
        aria-label="Close"
        class="absolute right-2 top-2 inline-flex size-9 items-center justify-center rounded-full text-zinc-500 transition hover:bg-zinc-100 hover:text-zinc-900 active:bg-zinc-200 pointer-coarse:size-11"
      >
        <.icon name="hero-x-mark" class="size-5" />
      </button>
      <div class="flex items-start gap-3 sm:gap-4">
        <img
          src={GameAssets.card_path(@card.name)}
          alt={@card.name}
          class="h-[147px] w-24 shrink-0 rounded-lg sm:h-[183px] sm:w-[120px] short:h-[98px] short:w-16"
        />
        <div class="flex min-w-0 flex-1 flex-col gap-3">
          <h3 id="action-panel-title" class="pr-10 text-lg font-semibold text-zinc-900">
            {@card.name}
          </h3>
          <.option_group
            id="build-options"
            title="Build"
            kind="build"
            prefix="build"
            card={@card.name}
            option={@card.build}
          />
          <.option_group
            id="wonder-options"
            title="Build wonder stage"
            kind="wonder_stage"
            prefix="wonder"
            card={@card.name}
            option={@card.wonder_stage}
          />
          <div :if={@card.free_build}>
            <button
              id="build-free-button"
              type="button"
              phx-click="submit"
              phx-value-card={@card.name}
              phx-value-kind="build_free"
              phx-value-option="0"
              class={@button_class}
            >
              Build for free (Olympía)
            </button>
          </div>
          <div>
            <button
              id="discard-button"
              type="button"
              phx-click="submit"
              phx-value-card={@card.name}
              phx-value-kind="discard"
              phx-value-option="0"
              class="inline-flex w-full items-center justify-center gap-1 rounded-lg bg-white px-3 py-2 text-sm font-semibold text-zinc-900 ring-1 ring-zinc-300 transition hover:bg-zinc-100 active:bg-zinc-200 sm:w-auto pointer-coarse:min-h-11"
            >
              <.icon name="hero-trash" class="size-4" /> Discard (+3 coins)
            </button>
          </div>
        </div>
      </div>
    </section>
```

In `option_group/1`, the `{:trade, options}` container class `"flex flex-wrap gap-2"` → `"flex flex-col gap-2 sm:flex-row sm:flex-wrap"`.

- [ ] **Step 6: The discard-picker sheet**

In `helios/lib/helios_web/components/game_components/extra_turn.ex` replace `discard_picker/1`'s `~H` with:

```heex
    <%= if my_discard_turn?(@view) do %>
      <div id="discard-scrim" class="fixed inset-0 z-40 bg-black/35" aria-hidden="true"></div>
      <section
        id="discard-picker"
        role="dialog"
        aria-modal="true"
        aria-labelledby="discard-picker-title"
        class={GameFormat.sheet_class()}
      >
        <h2 id="discard-picker-title" class="mb-3 font-semibold text-zinc-900">
          Build one card from the discard pile for free
        </h2>
        <div class="grid grid-cols-3 gap-2 sm:flex sm:flex-wrap">
          <button
            :for={{card, index} <- Enum.with_index(@view.discard_pile || [])}
            id={"discard-pick-#{index}"}
            type="button"
            phx-click="submit"
            phx-value-card={card}
            phx-value-kind="build_from_discard"
            phx-value-option="0"
            data-card={card}
            class="rounded-lg transition hover:-translate-y-1 hover:ring-4 hover:ring-sky-400 active:scale-95 active:ring-4 active:ring-sky-400"
          >
            <img
              src={GameAssets.card_path(card)}
              alt={card}
              class="aspect-[120/183] w-full rounded-lg sm:h-[183px] sm:w-[120px]"
            />
          </button>
        </div>
      </section>
    <% end %>
```

- [ ] **Step 7: The `deselect` event**

In `helios/lib/helios_web/live/game_live.ex`, add after the `"select_card"` clause:

```elixir
  # `phx-key="Escape"` filters keys in the browser; this guard keeps the server
  # safe from other keys (and hand-crafted events) too.
  def handle_event("deselect", %{"key" => key}, socket) when key != "Escape",
    do: {:noreply, socket}

  def handle_event("deselect", _params, socket), do: {:noreply, assign_selection(socket, nil)}
```

- [ ] **Step 8: Run to verify pass**

Run: `cd helios && mix test test/helios_web`
Expected: PASS.

Run: `lsof -ti :4004 | xargs kill 2>/dev/null; cd e2e && npx playwright test --project=responsive && npx playwright test game.spec.ts`
Expected: PASS in all projects (`game.spec.ts` runs in `chromium` and `mobile`).

- [ ] **Step 9: Precommit and commit**

Run: `cd helios && mix precommit` — Expected: PASS.

```bash
git add helios/lib/helios_web/game_format.ex helios/lib/helios_web/components/game_components/hand.ex helios/lib/helios_web/components/game_components/extra_turn.ex helios/lib/helios_web/live/game_live.ex helios/test/helios_web/components/game_components/hand_test.exs helios/test/helios_web/components/game_components/extra_turn_test.exs helios/test/helios_web/live/game_live_test.exs e2e/tests/responsive.spec.ts e2e/tests/game.spec.ts
git commit -m "feat(web): action and discard sheets with scrim, close button and Escape"
```

---

### Task 8: Scoreboard on phones

**Files:**
- Modify: `helios/lib/helios_web/components/game_components/scoreboard.ex`
- Test: `helios/test/helios_web/components/game_components/scoreboard_test.exs`
- Test: `e2e/tests/game.spec.ts`

**Interfaces:**
- Consumes: `expectNoHorizontalScroll` (Task 1).

- [ ] **Step 1: Write the failing ExUnit test**

Add to `helios/test/helios_web/components/game_components/scoreboard_test.exs`:

```elixir
  test "the player column stays put while the scores scroll sideways" do
    html =
      render_component(&Scoreboard.scoreboard/1,
        scores: SampleViews.scores(),
        names: SampleViews.names(),
        me: "1",
        lobby_id: "abc"
      )

    assert hd(attrs(html, "#scoreboard thead th:first-child", "class")) =~ "sticky"
    assert hd(attrs(html, "#score-row-1 td:first-child", "class")) =~ "sticky"
    # Sticky cells need an opaque row background to cover the scrolled columns.
    assert hd(attrs(html, "#score-row-1", "class")) =~ "bg-antique"
    assert hd(attrs(html, "#score-row-2", "class")) =~ "bg-amber-200"
  end
```

- [ ] **Step 2: Add the e2e regression guard**

In `e2e/tests/game.spec.ts`, import `expectNoHorizontalScroll` from `./support/layout`, and in "three players play a full game to the scoreboard" add inside the scoreboard `for` loop, after the `data-rank="1"` assertion:

```ts
    await expectNoHorizontalScroll(player.page);
    await expect(player.page.locator("#scoreboard tbody tr td:first-child").first()).toBeInViewport();
```

(This guard already passes today, since the table scrolls inside `overflow-x-auto`. It protects the sticky-column change and runs at phone size in the `mobile` project.)

- [ ] **Step 3: Run to verify failure**

Run: `cd helios && mix test test/helios_web/components/game_components/scoreboard_test.exs`
Expected: FAIL — no `sticky` class.

- [ ] **Step 4: Implement**

In `helios/lib/helios_web/components/game_components/scoreboard.ex`:
- `#scoreboard` class → `"mx-auto w-full max-w-4xl rounded-xl bg-antique/95 p-3 shadow-xl sm:p-6"`; `h2` class `text-2xl` → `text-xl sm:text-2xl`.
- The existing `<div class="overflow-x-auto">…</div>` (which holds the whole `<table>`) gets wrapped in `<div class="relative">`, and a fade element is added as that wrapper's second child, right after the `overflow-x-auto` div's closing tag:

```heex
        <div
          class="pointer-events-none absolute inset-y-0 right-0 w-6 bg-linear-to-l from-antique to-transparent sm:hidden"
          aria-hidden="true"
        >
        </div>
      </div>
```

  (The trailing `</div>` closes the new `relative` wrapper. The `<table>` markup itself only changes in the three places listed next.)

- First header cell: `<th class="sticky left-0 z-10 bg-antique px-2 py-2 text-left">Player</th>`.
- Row class list:

```heex
              class={[
                "border-b border-zinc-200",
                if(score.rank == 1, do: "bg-amber-200 font-semibold", else: "bg-antique"),
                score.player == @me && "outline-2 outline-sky-500"
              ]}
```

- First body cell: `<td class="sticky left-0 z-10 max-w-36 truncate bg-inherit px-2 py-2 text-left shadow-[4px_0_6px_-4px_rgba(0,0,0,0.25)] sm:max-w-none sm:shadow-none">` (content unchanged).
- `#back-to-lobby` class → `"inline-flex w-full items-center justify-center rounded-lg bg-zinc-900 px-4 py-2 font-semibold text-white transition hover:bg-zinc-700 active:bg-zinc-800 sm:w-auto pointer-coarse:min-h-11"`.

- [ ] **Step 5: Run to verify pass**

Run: `cd helios && mix test test/helios_web/components/game_components/scoreboard_test.exs test/helios_web/live/game_live_extra_turns_test.exs test/helios_web/live/game_live_test.exs`
Expected: PASS (still 10 header cells; the winner row keeps `bg-amber-200`).

Run: `lsof -ti :4004 | xargs kill 2>/dev/null; cd e2e && npx playwright test game.spec.ts -g "full game"`
Expected: PASS in `chromium` and `mobile` (about 4 minutes each).

- [ ] **Step 6: Precommit and commit**

Run: `cd helios && mix precommit` — Expected: PASS.

```bash
git add helios/lib/helios_web/components/game_components/scoreboard.ex helios/test/helios_web/components/game_components/scoreboard_test.exs e2e/tests/game.spec.ts
git commit -m "feat(web): sticky player column and full-width actions on the phone scoreboard"
```

---

### Task 9: Full verification and visual review

**Files:**
- No planned source changes. Fixes found here go in the file that owns them, each with a test in the style of the task that introduced it.

- [ ] **Step 1: Run every check**

Run: `cd helios && mix precommit`
Expected: PASS.

Run: `lsof -ti :4004 | xargs kill 2>/dev/null; cd e2e && npx playwright test`
Expected: PASS in all three projects (`chromium`, `mobile`, `responsive`).

- [ ] **Step 2: Screenshot every screen for review**

Start the e2e server (`lsof -ti :4004 | xargs kill 2>/dev/null; cd helios && MIX_ENV=e2e PORT=4004 mix do assets.build + ecto.reset + phx.server`, in the background). Then create `/tmp/rwd-shots/shots.mjs` (outside the repo) and run it from `e2e/` with `node /tmp/rwd-shots/shots.mjs` after `ln -s "$PWD/node_modules" /tmp/rwd-shots/node_modules`:

```js
import { chromium } from "@playwright/test";

const BASE = "http://localhost:4004";
const OUT = "/tmp/rwd-shots";
const sizes = [
  ["phone", { width: 360, height: 740 }, true],
  ["landscape", { width: 740, height: 360 }, true],
  ["desktop", { width: 1280, height: 800 }, false],
];
const browser = await chromium.launch();

async function player(name, viewport, touch) {
  const context = await browser.newContext({ viewport, isMobile: touch, hasTouch: touch, deviceScaleFactor: 2 });
  const page = await context.newPage();
  await page.goto(`${BASE}/login`);
  await page.locator("[data-phx-main].phx-connected").waitFor();
  await page.getByLabel("Access Token", { exact: true }).fill("e2e");
  await page.getByLabel("Name", { exact: true }).fill(name);
  await page.locator("#login-submit").click();
  await page.waitForURL(/\/lobby\//);
  await page.locator("[data-phx-main].phx-connected").waitFor();
  return page;
}

for (const [label, viewport, touch] of sizes) {
  const id = Math.random().toString(36).slice(2, 7);
  const login = await (await browser.newContext({ viewport, isMobile: touch, hasTouch: touch })).newPage();
  await login.goto(`${BASE}/login`);
  await login.screenshot({ path: `${OUT}/${label}-login.png` });

  const a = await player(`owner_with_long_name_${id}`.slice(0, 24), viewport, touch);
  const guests = [await player(`b_${id}`, viewport, touch), await player(`c_${id}`, viewport, touch)];
  for (const g of guests) {
    const name = await g.locator("#current-user-name").innerText();
    await a.locator("#invite_user_id").selectOption({ label: name });
    await a.locator("#invite-button").click();
    await g.getByRole("button", { name: "Accept" }).click();
    await g.waitForURL(a.url());
  }
  await a.screenshot({ path: `${OUT}/${label}-lobby.png`, fullPage: true });
  await a.locator("#start-game").click();
  await a.waitForURL(/\/game\//);
  await a.locator('#top-bar[data-turn-key="1-1"]').waitFor();
  await a.screenshot({ path: `${OUT}/${label}-game.png` });
  await a.screenshot({ path: `${OUT}/${label}-game-full.png`, fullPage: true });
  await a.locator("#hand [data-card]").first().click();
  await a.screenshot({ path: `${OUT}/${label}-sheet.png` });
}
await browser.close();
```

Open each PNG and check against the spec: nothing cut off or overlapping, the dock and sheet look intentional, long names truncate cleanly, and the landscape layout isn't cramped. The scoreboard and the discard picker aren't reachable from this script. The scoreboard is covered at phone size by `game.spec.ts` in the `mobile` project; the discard picker by ExUnit only (the e2e env seats no Halikarnassós B). Fix any visual defect in its owning component and rerun Step 1. Stop the server afterwards.

- [ ] **Step 3: Commit any fixes**

Commit each fix separately with explicit paths, e.g.:

```bash
git add helios/lib/helios_web/components/game_components/hand.ex
git commit -m "fix(web): <what the screenshot showed>"
```

If there was nothing to fix, there is nothing to commit.

- [ ] **Step 4: Whole-branch review**

Request a whole-branch review (superpowers:requesting-code-review) against the spec, the Global Constraints and the Review Focus list above, then address its findings. Do not push or open a PR without asking the user.
