import { expect, test, type Page } from "@playwright/test";
import {
  closeAll,
  playFirstBuildableOrDiscard,
  setupTable,
  startGame,
  waitForTurn,
} from "./support/game";
import {
  LONG_NAME_PREFIX,
  boxOf,
  expectNoHorizontalScroll,
  expectTapTargets,
} from "./support/layout";

/**
 * Boxes of every compact ring board (everything but mine) in document
 * coordinates (rect + scroll). boundingBox() is viewport-relative, so the
 * click's scroll-into-view (the west toggle starts below the 800px fold at
 * 1280x800) shifts every y without a layout change; document coordinates
 * cancel that harness scroll while a real shift still moves the rect — the
 * same rationale as the phone test's #seats-relative deltas.
 */
async function compactBoxes(page: Page) {
  const sections = page.locator("#seats > section:not(#my-board)");
  const count = await sections.count();
  const boxes: Record<string, { x: number; y: number; width: number; height: number }> = {};
  for (let i = 0; i < count; i++) {
    const section = sections.nth(i);
    const id = (await section.getAttribute("id"))!;
    boxes[id] = await section.evaluate((el) => {
      const r = el.getBoundingClientRect();
      return {
        x: r.x + window.scrollX,
        y: r.y + window.scrollY,
        width: r.width,
        height: r.height,
      };
    });
  }
  return boxes;
}

async function expectBoxesUnchanged(
  page: Page,
  before: Record<string, { x: number; y: number; width: number; height: number }>,
) {
  const after = await compactBoxes(page);
  expect(Object.keys(after).sort()).toEqual(Object.keys(before).sort());
  for (const [id, box] of Object.entries(before)) {
    expect(after[id], `${id} moved`).toEqual(box);
  }
}

test.describe("desktop 1280x800, 4 players: ring and nothing moves", () => {
  test.use({ viewport: { width: 1280, height: 800 }, hasTouch: false, isMobile: false });

  test("ring, exclusive panels and open across a turn", async ({ browser }) => {
    test.setTimeout(180_000);
    const { owner, players } = await setupTable(browser, 4, LONG_NAME_PREFIX);
    await startGame(owner, players);
    const page = owner.page;
    await waitForTurn(page, 1, 1);

    await expect(page.locator("#table-ring")).toBeVisible();
    await expect(page.locator("#table-ring path")).toHaveCount(1);
    await expect(page.locator("#seats > section")).toHaveCount(4);

    // One label of each role around the ring.
    for (const label of ["West", "East", "Opposite"]) {
      await expect(page.locator(`#seats > section header span:text-is("${label}")`)).toHaveCount(1);
    }

    // Opening a panel leaves every other board's box untouched.
    const before = await compactBoxes(page);
    await page.locator("#west-panel-toggle").click();
    await expect(page.locator("#west-panel-details")).toBeVisible();
    await expectBoxesUnchanged(page, before);

    // Exclusive on wide screens: east opens and west closes; east stays inside.
    await page.locator("#east-panel-toggle").click();
    await expect(page.locator("#west-panel-details")).toBeHidden();
    await expect(page.locator("#east-panel-details")).toBeVisible();
    const east = await boxOf(page.locator("#east-panel"));
    expect(east.x + east.width).toBeLessThanOrEqual(1280);
    expect(east.x).toBeGreaterThanOrEqual(0);
    await expectNoHorizontalScroll(page);

    // A tile inside the open panel opens the card sheet; the panel stays open.
    // No one has built yet at turn 1 — play a full round first so boards have tiles.
    for (const player of players) {
      await waitForTurn(player.page, 1, 1);
      await playFirstBuildableOrDiscard(player.page);
    }
    await waitForTurn(page, 1, 2);
    // A tile in the open east panel opens the card sheet; the panel stays open.
    const tile = page.locator("#east-panel-details [data-card]").first();
    await expect(tile).toHaveCount(1);
    await tile.click();
    await expect(page.locator("#card-sheet")).toBeVisible();
    await expect(page.locator("#east-panel")).toHaveClass(/is-open/);
    await page.keyboard.press("Escape");
    await expect(page.locator("#card-sheet")).toHaveCount(0);
    await expect(page.locator("#east-panel")).toHaveClass(/is-open/);

    // Clicking outside the panel closes it.
    await page.locator("#age-label").click();
    await expect(page.locator("#east-panel-details")).toBeHidden();

    // Open west, all play a turn; the panel stays open and boxes hold.
    const beforeTurn = await compactBoxes(page);
    await page.locator("#west-panel-toggle").click();
    await expect(page.locator("#west-panel-details")).toBeVisible();
    for (const player of players) {
      await waitForTurn(player.page, 1, 2);
      await playFirstBuildableOrDiscard(player.page);
    }
    await waitForTurn(page, 1, 3);
    await expect(page.locator("#west-panel-details")).toBeVisible();
    await expectBoxesUnchanged(page, beforeTurn);

    // Compact summaries all share the fixed height.
    const heights = await page
      .locator("#seats > section:not(#my-board) [id$='-summary']")
      .evaluateAll((els) => els.map((el) => el.getBoundingClientRect().height));
    expect(heights.length).toBe(3);
    for (const h of heights) expect(h).toBe(heights[0]);

    await closeAll(players);
  });
});

/** Worst-case DOM: 7 [data-category] columns × 10 tiles in #my-built, 7 chips per compact toggle. */
function injectWorstCaseBoard() {
  // Inlined because page.evaluate serializes only this function's source.
  const categories = [
    "raw_material",
    "manufactured_good",
    "civilian",
    "commercial",
    "military",
    "scientific",
    "guild",
  ];
  // Two-digit counts on the busiest chips: the widest plausible chip row.
  const chipCounts: Array<[string, string]> = [
    ["raw_material", "12"],
    ["manufactured_good", "7"],
    ["civilian", "4"],
    ["commercial", "3"],
    ["military", "2"],
    ["scientific", "2"],
    ["guild", "1"],
  ];

  const myBuilt = document.querySelector("#my-built");
  if (!myBuilt) throw new Error("#my-built not found");

  const existingTile = myBuilt.querySelector("[data-category] > *");
  let template: Element;
  if (existingTile) {
    template = existingTile;
  } else {
    const handImg = document.querySelector("#hand [data-card] img");
    if (!handImg) throw new Error("no #hand [data-card] img to clone");
    const tile = document.createElement("div");
    tile.className = "h-10 w-16 overflow-hidden rounded";
    const img = handImg.cloneNode(true) as HTMLImageElement;
    img.className = "w-full object-cover object-top";
    tile.appendChild(img);
    template = tile;
  }

  myBuilt.replaceChildren();
  for (const category of categories) {
    const column = document.createElement("div");
    column.setAttribute("data-category", category);
    column.className = "flex shrink-0 flex-col gap-1";
    for (let i = 0; i < 10; i++) {
      const tile = template.cloneNode(true) as HTMLElement;
      tile.removeAttribute("id");
      for (const el of tile.querySelectorAll("[id]")) el.removeAttribute("id");
      column.appendChild(tile);
    }
    myBuilt.appendChild(column);
  }

  let chips = 0;
  for (const section of document.querySelectorAll("#seats > section:not(#my-board)")) {
    const holder = section.querySelector("[data-board-toggle] > span");
    if (!holder) throw new Error(`${section.id}: toggle chips holder not found`);
    for (const [category, count] of chipCounts) {
      if (holder.querySelector(`[data-chip="${category}"]`)) continue;
      const chip = document.createElement("span");
      chip.setAttribute("data-chip", category);
      chip.className =
        "flex shrink-0 items-center gap-0.5 rounded-full px-1.5 py-0.5 text-xs font-semibold text-white bg-zinc-500";
      chip.textContent = count;
      holder.appendChild(chip);
      chips++;
    }
  }

  return { columns: myBuilt.querySelectorAll("[data-category]").length, chips };
}

type TableFit = {
  cardsFit: boolean;
  built: [number, number];
  overlaps: string[];
  chips: string[];
  vp: number;
  win: number;
  seats: number;
  west: number;
};

/** The three measurement checks at the current viewport: cards, board boxes, chips. */
function measureTableFit(): TableFit {
  const EPS = 1;
  const docWidth = document.documentElement.clientWidth;

  const myBuilt = document.querySelector("#my-built");
  if (!myBuilt) throw new Error("#my-built not found");
  const cardsFit = myBuilt.scrollWidth <= myBuilt.clientWidth + EPS;

  const sections = [...document.querySelectorAll("#seats > section")];
  const boxes = sections.map((section) => {
    const r = section.getBoundingClientRect();
    return { id: section.id, left: r.left, right: r.right, top: r.top, bottom: r.bottom };
  });
  const overlaps: string[] = [];
  for (let i = 0; i < boxes.length; i++) {
    for (let j = i + 1; j < boxes.length; j++) {
      const a = boxes[i];
      const b = boxes[j];
      const overlapX = Math.min(a.right, b.right) - Math.max(a.left, b.left);
      const overlapY = Math.min(a.bottom, b.bottom) - Math.max(a.top, b.top);
      if (overlapX > EPS && overlapY > EPS) overlaps.push(`${a.id} x ${b.id}`);
    }
  }

  const chipProblems: string[] = [];
  for (const section of sections) {
    if (section.id === "my-board") continue;
    const summary = section.querySelector("[id$='-summary']");
    if (!summary) {
      chipProblems.push(`${section.id}: no summary box`);
      continue;
    }
    const s = summary.getBoundingClientRect();

    const toggle = section.querySelector("[data-board-toggle]");
    if (toggle) {
      const t = toggle.getBoundingClientRect();
      if (t.left < s.left - EPS || t.right > s.right + EPS || t.top < s.top - EPS || t.bottom > s.bottom + EPS) {
        chipProblems.push(`${section.id}: toggle clipped by summary`);
      }
    }

    const holder = section.querySelector("[data-board-toggle] > span");
    if (holder && holder.scrollWidth > holder.clientWidth + EPS) {
      chipProblems.push(`${section.id}: chips clipped (${holder.scrollWidth} > ${holder.clientWidth})`);
    }

    for (const chip of section.querySelectorAll("[data-chip]")) {
      const c = chip.getBoundingClientRect();
      const name = `${section.id}[${chip.getAttribute("data-chip")}]`;
      if (c.width <= 0 || c.height <= 0) {
        chipProblems.push(`${name}: not rendered`);
      } else if (c.left < s.left - EPS || c.right > s.right + EPS || c.top < s.top - EPS || c.bottom > s.bottom + EPS) {
        chipProblems.push(`${name}: outside summary box`);
      } else if (c.left < -EPS || c.right > docWidth + EPS) {
        chipProblems.push(`${name}: outside viewport`);
      }
    }
  }

  return {
    cardsFit,
    built: [myBuilt.scrollWidth, myBuilt.clientWidth],
    overlaps,
    chips: chipProblems,
    vp: document.documentElement.clientWidth,
    win: window.innerWidth,
    seats: (document.querySelector("#seats") as HTMLElement | null)?.clientWidth ?? -1,
    west: (document.querySelector("#west-panel") as HTMLElement | null)?.clientWidth ?? -1,
  };
}

test.describe("measurement 1280-2400x1100, 7 players: worst-case fit", () => {
  test.use({ viewport: { width: 1280, height: 1100 }, hasTouch: false, isMobile: false });

  test("7 players, fully built own board fits at the max width", async ({ browser }) => {
    test.setTimeout(300_000);
    const { owner, players } = await setupTable(browser, 7, LONG_NAME_PREFIX);
    await startGame(owner, players);
    const page = owner.page;
    await waitForTurn(page, 1, 1);

    const injected = await page.evaluate(injectWorstCaseBoard);
    expect(injected.columns, "worst-case built columns were not injected").toBe(7);

    let firstFit: number | null = null;
    let firstFailure = "";
    let lastFailure = "";
    for (let width = 1280; width <= 2400; width += 32) {
      await page.setViewportSize({ width, height: 1100 });
      const state = await page.evaluate(measureTableFit);
      if (state.cardsFit && state.overlaps.length === 0 && state.chips.length === 0) {
        firstFit = width;
        break;
      }
      const line = `width ${width}: vp ${state.vp} win ${state.win} seats ${state.seats} west ${state.west}, built ${state.built[0]}/${state.built[1]}, overlaps [${state.overlaps.join("; ")}], chips [${state.chips.join("; ")}]`;
      if (!firstFailure) firstFailure = line;
      lastFailure = line;
    }

    if (firstFailure) console.log(`first failing width: ${firstFailure}`);
    if (firstFit === null) console.log(`no fitting width; last: ${lastFailure}`);
    else console.log(`first-fit width: ${firstFit}px`);
    expect(firstFit, `no width in 1280..2400 fits (${firstFailure})`).not.toBeNull();
    const fit = firstFit as number;

    // The injected DOM survived the sweep — otherwise the checks went vacuous.
    expect(await page.locator("#my-built [data-category]").count(), "injected built columns wiped").toBe(7);
    expect(
      await page.locator("#seats > section:not(#my-board) [data-chip]").count(),
      "injected toggle chips wiped",
    ).toBe(42); // 6 compact boards x 7 chips

    const configured = await page.evaluate(() => {
      const container = [...document.querySelectorAll("#game > div")].find(
        (el) => getComputedStyle(el).maxWidth !== "none",
      );
      if (!container) throw new Error("#game container with a max-width not found");
      const style = getComputedStyle(container);
      return {
        max: parseFloat(style.maxWidth),
        padding: parseFloat(style.paddingLeft) + parseFloat(style.paddingRight),
      };
    });
    const configuredWidth = Math.round(configured.max + configured.padding);
    console.log(
      `configured container: max-width ${configured.max}px + ${configured.padding}px padding = ${configuredWidth}px`,
    );
    expect(configuredWidth, "configured table width below the measured first-fit width").toBeGreaterThanOrEqual(fit);

    for (const width of [Math.round(configured.max), configuredWidth]) {
      await page.setViewportSize({ width, height: 1100 });
      const state = await page.evaluate(measureTableFit);
      expect(state.cardsFit, `#my-built scrolls horizontally at ${width}px (built ${state.built[0]}/${state.built[1]})`).toBe(
        true,
      );
      expect(state.overlaps, `boards intersect at ${width}px`).toEqual([]);
      expect(state.chips, `toggle chips not fully visible at ${width}px`).toEqual([]);
    }

    await closeAll(players);
  });
});

test.describe("phone 390x844, 4 players: ring order and in-place expansion", () => {
  test.use({ viewport: { width: 390, height: 844 }, hasTouch: true, isMobile: true });

  test("list order, in-place expand, both stay open", async ({ browser }) => {
    test.setTimeout(180_000);
    const { owner, players } = await setupTable(browser, 4, LONG_NAME_PREFIX);
    await startGame(owner, players);
    const page = owner.page;
    await waitForTurn(page, 1, 1);

    // Narrow order: W, me, E, then around.
    const ids = await page.locator("#seats > section").evaluateAll((els) =>
      els.map((el) => (el as HTMLElement).id),
    );
    const opp = ids.find((id) => id.startsWith("board-"));
    expect(ids).toEqual(["west-panel", "my-board", "east-panel", opp]);

    await expect(page.locator("#table-ring")).toBeHidden();
    await expectNoHorizontalScroll(page);

    // West expands in place: west stays put relative to #seats, me is pushed
    // down. Compare deltas against #seats so a scroll between the two
    // boundingBox measurements cancels out.
    const seatsBefore = await boxOf(page.locator("#seats"));
    const westRel = (await boxOf(page.locator("#west-panel"))).y - seatsBefore.y;
    const meRel = (await boxOf(page.locator("#my-board"))).y - seatsBefore.y;
    await page.locator("#west-panel-toggle").click();
    await expect(page.locator("#west-panel-details")).toBeVisible();
    const seatsAfter = await boxOf(page.locator("#seats"));
    expect((await boxOf(page.locator("#west-panel"))).y - seatsAfter.y).toBe(westRel);
    expect((await boxOf(page.locator("#my-board"))).y - seatsAfter.y).toBeGreaterThan(meRel);

    // Narrow keeps both open.
    await page.locator("#east-panel-toggle").click();
    await expect(page.locator("#west-panel-details")).toBeVisible();
    await expect(page.locator("#east-panel-details")).toBeVisible();

    // Both survive another player's move.
    await playFirstBuildableOrDiscard(players[1].page);
    await expect(page.locator("#west-panel-details")).toBeVisible();
    await expect(page.locator("#east-panel-details")).toBeVisible();

    await expectTapTargets(page.locator("#west-panel-toggle, #east-panel-toggle"));
    await expectNoHorizontalScroll(page);

    await closeAll(players);
  });
});
