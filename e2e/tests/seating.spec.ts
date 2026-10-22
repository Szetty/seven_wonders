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

/** Boxes of every compact ring board (everything but mine). */
async function compactBoxes(page: Page) {
  const sections = page.locator("#seats > section:not(#my-board)");
  const count = await sections.count();
  const boxes: Record<string, { x: number; y: number; width: number; height: number }> = {};
  for (let i = 0; i < count; i++) {
    const section = sections.nth(i);
    const id = (await section.getAttribute("id"))!;
    boxes[id] = await boxOf(section);
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
