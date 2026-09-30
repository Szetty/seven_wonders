import { expect, test } from "@playwright/test";
import {
  closeAll,
  newPlayer,
  playFirstBuildableOrDiscard,
  setupTable,
  startGame,
  waitForTurn,
} from "./support/game";
import {
  LONG_NAME_PREFIX,
  VIEWPORTS,
  boxOf,
  expectNoHorizontalScroll,
  expectTapTargets,
  expectViewport,
} from "./support/layout";
import { notification } from "./support/lobby";

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
      // "7 WONDERS" stays on one line.
      const title = page.locator("#login-title");
      const lineHeight = await title.evaluate((el) => parseFloat(getComputedStyle(el).lineHeight));
      expect((await boxOf(title)).height).toBeLessThanOrEqual(lineHeight * 1.2);
    });

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

      await closeAll(players);
    });
  });
}

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
