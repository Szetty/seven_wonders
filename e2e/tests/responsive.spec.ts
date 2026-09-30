import { expect, test } from "@playwright/test";
import { closeAll, newPlayer, setupTable } from "./support/game";
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
  });
}
