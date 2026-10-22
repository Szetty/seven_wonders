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
      if (vp.touch)
        await expectTapTargets(page.locator("#login-submit, #login_form input:not([type='hidden'])"));
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
        // The members table shrinks to its wrapper instead of clipping the Delete column.
        const table = await boxOf(page.locator("#members-table"));
        const wrapper = await boxOf(page.locator("#members-table").locator("xpath=./.."));
        expect(table.x + table.width).toBeLessThanOrEqual(wrapper.x + wrapper.width + 1);
      }
      // Review Focus 5: a guest's header with the Recent tables button still fits.
      const guestPage = players[1].page;
      await expect(guestPage.locator("#recent-tables-button")).toBeVisible();
      await expectNoHorizontalScroll(guestPage);
      for (const id of ["#recent-tables-button", "#my-table-link", "#logout-link"]) {
        await expect(guestPage.locator(id)).toBeInViewport({ ratio: 1 });
      }
      if (vp.touch) await expectTapTargets(guestPage.locator("#recent-tables-button"));
      if (vp.touch) {
        await expectTapTargets(page.locator("[id^='uninvite-']"));
        await expectTapTargets(page.locator("#start-game, #invite-button, #invite_user_id"));
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
        // innerText cannot see ellipsis clipping; content must fit the element box.
        const passFits = await page
          .locator("#pass-direction")
          .evaluate((el) => el.scrollWidth <= el.clientWidth);
        expect(passFits, "pass-direction content overflows (truncated or clipped)").toBe(true);

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

      // --- table: details are hidden until the toggle is pressed on every size
      // (ring layout); open panels stay open across live updates.
      await expect(page.locator("#west-panel-details")).toBeHidden();
      await page.locator("#west-panel-toggle").click();
      await expect(page.locator("#west-panel-details")).toBeVisible();
      await expect(page.locator("#west-panel-toggle")).toHaveAttribute("aria-expanded", "true");
      if (vp.touch) await expectTapTargets(page.locator("#west-panel-toggle, #east-panel-toggle"));

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
      if (vp.width >= 640) {
        // Disabled options shrink to fit their group like every other button.
        const sheetBox = await boxOf(sheet);
        for (const button of await sheet.locator("button[disabled]:visible").all()) {
          expect((await boxOf(button)).width).toBeLessThanOrEqual(sheetBox.width - 64);
        }
      }
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
    expect((await boxOf(page.locator("#top-bar"))).height).toBeLessThanOrEqual(120);
    const sections = page.locator("#seats > section");
    await expect(sections).toHaveCount(7);
    const ids = await sections.evaluateAll((els) => els.map((el) => (el as HTMLElement).id));
    expect(ids.slice(0, 3)).toEqual(["west-panel", "my-board", "east-panel"]);
    for (const section of await sections.all()) {
      const box = await boxOf(section);
      expect(box.x + box.width, `${section} overflows`).toBeLessThanOrEqual(360);
    }
    await closeAll(players);
  });
});
