import { expect, test } from "@playwright/test";
import { newPlayer, playFirstBuildableOrDiscard, waitForLive, waitForTurn } from "./support/game";
import { expectNoHorizontalScroll } from "./support/layout";

test("the owner seats AI players; they move, and a slow one is tagged random", async ({ browser }) => {
  test.setTimeout(120_000);
  const owner = await newPlayer(browser, "ai");
  const page = owner.page;
  await waitForLive(page);

  for (const model of ["fake-first", "fake-timeout"]) {
    await page.locator("#ai-seat-form select").selectOption(model);
    await page.locator("#add-ai-seat").click();
    await expect(page.locator(`#members-table [data-ai-seat][data-model='${model}']`)).toHaveCount(1);
  }
  await expectNoHorizontalScroll(page);

  await expect(page.locator("#start-game")).toBeEnabled();
  await page.locator("#start-game").click();
  await expect(page).toHaveURL(/\/game\/[0-9a-f-]{36}$/);
  await waitForLive(page);

  await waitForTurn(page, 1, 1);
  await expect(page.locator("#connection-ai-1[data-connected='true']")).toBeVisible();
  await expect(page.locator("[data-player='ai-1']").first()).toContainText("Fake AI");

  await playFirstBuildableOrDiscard(page);
  await waitForTurn(page, 1, 2);

  await expect(page.locator("#random-badge-ai-2")).toBeVisible();
  await expect(page.locator("#random-badge-ai-1")).toHaveCount(0);
  await expectNoHorizontalScroll(page);
  await page.locator("#history-button").click();
  await expect(page.locator("#history-drawer [data-random]")).toHaveCount(1);

  await owner.context.close();
});
