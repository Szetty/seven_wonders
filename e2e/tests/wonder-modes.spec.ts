import { expect, test } from "@playwright/test";
import { playFirstBuildableOrDiscard, setupTable, startGame, waitForTurn } from "./support/game";

test("choose board: everyone claims and confirms, then Age I begins", async ({ browser }) => {
  test.setTimeout(120_000);
  const { owner, guests, players } = await setupTable(browser, 3);

  await owner.page.locator("#wonder-mode-choose_board").click();
  await expect(owner.page.locator("#wonder-mode-choose_board")).toHaveAttribute("aria-pressed", "true");
  for (const guest of guests) {
    await expect(guest.page.locator("#wonder-mode")).toHaveAttribute("data-mode", "choose_board");
  }

  await startGame(owner, players);
  for (const player of players) {
    await expect(player.page.locator("#top-bar")).toHaveAttribute("data-phase", "setup");
    await expect(player.page.locator("#age-label")).toHaveText("Choosing boards");
    await expect(player.page.locator("#confirm-wonder")).toBeDisabled();
  }

  const [a, b, c] = players;
  await a.page.locator("#claim-gizah").click();
  await expect(a.page.locator("#setup-wonder-gizah[data-mine]")).toHaveCount(1);
  await expect(b.page.locator("#claim-gizah")).toBeDisabled();

  await b.page.locator("#side-rhodos-b").click();
  await expect(b.page.locator("#setup-wonder-rhodos")).toHaveAttribute("data-side", "b");
  await b.page.locator("#claim-rhodos").click();
  await expect(b.page.locator("#setup-wonder-rhodos[data-mine]")).toHaveCount(1);

  await c.page.locator("#claim-ephesos").click();
  await expect(c.page.locator("#setup-wonder-ephesos[data-mine]")).toHaveCount(1);

  for (const player of players) {
    await expect(player.page.locator("#confirm-wonder")).toBeEnabled();
    await player.page.locator("#confirm-wonder").click();
  }

  for (const player of players) {
    await expect(player.page.locator("#top-bar")).toHaveAttribute("data-phase", "choosing_cards");
    await waitForTurn(player.page, 1, 1);
    await expect(player.page.locator("#hand [data-card]")).toHaveCount(7);
  }
  await expect(a.page.locator("#my-board img[alt='Gizah A']")).toBeVisible();
  await expect(b.page.locator("#my-board img[alt='Rhódos B']")).toBeVisible();
  await expect(c.page.locator("#my-board img[alt='Éphesos A']")).toBeVisible();

  for (const player of players) await playFirstBuildableOrDiscard(player.page);
  for (const player of players) await waitForTurn(player.page, 1, 2);

  for (const player of players) await player.context.close();
});
