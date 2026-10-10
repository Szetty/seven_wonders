import { expect, test } from "@playwright/test";
import {
  builtCards,
  chooseBuild,
  handCards,
  playFirstBuildableOrDiscard,
  setupTable,
  startGame,
  waitForLive,
  waitForTurn,
} from "./support/game";
import { expectNoHorizontalScroll } from "./support/layout";

test("three players play a full game to the scoreboard", async ({ browser }) => {
  test.setTimeout(240_000);
  const { owner, players } = await setupTable(browser, 3);
  await startGame(owner, players);

  for (let age = 1; age <= 3; age++) {
    for (let turn = 1; turn <= 6; turn++) {
      for (const player of players) {
        await waitForTurn(player.page, age, turn);
        await playFirstBuildableOrDiscard(player.page);
      }
    }
  }

  for (const player of players) {
    await expect(player.page.locator("#scoreboard")).toBeVisible();
    await expect(player.page.locator("#scoreboard tbody tr")).toHaveCount(3);
    await expect(player.page.locator('#scoreboard tbody tr[data-rank="1"]').first()).toBeVisible();
    await expectNoHorizontalScroll(player.page);
    await expect(player.page.locator("#scoreboard tbody tr td:first-child").first()).toBeInViewport();
  }

  await owner.page.locator("#past-games-link").click();
  await expect(owner.page).toHaveURL(/\/games$/);
  const row = owner.page.locator("[id^='past-game-']").first();
  await expect(row).toBeVisible();
  await row.locator("a").click();
  await expect(owner.page.locator("#scoreboard")).toBeVisible();
  await expect(owner.page.locator("#table #my-board")).toBeVisible();

  await expect(owner.page.locator("#replay-position")).toHaveText("Final");
  await owner.page.locator("#replay-first").click();
  await expect(owner.page.locator("#replay-position")).toHaveText("Age I · Turn 1");
  await expect(owner.page.locator("#replay-hand [data-card]")).toHaveCount(7);
  await owner.page.locator("#replay-next").click();
  await expect(owner.page.locator("#replay-position")).toHaveText("Age I · Turn 2");

  for (const player of players) await player.context.close();
});

test("a player who reloads keeps their hand and board and play continues", async ({ browser }) => {
  const { owner, players } = await setupTable(browser, 3);
  await startGame(owner, players);

  for (const player of players) {
    await waitForTurn(player.page, 1, 1);
    await playFirstBuildableOrDiscard(player.page);
  }

  const b = players[1];
  await waitForTurn(b.page, 1, 2);
  const handBefore = await handCards(b.page);
  const builtBefore = await builtCards(b.page);

  await b.page.reload();
  await waitForLive(b.page);
  await waitForTurn(b.page, 1, 2);
  expect(await handCards(b.page)).toEqual(handBefore);
  expect(await builtCards(b.page)).toEqual(builtBefore);

  for (const player of players) {
    await waitForTurn(player.page, 1, 2);
    await playFirstBuildableOrDiscard(player.page);
  }
  for (const player of players) await waitForTurn(player.page, 1, 3);

  for (const player of players) await player.context.close();
});

test("a player can change their choice before the turn resolves", async ({ browser }) => {
  const { owner, players } = await setupTable(browser, 3);
  await startGame(owner, players);
  const [a, b, c] = players;

  await waitForTurn(a.page, 1, 1);
  const first = a.page.locator("#hand [data-card]").first();
  const discarded = await first.getAttribute("data-card");
  await first.click();
  await a.page.locator("#discard-button").click();
  await expect(a.page.locator("#pending-choice")).toContainText(`Discard ${discarded}`);

  await a.page.locator("#change-choice").click();
  const buildable = a.page.locator('#hand [data-buildable="true"]').first();
  const built = await buildable.getAttribute("data-card");
  if (built !== discarded) {
    // The sheet's scrim covers the dock: close it before tapping another card.
    await a.page.locator("#close-action-panel").click();
    await buildable.click();
  }
  await chooseBuild(a.page);
  await expect(a.page.locator("#pending-choice")).toContainText(`Build ${built}`);

  for (const player of [b, c]) {
    await waitForTurn(player.page, 1, 1);
    await playFirstBuildableOrDiscard(player.page);
  }

  await waitForTurn(a.page, 1, 2);
  await expect(a.page.locator(`#my-built [data-card="${built}"]`)).toBeVisible();

  for (const player of players) await player.context.close();
});

test("Start is disabled until three players are connected", async ({ browser }) => {
  const { owner, players } = await setupTable(browser, 2);
  await expect(owner.page.locator("#start-game")).toBeDisabled();
  await expect(owner.page.locator("#start-blocker")).toHaveText("Need at least 3 connected players");

  for (const player of players) await player.context.close();
});
