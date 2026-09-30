import { Browser, BrowserContext, Page, expect } from "@playwright/test";
import { login, uniqueName } from "./auth";
import { acceptInvite, invite } from "./lobby";

export type Player = { context: BrowserContext; page: Page; name: string };

export async function newPlayer(browser: Browser, prefix: string): Promise<Player> {
  const context = await browser.newContext();
  const page = await context.newPage();
  const name = uniqueName(prefix);
  await login(page, name);
  await expect(page).toHaveURL(/\/lobby\/[0-9a-f-]{36}$/);
  return { context, page, name };
}

export async function inviteAndAccept(owner: Player, guest: Player): Promise<void> {
  const lobbyUrl = owner.page.url();
  await invite(owner.page, guest.name);
  await expect(guest.page.getByText(`You are expected on table ${owner.name}`)).toBeVisible();
  await acceptInvite(guest.page, owner.name);
  await expect(guest.page).toHaveURL(lobbyUrl);
  await waitForLive(guest.page);
}

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

export async function startGame(owner: Player, players: Player[]): Promise<void> {
  await expect(owner.page.locator("#start-game")).toBeEnabled();
  await owner.page.locator("#start-game").click();
  for (const player of players) {
    await expect(player.page).toHaveURL(/\/game\/[0-9a-f-]{36}$/);
    await waitForLive(player.page);
  }
}

export async function waitForLive(page: Page): Promise<void> {
  await page.locator("[data-phx-main].phx-connected").waitFor();
}

export async function waitForTurn(page: Page, age: number, turn: number): Promise<void> {
  await expect(page.locator("#top-bar")).toHaveAttribute("data-turn-key", `${age}-${turn}`);
}

export async function handCards(page: Page): Promise<string[]> {
  return page.locator("#hand [data-card]").evaluateAll((els) => els.map((el) => (el as HTMLElement).dataset.card ?? ""));
}

export async function builtCards(page: Page): Promise<string[]> {
  return page.locator("#my-built [data-card]").evaluateAll((els) => els.map((el) => (el as HTMLElement).dataset.card ?? ""));
}

/** Picks the first card with an enabled Build option (else discards the first card) and submits. */
export async function playFirstBuildableOrDiscard(page: Page): Promise<void> {
  const buildable = page.locator('#hand [data-buildable="true"]');
  if ((await buildable.count()) > 0) {
    await buildable.first().click();
    await page.locator("#build-option-0").click();
  } else {
    await page.locator("#hand [data-card]").first().click();
    await page.locator("#discard-button").click();
  }
  await expect(page.locator("#action-panel")).toHaveCount(0);
}
