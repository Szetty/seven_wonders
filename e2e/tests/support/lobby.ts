import {
  test as base,
  expect,
  type BrowserContext,
  type Locator,
  type Page,
} from "@playwright/test";
import { LOBBY_URL, login, uniqueName } from "./auth";

export { expect };

/** Must match `config :helios, presence_grace_ms` in helios/config/e2e.exs. */
export const GRACE_MS = 2_000;

export type Player = {
  context: BrowserContext;
  page: Page;
  name: string;
  lobbyUrl: string;
};

function exact(text: string): RegExp {
  return new RegExp(`^\\s*${text.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")}\\s*$`);
}

export async function waitForLiveView(page: Page): Promise<void> {
  await expect(page.locator("[data-phx-main].phx-connected")).toHaveCount(1);
}

export const test = base.extend<{ player: (prefix: string) => Promise<Player> }>({
  player: async ({ browser }, use) => {
    const created: Player[] = [];

    await use(async (prefix: string) => {
      const context = await browser.newContext();
      const page = await context.newPage();
      const name = uniqueName(prefix);
      await login(page, name);
      await expect(page).toHaveURL(LOBBY_URL);
      await waitForLiveView(page);
      const player = { context, page, name, lobbyUrl: page.url() };
      created.push(player);
      return player;
    });

    for (const { context } of created) {
      await context.close().catch(() => undefined);
    }
  },
});

export function memberRows(page: Page): Locator {
  return page.locator("#members-table tbody tr[data-member]");
}

export function memberRow(page: Page, name: string): Locator {
  return page.locator(`#members-table tbody tr[data-member][data-name="${name}"]`);
}

export function freeRows(page: Page): Locator {
  return page.locator("#members-table tbody tr[data-free-slot]");
}

export function inviteOption(page: Page, name: string): Locator {
  return page.locator("#invite_user_id option", { hasText: exact(name) });
}

export async function invite(page: Page, name: string): Promise<void> {
  await page.locator("#invite_user_id").selectOption({ label: name });
  await expect(page.locator("#invite-button")).toBeEnabled();
  await page.locator("#invite-button").click();
  await expect(memberRow(page, name)).toHaveCount(1);
}

export function notification(page: Page, text: string): Locator {
  return page.locator("#notifications [data-notification]", { hasText: text });
}

export async function acceptInvite(page: Page, ownerName: string): Promise<void> {
  await notification(page, `You are expected on table ${ownerName}`)
    .getByRole("button", { name: "Accept" })
    .click();
}

export async function declineInvite(page: Page, ownerName: string): Promise<void> {
  await notification(page, `You are expected on table ${ownerName}`)
    .getByRole("button", { name: "Decline" })
    .click();
}

export async function removeMember(page: Page, name: string): Promise<void> {
  await page.getByRole("button", { name: `Remove ${name}` }).click();
}
