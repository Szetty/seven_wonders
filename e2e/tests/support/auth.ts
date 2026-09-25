import { expect, type Page } from "@playwright/test";

/** Access token configured for MIX_ENV=e2e (helios/config/e2e.exs). */
export const ACCESS_TOKEN = "e2e";

/** Matches `/lobby/<uuid>` — every logged-in user lands on their own table. */
export const LOBBY_URL =
  /\/lobby\/[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/;

/** A per-test unique user name: `<prefix>_<random>`, never longer than 24 characters. */
export function uniqueName(prefix = "user"): string {
  const suffix = `${Date.now().toString(36)}${Math.random().toString(36).slice(2, 6)}`;
  return `${prefix.slice(0, 24 - suffix.length - 1)}_${suffix}`;
}

/** Waits until the page's main LiveView has joined over the websocket. */
export async function waitForLiveView(page: Page): Promise<void> {
  await expect(page.locator("[data-phx-main]")).toHaveClass(/phx-connected/);
}

/**
 * Opens /login, fills the form and submits it. Does not assert the outcome:
 * callers check the URL (success) or `#login-error` (validation failure).
 */
export async function login(page: Page, name: string, token: string = ACCESS_TOKEN): Promise<void> {
  await page.goto("/login");
  await waitForLiveView(page);
  await page.getByLabel("Access Token", { exact: true }).fill(token);
  await page.getByLabel("Name", { exact: true }).fill(name);
  await page.locator("#login-submit").click();
}

/** Clicks Logout in the site header and waits for the login page. */
export async function logout(page: Page): Promise<void> {
  await page.locator("#logout-link").click();
  await expect(page).toHaveURL(/\/login$/);
}
