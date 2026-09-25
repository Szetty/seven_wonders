import { expect, test } from "@playwright/test";
import { LOBBY_URL, login, logout, uniqueName, waitForLiveView } from "./support/auth";

test.describe("authentication", () => {
  test("login succeeds and lands on the lobby with the name in the header", async ({ page }) => {
    const name = uniqueName("alice");

    await login(page, name);

    await expect(page).toHaveURL(LOBBY_URL);
    await expect(page.locator("#current-user-name")).toHaveText(name);
  });

  test("a wrong access token is rejected", async ({ page }) => {
    await login(page, uniqueName("wrong"), "not-the-token");

    await expect(page.locator("#login-error")).toHaveText("Wrong access token");
    await expect(page).toHaveURL(/\/login$/);
  });

  test("an empty access token is rejected", async ({ page }) => {
    await login(page, uniqueName("notoken"), "");

    await expect(page.locator("#login-error")).toHaveText("Access token can't be empty!");
    await expect(page).toHaveURL(/\/login$/);
  });

  test("an empty name is rejected", async ({ page }) => {
    await login(page, "");

    await expect(page.locator("#login-error")).toHaveText("Name can't be empty!");
    await expect(page).toHaveURL(/\/login$/);
  });

  test("a name held by an online user is rejected", async ({ page, browser }) => {
    const name = uniqueName("held");
    await login(page, name);
    await expect(page).toHaveURL(LOBBY_URL);
    await waitForLiveView(page);

    const other = await browser.newContext();
    try {
      const otherPage = await other.newPage();
      await login(otherPage, name);

      await expect(otherPage.locator("#login-error")).toHaveText("Name is already taken");
      await expect(otherPage).toHaveURL(/\/login$/);
    } finally {
      await other.close();
    }
  });

  test("a name is freed when its user logs out", async ({ page, browser }) => {
    const name = uniqueName("reenter");
    await login(page, name);
    await expect(page).toHaveURL(LOBBY_URL);
    await waitForLiveView(page);
    await logout(page);

    const other = await browser.newContext();
    try {
      const otherPage = await other.newPage();
      await login(otherPage, name);

      await expect(otherPage).toHaveURL(LOBBY_URL);
      await expect(otherPage.locator("#current-user-name")).toHaveText(name);
    } finally {
      await other.close();
    }
  });

  test("guests visiting the lobby are sent to login", async ({ page }) => {
    await page.goto(`/lobby/${crypto.randomUUID()}`);

    await expect(page).toHaveURL(/\/login$/);
    await expect(page.locator("#flash-error")).toContainText("You must log in to access this page.");

    await page.goto("/");
    await expect(page).toHaveURL(/\/login$/);
  });

  test("logged-in users visiting login are sent to the lobby", async ({ page }) => {
    await login(page, uniqueName("already"));
    await expect(page).toHaveURL(LOBBY_URL);

    await page.goto("/login");
    await expect(page).toHaveURL(LOBBY_URL);
  });

  test("logout returns to login and protects the lobby again", async ({ page }) => {
    await login(page, uniqueName("bye"));
    await expect(page).toHaveURL(LOBBY_URL);

    await logout(page);

    await page.goto(`/lobby/${crypto.randomUUID()}`);
    await expect(page).toHaveURL(/\/login$/);
  });
});
