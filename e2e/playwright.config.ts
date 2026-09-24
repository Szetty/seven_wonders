import { defineConfig, devices } from "@playwright/test";

const PORT = 4004;
const baseURL = `http://localhost:${PORT}`;

export default defineConfig({
  testDir: "./tests",
  fullyParallel: true,
  forbidOnly: !!process.env.CI,
  retries: process.env.CI ? 1 : 0,
  reporter: [["list"], ["html", { open: "never" }]],
  use: {
    baseURL,
    trace: "retain-on-failure",
  },
  projects: [{ name: "chromium", use: { ...devices["Desktop Chrome"] } }],
  webServer: {
    // assets.build runs first: ecto.setup's seeds step starts the endpoint
    // (server: true), so assets must exist before /login starts answering.
    command: "cd ../helios && MIX_ENV=e2e mix do assets.build + ecto.reset + phx.server",
    url: `${baseURL}/login`,
    reuseExistingServer: !process.env.CI,
    timeout: 180_000,
    env: { PORT: String(PORT) },
  },
});
