import { GRACE_MS, expect, inviteOption, notification, test, waitForLiveView } from "./support/lobby";

test("2. online users: A sees B come online, both can invite each other", async ({ player }) => {
  const a = await player("a");
  const b = await player("b");

  await expect(notification(a.page, `User ${b.name} got online!`)).toBeVisible();
  await expect(inviteOption(a.page, b.name)).toHaveCount(1);
  await expect(inviteOption(b.page, a.name)).toHaveCount(1);
});

test("3. offline users: B's browser closes, A is told after the grace period", async ({ player }) => {
  const a = await player("a");
  const b = await player("b");
  await expect(inviteOption(a.page, b.name)).toHaveCount(1);

  await b.context.close();

  await expect(notification(a.page, `User ${b.name} got offline!`)).toBeVisible({
    timeout: GRACE_MS + 5_000,
  });
  await expect(inviteOption(a.page, b.name)).toHaveCount(0);
});

test("4. reconnect: B reloads, A gets no online/offline notification", async ({ player }) => {
  // B first, so A starts with B already online and gets no initial "got online".
  const b = await player("b");
  const a = await player("a");
  await expect(inviteOption(a.page, b.name)).toHaveCount(1);

  await b.page.reload();
  await waitForLiveView(b.page);
  await a.page.waitForTimeout(2 * GRACE_MS);

  await expect(notification(a.page, b.name)).toHaveCount(0);
  await expect(inviteOption(a.page, b.name)).toHaveCount(1);
});
