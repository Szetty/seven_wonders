import { Username } from "./test_helpers/api";
import { Client } from "./test_helpers/client";
import { delay, expectNotification, expectOnlineUsers, waitFor } from "./test_helpers/common";
import { initClient } from "./test_helpers/context";
import { testCtx } from "./test_helpers/test_context";

[
  '/api/secured/game/lobby',
  // '/api/secured/game'
].forEach((wsURL) => {
  describe(`For WS URL ${wsURL}`, async () => {
    test('WebSocket connection', async () => {
      const c = await initClient(wsURL);
      testCtx.cleanUps.push(() => c.close());
    });

    test('Online users', async () => {
      const c1 = await initClient(wsURL);
      testCtx.cleanUps.push(() => c1.close());
      const c2 = await initClient(wsURL);
      testCtx.cleanUps.push(() => c2.close());

      await expectUserGotOnline(c1, c2.username);
      await expectOnlineUsers(c1, [c1.username, c2.username]);
      await expectOnlineUsers(c2, [c1.username, c2.username]);
    });

    test('Offline users', async () => {
      const c1 = await initClient(wsURL);
      testCtx.cleanUps.push(() => c1.close());
      const c2 = await initClient(wsURL);

      await expectUserGotOnline(c1, c2.username);

      c2.close();

      await delay(5000);
      await expectUserGotOffline(c1, c2.username);
    });

    test('Reconnect', async () => {
      const c1 = await initClient(wsURL);
      testCtx.cleanUps.push(() => c1.close());
      const c2 = await initClient(wsURL);
      testCtx.cleanUps.push(() => c2.close());

      await expectUserGotOnline(c1, c2.username);

      c2.reconnect();
      await c2.receiveSyncData();

      // DO NOT EXPECT ONLINE/OFFLINE events as reconnect is short enough
      await expectNoOnlineAndOffline(c1, c2.username);
    });
  })
})

async function expectUserGotOnline(c: Client, username: Username) {
  await expectNotification(
    async () => expect(await c.onOnlineUsers()).toContain(username),
    `get online event for user ${username}`
  );
}

async function expectUserGotOffline(c: Client, username: Username) {
  await expectNotification(
    async () => expect(await c.onOfflineUsers()).toContain(username),
    `get offline event for user ${username}`
  );
}

async function expectNoOnlineAndOffline(c: Client, username: Username) {
  try {
    await waitFor(async () => {
      const res = await Promise.race([
        c.onOnlineUsers(),
        c.onOfflineUsers(),
      ]);
      expect(res).toContain(username);
    });
  } catch {
    return;
  }
  throw new Error(`Expecting no online/offline events for user ${username}`);
}