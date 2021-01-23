import { ErrorBody, InvitedUser, User, Username } from "./test_helpers/api";
import { Client } from "./test_helpers/client";
import { expectNotification, expectOnlineUsers, waitFor } from "./test_helpers/common";
import { initClient } from "./test_helpers/context";
import { testCtx } from "./test_helpers/test_context";

const wsURL = '/api/secured/game/lobby';

test('Invite users', async() => {
  const c1 = await initClient(wsURL);
  const c2 = await initClient(wsURL);
  testCtx.cleanUps.push(() => c1.close());
  testCtx.cleanUps.push(() => c2.close());

  await expectLeaderToBeInvited(c1);
  await c1.inviteUser(c2.username);
  await expectGotInvite(c2, { name: c1.username, gameID: c1.gameID });
  await expectInvitedUsers(c1, [c1.username, c2.username]);
  await expectInvitedUsers(c2, [c2.username]);
});

test('Accept invitation', async() => {
  const c1 = await initClient(wsURL);
  const c2 = await initClient(wsURL);
  testCtx.cleanUps.push(() => c1.close());
  testCtx.cleanUps.push(() => c2.close());

  await c1.inviteUser(c2.username);
  await expectGotInvite(c2, { name: c1.username, gameID: c1.gameID });
  await expectInvitedButNotConnected(c1, c2.username);

  await c2.acceptInvitation(c1.gameID);
  await expectGotConnected(c1, c2.username);
  await expectConnected(c1, c2.username);
  await expectInvitedUsers(c2, [c1.username, c2.username]);
});

test('Decline invitation', async() => {
  const c1 = await initClient(wsURL);
  const c2 = await initClient(wsURL);
  testCtx.cleanUps.push(() => c1.close());
  testCtx.cleanUps.push(() => c2.close());

  await c1.inviteUser(c2.username);
  await expectGotInvite(c2, { name: c1.username, gameID: c1.gameID });
  await expectInvitedUsers(c1, [c1.username, c2.username]);

  await c2.declineInvitation(c1.gameID);
  await expectDeclineInvitation(c1, c2.username);
  await expectInvitedUsers(c1, [c1.username]);
});

test('Unauthorized invite', async() => {
  const c1 = await initClient(wsURL);
  const c2 = await initClient(wsURL);
  testCtx.cleanUps.push(() => c1.close());
  testCtx.cleanUps.push(() => c2.close());

  await c1.inviteUser(c2.username);
  await expectGotInvite(c2, { name: c1.username, gameID: c1.gameID });
  await c2.acceptInvitation(c1.gameID);

  const { code } = (await c2.inviteUser(c1.username)) as ErrorBody;
  expect(code).toBe('Unauthorized');
});

test('Return to own lobby', async() => {
  const c1 = await initClient(wsURL);
  const c2 = await initClient(wsURL);
  testCtx.cleanUps.push(() => c1.close());
  testCtx.cleanUps.push(() => c2.close());

  await c1.inviteUser(c2.username);
  await expectGotInvite(c2, { name: c1.username, gameID: c1.gameID });
  await c2.acceptInvitation(c1.gameID);

  await c2.returnToOwnLobby();
  await expectLeaderToBeInvited(c2);
  await expectOnlineUsers(c1, [c1.username, c2.username]);
});

async function expectLeaderToBeInvited(c: Client) {
  const res = await c.getInvitedUsers();
  const name = c.username;
  expect(res).toEqual([{ connected: true, leader: true, name } as InvitedUser]);
}

async function expectGotInvite(c: Client, user: User) {
  await expectNotification(
    async () => expect(await c.onInvite()).toEqual([user]),
    `get invite from user ${user.name}`  
  );
}

async function expectInvitedUsers(c: Client, usernames: Username[]) {
  const res = (await c.getInvitedUsers()).map(({ name }) => name);
  expect(res.sort()).toEqual(usernames.sort());
}

async function expectInvitedButNotConnected(c: Client, username: Username) {
  const [{ name, connected, leader }] = (await c.getInvitedUsers()).filter(({ name }) => name != c.username);
  expect(name).toBe(username);
  expect(connected).toBe(false);
  expect(leader).toBe(false);
}

async function expectGotConnected(c: Client, username: Username) {
  await expectNotification(
    async () => expect(await c.onConnected()).toEqual([username]),
    `get connected ${username}`  
  );
}

async function expectConnected(c: Client, username: Username) {
  const [{ name, connected, leader }] = (await c.getInvitedUsers()).filter(({ name }) => name != c.username);
  expect(name).toBe(username);
  expect(connected).toBe(true);
  expect(leader).toBe(false);
}

async function expectDeclineInvitation(c: Client, username: Username) {
  await expectNotification(
    async () => expect(await c.onDeclineInvitation()).toEqual([username]),
    `get decline invitation from ${username}`
  );
}