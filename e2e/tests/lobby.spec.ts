import { LOBBY_URL, login, logout } from "./support/auth";
import {
  acceptInvite,
  declineInvite,
  expect,
  freeRows,
  invite,
  inviteOption,
  memberRow,
  memberRows,
  notification,
  removeMember,
  test,
  type Player,
} from "./support/lobby";

async function joinTable(owner: Player, guest: Player): Promise<void> {
  await invite(owner.page, guest.name);
  await acceptInvite(guest.page, owner.name);
  await expect(guest.page).toHaveURL(owner.lobbyUrl);
  await expect(memberRow(owner.page, guest.name)).toHaveAttribute("data-connected", "true");
}

test("1. lobby connection: own table shows only me, leader and connected", async ({ player }) => {
  const a = await player("a");

  await expect(memberRows(a.page)).toHaveCount(1);
  const me = memberRow(a.page, a.name);
  await expect(me).toHaveAttribute("data-leader", "true");
  await expect(me).toHaveAttribute("data-self", "true");
  await expect(me).toHaveAttribute("data-connected", "true");
  await expect(freeRows(a.page)).toHaveCount(6);
});

test("5. invite users", async ({ player }) => {
  const a = await player("a");
  const b = await player("b");

  await invite(a.page, b.name);

  await expect(memberRows(a.page)).toHaveCount(2);
  await expect(memberRow(a.page, a.name)).toHaveAttribute("data-leader", "true");
  await expect(memberRow(a.page, b.name)).toHaveAttribute("data-leader", "false");
  await expect(memberRow(a.page, b.name)).toHaveAttribute("data-connected", "false");
  await expect(notification(b.page, `You are expected on table ${a.name}`)).toBeVisible();
  await expect(memberRows(b.page)).toHaveCount(1);
  await expect(memberRow(b.page, b.name)).toHaveAttribute("data-leader", "true");
});

test("6. accept invitation", async ({ player }) => {
  const a = await player("a");
  const b = await player("b");

  await invite(a.page, b.name);
  await acceptInvite(b.page, a.name);

  await expect(b.page).toHaveURL(a.lobbyUrl);
  await expect(memberRow(a.page, b.name)).toHaveAttribute("data-connected", "true");
  await expect(memberRows(b.page)).toHaveCount(2);
  await expect(memberRow(b.page, a.name)).toHaveAttribute("data-leader", "true");
  await expect(memberRow(b.page, b.name)).toHaveAttribute("data-self", "true");
  await expect(notification(b.page, `You are expected on table ${a.name}`)).toHaveCount(0);
});

test("7. decline invitation", async ({ player }) => {
  const a = await player("a");
  const b = await player("b");

  await invite(a.page, b.name);
  await declineInvite(b.page, a.name);

  await expect(notification(a.page, `User ${b.name} declined your invitation!`)).toBeVisible();
  await expect(memberRows(a.page)).toHaveCount(1);
  await expect(notification(b.page, `You are expected on table ${a.name}`)).toHaveCount(0);
});

test("8. unauthorized invite: a guest sees no invite controls", async ({ player }) => {
  const a = await player("a");
  const b = await player("b");
  await joinTable(a, b);

  await expect(b.page.locator("#invite-form")).toHaveCount(0);
  await expect(b.page.getByRole("button", { name: /^Remove / })).toHaveCount(0);
});

test("9. return to own lobby", async ({ player }) => {
  const a = await player("a");
  const b = await player("b");
  await joinTable(a, b);

  await b.page.getByRole("link", { name: "My table" }).click();

  await expect(b.page).toHaveURL(b.lobbyUrl);
  await expect(memberRows(b.page)).toHaveCount(1);
  await expect(memberRow(b.page, b.name)).toHaveAttribute("data-leader", "true");
  await expect(memberRow(b.page, b.name)).toHaveAttribute("data-connected", "true");
  // B left A's table but is still invited (leaving never removes authorization)...
  await expect(memberRow(a.page, b.name)).toHaveAttribute("data-connected", "false");
  // ...and still online: once removed from the table, B is offered in A's invite select.
  await removeMember(a.page, b.name);
  await expect(inviteOption(a.page, b.name)).toHaveCount(1);
  await expect(notification(a.page, `User ${b.name} got offline!`)).toHaveCount(0);
});

test("10. direct URL without invite redirects to own lobby", async ({ player }) => {
  const a = await player("a");
  const c = await player("c");

  await c.page.goto(a.lobbyUrl);

  await expect(c.page).toHaveURL(c.lobbyUrl);
  await expect(c.page.getByText("Only invited players can join this table")).toBeVisible();
});

test("11. uninvite while connected sends the guest home", async ({ player }) => {
  const a = await player("a");
  const b = await player("b");
  await joinTable(a, b);

  await removeMember(a.page, b.name);

  await expect(b.page).toHaveURL(b.lobbyUrl);
  await expect(b.page.getByText(`You were removed from ${a.name}'s table`)).toBeVisible();
  await expect(memberRows(a.page)).toHaveCount(1);
});

test("12. an invite sent while logged out is shown at next login", async ({ player }) => {
  const a = await player("a");
  const b = await player("b");
  await expect(inviteOption(a.page, b.name)).toHaveCount(1);

  await logout(b.page);
  // B is gone; A still lists B during the presence grace period, so invite now.
  await invite(a.page, b.name);
  await expect(memberRow(a.page, b.name)).toHaveAttribute("data-connected", "false");

  await login(b.page, b.name);
  await expect(b.page).toHaveURL(LOBBY_URL);
  await expect(notification(b.page, `You are expected on table ${a.name}`)).toBeVisible();
});
