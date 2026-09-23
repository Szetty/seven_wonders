# Phase 2 — Lobby

Status: approved design (2026-09-23). Overview: `2026-09-23-migration-overview-design.md`. Depends on Phase 1.

## Goal

Port the Go lobby model to LiveView + PubSub + Presence, with invites persisted in SQLite, the legacy integration scenarios rewritten as Playwright tests, and the legacy Go backend, custom websocket client and Jest tests deleted.

## Legacy behaviour (reference: `backend_old/crux/lobby`, `frontend/src/elm/Pages/Lobby.elm`)

- Every user has one lobby (their "table"), identified by `game_id`; its leader is its owner, forever.
- `authorizedUsers` = owner + invited/joined users. Only authorized users may open a lobby.
- Only the leader may invite/uninvite; others get `Unauthorized` (Helios: `:not_leader`).
- Invite → invitee gets `GotInvite {name: leader, gameID}` shown as an Accept/Decline notification ("You are expected on table <name>").
- Accept = navigate to the inviter's lobby (no message). Decline → leader gets "User X declined your invitation!" and the invitee loses authorization.
- Uninvite → invitee loses authorization; if currently viewing that lobby, they are sent back to their own lobby.
- Presence is global for online/offline (`UserGotOnline`/`UserGotOffline`, with a 5 s grace on disconnect so reconnects don't flap), and per-lobby for connected/disconnected.
- Leaving a lobby never removes authorization.
- Owner UI: select of online users (not invited, not self) + Invite button; members table (crown for leader, double-chevron for self, gray rows for disconnected, ✕ uninvite per non-self row, padded with "FREE" rows up to 7). Guest UI: table of connected members only.

**Go bugs deliberately not ported:** silent no-reply on duplicate/self invites (now an error flash), lost invites to offline users (now persisted), panics on malformed payloads, duplicate online events on fast reconnect.

## Design

### Data
Migration `create_lobbies`:
- `lobbies`: `id` (`:binary_id` PK — the public `game_id`), `owner_id` (FK users, `on_delete: :delete_all`, not null), timestamps. `unique_index(:lobbies, [:owner_id])`.

Migration `create_lobby_invites`:
- `lobby_invites`: `id`, `lobby_id` (FK, delete_all), `user_id` (FK, delete_all), `status` (string: `"pending" | "accepted"`, default `"pending"`), timestamps. `unique_index(:lobby_invites, [:lobby_id, :user_id])`.
- A row (any status) = authorized. `pending` rows drive invite notifications.

### `Helios.Lobbies` context
- `get_or_create_own_lobby(user) :: Lobby.t()` (lazy creation, like Go; `on_conflict: :nothing` + re-fetch to be race-safe).
- `fetch_lobby(game_id) :: {:ok, Lobby.t()} | {:error, :invalid_game_id}` — malformed UUID or not found → `:invalid_game_id`.
- `authorize(lobby, user) :: :ok | {:error, :unauthorized}` — owner or any invite row.
- `members(lobby) :: [%{user: User.t(), leader?: boolean()}]` — owner first, then invitees ordered by `inserted_at`.
- `invite(scope, lobby, invitee_id) :: {:ok, invite} | {:error, reason}` — reasons: `:not_leader` (not owner), `:self_invite`, `:already_invited`, `:lobby_full` (owner + invites ≥ 7), `:invalid_user` (no such user). On success broadcast `{:invited, invite}` on `"user:<invitee_id>"` and `{:members_changed}` on `"lobby:<id>"`.
- `uninvite(scope, lobby, user_id) :: :ok | {:error, :not_leader | :not_invited}` — deletes the row; broadcast `{:uninvited, lobby_id}` on `"user:<user_id>"` and `{:members_changed}` on `"lobby:<id>"`.
- `accept(scope, lobby_id) :: :ok | {:error, :not_invited}` — sets `status: "accepted"`; broadcast `{:invite_resolved, lobby_id}` on the invitee's own topic (clears the notification in other tabs).
- `decline(scope, lobby_id) :: :ok | {:error, :not_invited}` — deletes the row; broadcast `{:declined, user}` on `"lobby:<id>"` and `{:invite_resolved, lobby_id}` on `"user:<id>"`.
- `pending_invites(user) :: [%{lobby_id, owner_name}]`.
- Human messages for each error atom live in one function `Lobbies.error_message/1` (e.g. `:not_leader` → "Only the table leader can do that", `:unauthorized` → "Only invited players can join this table", `:invalid_game_id` → "That table does not exist").

### Presence
- `"users:online"` (tracked in Phase 1's auth hook): global online list.
- `"lobby:<id>"`: tracked by LobbyLive on connected mount; key = user id. Drives each member's `connected` flag.
- **Grace period (5 s)** in a reusable module `HeliosWeb.OnlineTracker` (pure functions over assigns + `Process.send_after`): on a `leave` for a user, schedule `{:confirm_offline, user_id}` in 5 s; on `join` for a user with a pending check, cancel it and emit nothing; on `confirm_offline`, if the user is still absent from `Presence.list/1`, remove from the online set and emit `:went_offline`; a `join` for a user not previously online emits `:came_online`. The 5 s constant is configurable (`config :helios, :presence_grace_ms`) so tests can shorten it.

### Notifications (all authenticated pages)
- `HeliosWeb.Notifications` on_mount hook (added to the `:authenticated` live_session): on connected mount subscribes to `"user:<id>"`, assigns `notifications` from `Lobbies.pending_invites/1`, and `attach_hook(:notifications, :handle_info, ...)` handles `{:invited, _}`, `{:uninvited, lobby_id}`, `{:invite_resolved, lobby_id}`; `attach_hook(:notifications, :handle_event, ...)` handles `"accept_invite"` (→ `Lobbies.accept` then `push_navigate` to `/lobby/<id>`) and `"decline_invite"`, `"dismiss_notification"`.
- Two kinds: **approve** (pending invites, persistent until resolved: "You are expected on table <owner>" + Accept / Decline) and **simple** (transient: "User X got online!", "User X got offline!", "User X declined your invitation!", "You were removed from <owner>'s table"; OK button, auto-expire after 8 s). At most 5 shown, newest first; approve notifications are never evicted by simple ones.
- Component `<.notifications items={@notifications} />` rendered by `Layouts.app` (pass `notifications` through), teal strip style.

### LobbyLive (`/lobby/:game_id`)
- Mount: `fetch_lobby` → `authorize`; on error `push_navigate` to own lobby with the error message flash. Subscribes to `"lobby:<id>"` and `"users:online"`; tracks lobby presence.
- Assigns: `lobby`, `owner?`, `members`, `connected_ids` (from lobby presence), `online` (from OnlineTracker), `invite_form`.
- Owner view: invite form (`<.input type="select">` of online users not in members and not self, prompt "Select username"; Invite button disabled until selected), members table (Username column + Delete column), padding "FREE" rows to 7.
- Guest view: table of connected members only, no controls.
- Events: `"invite"`, `"uninvite"` → context functions; errors → `put_flash(:error, Lobbies.error_message(reason))`. Non-owner invite attempts are rejected server-side (`:not_leader`) even though the UI hides the controls.
- handle_info: `presence_diff` on either topic (recompute connected / feed OnlineTracker → simple notifications for other users only), `{:members_changed}` → reload members, `{:declined, user}` → reload + simple notification (owner only), `{:uninvited, lobby_id}` for the current lobby (arrives via the Notifications hook) → `push_navigate` to own lobby with flash.
- Header gains a "My table" link to the user's own lobby.
- `/` now redirects logged-in users to `/lobby/<own game_id>`; `LobbyStubLive` and `/lobby` are removed. Phase 1 e2e assertions updated from `/lobby` to `/lobby/<uuid>`.

## Testing

### ExUnit
- `Lobbies`: every rule and error atom in `invite/uninvite/accept/decline/authorize/fetch_lobby`; capacity 7; members order; broadcasts (subscribe in test, `assert_receive`).
- `OnlineTracker`: leave→confirm emits `:went_offline`; leave→join within grace emits nothing; fresh join emits `:came_online`.
- `LobbyLive`: owner vs guest rendering; unauthorized and invalid id redirects; invite flow across two LiveView test clients; non-owner `render_hook`/`render_submit` of `"invite"` is rejected with the flash.
- `Notifications` hook: pending invites rendered on mount; accept navigates; decline removes.

### Playwright (`e2e/tests/lobby.spec.ts`, `e2e/tests/presence.spec.ts`)
Ports of all legacy scenarios (`integration-tests/src/{protocol,lobby}.test.ts`), each with separate browser contexts per user. The e2e env sets `presence_grace_ms: 2000` to keep tests fast.
1. **Lobby connection** — after login, own lobby shows a single row: self, leader (crown), connected.
2. **Online users** — A then B log in; A sees "User B got online!" and B in A's invite select; B's select contains A.
3. **Offline users** — B's context closes; A sees "User B got offline!" and B leaves the select.
4. **Reconnect** — B reloads; A receives no online/offline notification about B for 2× grace.
5. **Invite users** — A invites B; A's table lists A (leader) and B (disconnected); B sees "You are expected on table A"; B's own lobby shows only B.
6. **Accept invitation** — B accepts; B is on A's lobby URL; A's table shows B connected; B's view lists A and B.
7. **Decline invitation** — B declines; A sees "User B declined your invitation!"; A's table shows only A.
8. **Unauthorized invite** — B in A's lobby sees no invite controls (server-side rejection covered by ExUnit).
9. **Return to own lobby** — B in A's lobby clicks "My table"; B's lobby shows B as leader and connected; A's invite select still contains B (online).
10. **Direct URL without invite** — C opens A's lobby URL → redirected to own lobby with "Only invited players can join this table" flash (`:unauthorized`).
11. **Uninvite while connected** — B in A's lobby; A clicks ✕ on B; B is sent to own lobby with "You were removed from A's table".
12. **Invite persists offline** — A invites B while B is logged out; B logs in and sees the invite notification.

## Deletions (end of phase, after all tests are green)
- `git rm -r backend_old websocket-client integration-tests`.
- CI: remove `old_backend`, `integration-tests`, and `frontend` jobs (the Elm build depends on `websocket-client/`). `frontend/` stays on disk until Phase 4 as the asset source.

## Acceptance criteria
- `mix precommit` and the full Playwright suite green locally and in CI.
- All 9 legacy scenarios have a Playwright equivalent (1–9 above).
