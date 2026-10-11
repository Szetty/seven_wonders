# Wonder selection modes — choose board, choose side, random

Status: approved design (2026-10-11). Companion to `2026-10-11-table-polish-design.md` (same folder). Builds on the Phase 4 game design (`docs/superpowers/specs/2026-09-23-migration/2026-09-23-phase-4-game-design.md`).

## Goal

The table owner picks one of three ways to hand out wonder boards before starting a game:

| Mode | `wonder_mode` | Board | Side |
|---|---|---|---|
| 1 | `choose_board` | each player claims one, first claim wins | chosen by the player |
| 2 | `choose_side` | random (from the seed) | chosen by the player |
| 3 | `random` (default) | random | random — today's behaviour |

Success criteria:
- The owner can set the mode in the lobby; every member sees the current mode.
- In modes 1 and 2 the game opens on a setup screen; Age I starts when every player has confirmed.
- Two players can never end up with the same board; the second claim on a board is rejected.
- Setup survives refresh, reconnect and server restart (claims are logged actions, replayed from seed + action log).
- Existing `random` / `explicit` games replay identically (pinned seed-42 deal unchanged).
- All core, helios and e2e checks pass, plus the new tests below.

## Decisions

- **The setup is an engine phase.** Board legality ("one board per player", "your drawn board only") is a game rule, so it lives in `core`. Claims and confirmations are ordinary actions in `game_actions`.
- **Claims are changeable until confirmed.** A claim reserves the board immediately; re-claiming switches board or side and releases the old one; `Confirm` locks it.
- **First claim wins** because the `GameServer` serialises submissions; the losing claim gets an error.
- **RNG order is unchanged** for existing modes; `choose_side` draws boards exactly like `random` minus the side draw; `choose_board` draws nothing for wonders. The deck is generated at `Game::new` in all modes, so player choices never influence the deal. No `ENGINE_VERSION` bump (no existing replay changes); the pinned-deal test proves it.
- **No timeout** for setup (same as card turns).
- **All setup information is public** (who holds which board/side, who confirmed); there is no hidden-information concern.
- **The e2e `wonders:` config override applies only when the lobby mode is `random`** (explicit boards, no setup phase), so existing e2e tests are unaffected and e2e can still exercise the setup modes.

## Engine (`core`)

### Types (`core/src/game/types.rs`)
- `WonderSelection` gains `ChooseBoard` and `ChooseSide` (next to `Random`, `Explicit(..)`).
- `Phase` gains `Setup` (and `PhaseKind::Setup` in `view.rs`, `PhaseKindDto::Setup`).
- `Action` gains `ClaimWonder { wonder: String, side: Side }` and `ConfirmWonder`.
- `ActionError` gains `UnknownWonder`, `WonderTaken`, `NotYourWonder` (choose_side, wrong board), `NoClaim` (confirm without claim), `AlreadyConfirmed`. Card actions during `Setup` and setup actions outside it return the existing `ActionNotAllowedNow`.

### Setup (`core/src/game/setup.rs`)
RNG contract (header comment updated):
1. `Random`: shuffle wonders, keep `n`, draw a side per seat (unchanged).
   `ChooseSide`: shuffle wonders, keep `n` (same draws as `Random` step 1 minus the sides).
   `ChooseBoard`, `Explicit`: no wonder draws.
2. `generate_deck` (unchanged), in every mode.

For `ChooseBoard` / `ChooseSide`, `Game::new` stores the generated deck and a setup state instead of building `GameState`:
- `ChooseSide`: each seat has its drawn wonder, no side, not confirmed.
- `ChooseBoard`: each seat has no claim, not confirmed.

`GameState` construction, `state.init()` (starting coins and resources) and `deal_age(1)` are moved into a `finish_setup` step shared by every mode: `Random`/`Explicit` call it immediately in `Game::new` (identical behaviour to today); setup modes call it when the last seat confirms. The exact representation (e.g. `Option<GameState>` vs a `Setup` sub-struct holding the deck) is an implementation choice, but `game` stays free of rustler imports and `HashMap` iteration.

### Resolution
- `ClaimWonder { wonder, side }` (phase `Setup`, player not confirmed):
  - `wonder` must be a base wonder name, else `UnknownWonder`.
  - `ChooseSide`: `wonder` must equal the seat's drawn wonder, else `NotYourWonder`.
  - `ChooseBoard`: `wonder` must not be held (claimed or confirmed) by another seat, else `WonderTaken`. Any previous claim of this seat is released.
  - Records `(wonder, side)` for the seat.
- `ConfirmWonder`: requires a claim (`NoClaim`), not already confirmed (`AlreadyConfirmed`). When every seat is confirmed: `finish_setup` with the claims in seat order, phase → `ChoosingCards { age: 1, turn: 1 }`.
- Setup actions produce no history events.

### View (`core/src/game/view.rs`)
During `Setup` the player view carries a `setup` field (`None` otherwise):
- `mode`: `choose_board | choose_side`,
- `seats`: per player `{ player, wonder: Option<String>, side: Option<Side>, confirmed: bool }` (for `choose_side` `wonder` is the drawn board from the start),
- `available`: wonder names not held by anyone (`choose_board`; empty for `choose_side`).
Players' boards, hands and coins are empty/absent during setup; Helios must not render the table then. `submitted`/waiting during setup = seats not confirmed.

### NIF (`core/src/nif/`)
- `WondersDto` gains `:choose_board | :choose_side`.
- `ActionDto` gains `{:claim_wonder, %{wonder, side}}` and `:confirm_wonder`.
- `ActionErrorDto`, `PhaseKindDto` and the view DTO gain the new variants/field.

## Helios

### Lobby setting
- Migration: `alter table(:lobbies), add :wonder_mode, :string, null: false, default: "random"`. Schema field with `validate_inclusion` in `~w(random choose_side choose_board)`.
- `Lobbies.set_wonder_mode(scope, lobby, mode)`: owner only; broadcasts `{:settings_changed}` on `lobby:<id>`. Changing it while a game is active is allowed (it applies to the next game).
- `LobbyLive`: above the Start button the owner sees a three-option segmented control `#wonder-mode` (`#wonder-mode-random`, `#wonder-mode-choose_side`, `#wonder-mode-choose_board`; labels "Random board & side", "Random board, choose side", "Choose board"). Others see the current label read-only. Handles `{:settings_changed}` by reloading the lobby.

### Starting a game (`games.ex`)
`configured_wonders/2` becomes: lobby mode `choose_side` / `choose_board` → `:choose_side | :choose_board`; mode `random` → the config `:wonders` override as `{:explicit, …}` if set, else `:random`. `ActionCodec.encode_wonders/decode_wonders` add `%{"mode" => "choose_side" | "choose_board"}` (`nil` stays `:random`, `%{"explicit" => …}` unchanged). `Replay.rebuild` needs no change beyond the codec.

`ActionCodec.encode/decode` add `claim_wonder` (`%{"type" => "claim_wonder", "wonder" => name, "side" => "a" | "b"}`) and `confirm_wonder`. `Games.Choice` (or the GameServer submit path) accepts them during `Setup`.

### Setup screen (`GameLive`, `view.phase.kind == :setup`)
New component module `HeliosWeb.GameComponents.WonderSetup`, rendered instead of the table and dock.

- `choose_board`: `#wonder-setup` grid of all base wonders, `#setup-wonder-<slug>` each, showing `wonders/{city}{A,B}.png` for the currently shown side, an A/B toggle, and the holder's name (with a check once confirmed). Tapping a free board (or toggling the side of your own) submits `claim_wonder`. Boards held by others are disabled. `#confirm-wonder` submits `confirm_wonder`, enabled once you hold a claim; after confirming, the screen shows "Waiting for …".
- `choose_side`: your drawn board shown large as A and B (`#setup-side-a`, `#setup-side-b`); tapping one claims it; `#confirm-wonder` as above. A compact list shows the other players' drawn boards and confirmation status.
- Errors (`WonderTaken` after a race) show a flash ("‹Board› was just taken") and the view refreshes.
- Top bar during setup: age/turn/pass labels replaced by "Choosing boards"; the existing waiting list (A in the polish spec) lists unconfirmed players; `#discard-pile` absent.
- Updates use the existing `{:game_updated, seq}` → re-fetch view flow. No new broadcasts.
- `GameFormat.dock?/show_hand?/waiting_for` handle `:setup`.

### Replay / history
- `Games.replay/2`: the first frame snapshot is taken when the phase first becomes `ChoosingCards` (setup actions do not open frames and are not anyone's `my_choice`).
- History drawer: no setup entries.

## Testing

Core (`core/src/tests/`):
- `game_setup.rs`: pinned seed-42 deal unchanged for `Random`; `ChooseSide` draws the same boards as `Random` for the same seed; `ChooseBoard` deals exactly like `Explicit` with the same boards and seed; the dealt hands never depend on which boards/sides were claimed. (Decks differ between `Random`, `ChooseSide` and `ChooseBoard` for the same seed, because each consumes a different number of wonder draws before the deck shuffle.)
- New `game_wonder_setup.rs`: claim/switch/flip/confirm; `WonderTaken` on a held board (including a confirmed one); `NotYourWonder` in choose_side; `NoClaim`; `AlreadyConfirmed`; card actions during setup → `ActionNotAllowedNow`; last confirmation starts Age I with starting coins and dealt hands; view `setup` field contents.
- `nif_dto.rs`: new DTO round-trips.
- Determinism: a full simulated `ChooseBoard` game replays identically.

Helios:
- `lobbies_test.exs`: `set_wonder_mode` owner-only, validation, broadcast.
- `lobby_live_test.exs`: owner control changes mode; others see it read-only and get the live update.
- `games_test.exs`: start passes the mode; the config override applies only in `random` mode.
- `action_codec_test.exs`: wonders mode and new actions round-trip.
- `game_live_wonder_setup_test.exs`: both modes end to end with LiveViewTest; two players racing for one board (second gets the flash); refresh mid-setup keeps claims; game proceeds to Age I.
- `replay_test.exs`: a choose_board game's frames start at Age I turn 1.

e2e: one `choose_board` game (3 browsers): the owner sets the mode, everyone claims and confirms, the game reaches Age I turn 1 (the config override does not apply because the mode is not `random`).

## Out of scope

- Expansion wonders, Leaders/Cities.
- Setup timeouts or auto-pick for idle players.
- Draft-order (snake) picking.
- Showing setup actions in the history or replay.
