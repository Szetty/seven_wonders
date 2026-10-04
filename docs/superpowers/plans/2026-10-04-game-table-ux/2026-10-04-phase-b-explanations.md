# Game Table UX — Phase B: Explanations and Costs Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Every card and wonder stage explains itself (effects, cost, chains), chain builds read "Free (you have …)", hand cards carry a cheapest-cost badge, and payment choices appear only after tapping an action.

**Architecture:** The engine's effects become `Effect { spec, apply }` so a plain-data `EffectSpec` travels next to each closure; `game_settings` exposes a card/wonder catalogue built from it; `BuildOption` gains `Chain(card)`. Helios caches the catalogue (`Helios.Catalogue`), renders specs to English (`HeliosWeb.CardText`), and redesigns the action sheet around action rows.

**Tech Stack:** Rust 2021 + Rustler 0.38 (`NifMap`/`NifTaggedEnum`/`NifUnitEnum`), Phoenix 1.8 / LiveView 1.1, Tailwind 4.1, ExUnit + LazyHTML, Playwright.

**Spec:** `docs/superpowers/specs/2026-10-04-game-table-ux/2026-10-04-game-table-ux-design.md` (section "Phase B — Explanations and costs").

## Global Constraints

- Read `helios/AGENTS.md` before touching `helios/`.
- All game rules live in `core`; Helios only formats specs and options the engine returns. The `game` module stays free of rustler imports; DTOs live in `core/src/nif/dto.rs`.
- No rule change: `ENGINE_VERSION` stays `1`; `random_games_finish_for_every_player_count` and `identical_inputs_produce_identical_views` must pass unchanged.
- Never iterate a `HashMap` where order can affect an outcome (the chain source is chosen in the card's dependency order).
- Existing NIF fields keep their shape (`game_settings.cards[].name/category/age`, `wonders[].name/sides`); additions only.
- `Choice` keeps resolving by the **engine's** option index; sorting for display never changes the index sent.
- Touch targets ≥ 44×44 CSS px on touch viewports; no horizontal scroll at ≥ 360px.
- Stage explicit paths only; never stage `helios/config/config.exs` / `helios/config/dev.exs`.
- Core checks before each core commit: `cd core && cargo fmt --check && cargo clippy --all-targets -- -D warnings && cargo test`. Helios: `cd helios && mix precommit`.

## Review Focus

- Forum when the player has built **both** trading posts: the option must be `{:chain, "East trading post"}` (first in Forum's dependency list), identical on every call — never dependent on `HashMap` order. (Test in Task 3.)
- A card that is free because of the player's own production (no chain) must stay `:free`, not `{:chain, _}`; a chain card the player has **already built** must stay `{:unavailable, :already_built}`. (Tests in Task 3.)
- Loom, Glassworks and Press exist in Ages I and II: `Helios.Catalogue.card/1` must return one entry without raising, and the explanation must not render twice. (Test in Task 4.)
- On a 375×667 phone the action sheet with the explanation box must keep every visible button in the viewport (`responsive.spec.ts`); the cost/chain lines are hidden below `sm`. (Checked in Task 6's phase gate.)
- A trade row with several options: the option sent must be the engine index of the clicked button even though the list is displayed sorted by total. (Test in Task 6.)

---

### Task 1: `EffectSpec` next to every effect (engine refactor, no behaviour change)

**Files:**
- Create: `core/src/domain/effect_spec.rs`
- Modify: `core/src/domain/mod.rs`, `core/src/domain/game_state.rs` (Effect type + constructors + appliers), `core/src/domain/player.rs` (`PlayerDirection` derives), `core/src/domain/supply/scientific.rs` (`ScientificSymbol` derives `Eq`), `core/src/game/scoring.rs` (two appliers), `core/src/lib.rs` (test module list)
- Test: `core/src/tests/effect_specs.rs` (new)

**Interfaces:**
- Produces: `crate::domain::EffectSpec` (enum below, `Debug, Clone, PartialEq, Eq, Serialize`); `crate::domain::Effect { pub spec: EffectSpec, run }` with `pub fn apply(&self, state: &mut GameState, player: PName)`.

- [ ] **Step 1: Write the failing test**

Create `core/src/tests/effect_specs.rs`:

```rust
use crate::domain::{
    Category, EffectSpec, PlayerDirection, PointCategory, ResourceType::*, ScientificSymbol,
};
use crate::engine::data::{STRUCTURES_BY_NAME, WONDERS, WONDERS_BY_NAME};

fn specs(card: &str) -> Vec<EffectSpec> {
    STRUCTURES_BY_NAME[card]
        .effects()
        .iter()
        .map(|effect| effect.spec.clone())
        .collect()
}

#[test]
fn every_card_and_wonder_stage_describes_its_effects() {
    for structure in STRUCTURES_BY_NAME.values() {
        assert!(!structure.effects().is_empty(), "{} has no effect", structure.name());
    }
    for wonder in WONDERS.iter() {
        for side in [&wonder.1, &wonder.2] {
            assert!(!side.initial_effects().is_empty(), "{} has no starting resource", side.name());
            for n in 1..=side.stages_total() {
                assert!(
                    !side.wonder_stage_with_idx(n).effects().is_empty(),
                    "{} stage {n} has no effect",
                    side.name()
                );
            }
        }
    }
}

#[test]
fn constructors_record_their_arguments() {
    assert_eq!(
        specs("Lumber Yard"),
        vec![EffectSpec::Produces { resources: vec![Wood], choice: false, tradable: true }]
    );
    assert_eq!(
        specs("Tree Farm"),
        vec![EffectSpec::Produces { resources: vec![Wood, Clay], choice: true, tradable: true }]
    );
    assert_eq!(
        specs("Forum"),
        vec![EffectSpec::Produces { resources: vec![Loom, Glass, Papyrus], choice: true, tradable: false }]
    );
    assert_eq!(specs("Tavern"), vec![EffectSpec::Coins(5)]);
    assert_eq!(specs("Stockade"), vec![EffectSpec::Military(1)]);
    assert_eq!(
        specs("Altar"),
        vec![EffectSpec::Points { category: PointCategory::CivilianP, points: 2 }]
    );
    assert_eq!(
        specs("Workshop"),
        vec![EffectSpec::Science { symbols: vec![ScientificSymbol::Gears], choice: false }]
    );
    assert_eq!(
        specs("East trading post"),
        vec![EffectSpec::TradeDiscount {
            directions: vec![PlayerDirection::East],
            resources: vec![Wood, Stone, Ore, Clay],
        }]
    );
    assert_eq!(
        specs("Haven"),
        vec![
            EffectSpec::CoinsPerCard {
                directions: vec![PlayerDirection::Own],
                categories: vec![Category::RM],
                coins: 1,
            },
            EffectSpec::PointsPerCard {
                category: PointCategory::CommercialP,
                directions: vec![PlayerDirection::Own],
                categories: vec![Category::RM],
                points: 1,
            },
        ]
    );
    assert_eq!(
        specs("Strategists Guild"),
        vec![EffectSpec::PointsPerDefeat {
            category: PointCategory::GuildP,
            directions: vec![PlayerDirection::East, PlayerDirection::West],
            points: 1,
        }]
    );
    let rhodos_b = &WONDERS_BY_NAME["Rhódos"].2;
    let stage_one: Vec<EffectSpec> = rhodos_b
        .wonder_stage_with_idx(1)
        .effects()
        .iter()
        .map(|e| e.spec.clone())
        .collect();
    assert_eq!(
        stage_one,
        vec![
            EffectSpec::Military(1),
            EffectSpec::Points { category: PointCategory::WonderP, points: 3 },
            EffectSpec::Coins(3),
        ]
    );
    let babylon_b = &WONDERS_BY_NAME["Babylon"].2;
    assert_eq!(babylon_b.wonder_stage_with_idx(2).effects()[0].spec, EffectSpec::PlayLastCard);
}
```

Register it in `core/src/lib.rs` inside `mod tests { … }` (keep alphabetical order): `pub mod effect_specs;`

- [ ] **Step 2: Run the test to verify it fails**

Run: `cd core && cargo test effect_specs`
Expected: compile error — `EffectSpec` not found, `spec` is not a field.

- [ ] **Step 3: Create `EffectSpec`**

`core/src/domain/effect_spec.rs`:

```rust
//! Plain-data description of a card or wonder-stage effect. It travels next to
//! the closure that applies it (`Effect`), so clients can explain an effect
//! without re-implementing it. Never read by the rules.
use super::player::PlayerDirection;
use super::point::{Point, PointCategory};
use super::structure::Category;
use super::supply::{Coin, MilitarySymbolCount, ResourceType, ScientificSymbol};
use serde::Serialize;

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum EffectSpec {
    /// `choice`: one of `resources` per turn; `tradable`: neighbours may buy it.
    Produces {
        resources: Vec<ResourceType>,
        choice: bool,
        tradable: bool,
    },
    Points {
        category: PointCategory,
        points: Point,
    },
    Military(MilitarySymbolCount),
    Science {
        symbols: Vec<ScientificSymbol>,
        choice: bool,
    },
    /// Paid once, when built.
    Coins(Coin),
    /// Buy `resources` from the players in `directions` for 1 coin.
    TradeDiscount {
        directions: Vec<PlayerDirection>,
        resources: Vec<ResourceType>,
    },
    /// Paid once, when built.
    CoinsPerCard {
        directions: Vec<PlayerDirection>,
        categories: Vec<Category>,
        coins: Coin,
    },
    /// Scored at game end.
    PointsPerCard {
        category: PointCategory,
        directions: Vec<PlayerDirection>,
        categories: Vec<Category>,
        points: Point,
    },
    CoinsPerWonderStage {
        directions: Vec<PlayerDirection>,
        coins: Coin,
    },
    PointsPerWonderStage {
        category: PointCategory,
        directions: Vec<PlayerDirection>,
        points: Point,
    },
    PointsPerDefeat {
        category: PointCategory,
        directions: Vec<PlayerDirection>,
        points: Point,
    },
    PlayLastCard,
    BuildFreeOncePerAge,
    CopyGuild,
    BuildFromDiscard,
}
```

`core/src/domain/mod.rs` — add:

```rust
pub mod effect_spec;
pub use effect_spec::*;
```

`core/src/domain/player.rs` — `PlayerDirection` needs value semantics:

```rust
#[derive(EnumIter, Debug, Clone, Copy, PartialEq, Eq, serde::Serialize)]
pub enum PlayerDirection {
```

`core/src/domain/supply/scientific.rs` — add `Eq`:

```rust
#[derive(Display, Copy, Clone, Debug, PartialEq, Eq, serde::Serialize)]
pub enum ScientificSymbol {
```

- [ ] **Step 4: Make `Effect` a struct and rewrite the constructors**

In `core/src/domain/game_state.rs` add `use super::effect_spec::EffectSpec;` and replace `pub type Effect = Box<dyn Fn(&mut GameState, PName) + Sync>;` with:

```rust
pub type EffectFn = Box<dyn Fn(&mut GameState, PName) + Sync>;

/// A card or wonder-stage effect: what it does (`spec`, for clients) and how
/// it changes the game (`run`).
pub struct Effect {
    pub spec: EffectSpec,
    run: EffectFn,
}

impl Effect {
    fn new(spec: EffectSpec, run: EffectFn) -> Self {
        Self { spec, run }
    }

    pub fn apply(&self, state: &mut GameState, player: PName) {
        (self.run)(state, player)
    }
}
```

Replace every constructor (from `all_resources_effect` to `build_free_from_discarded_cards_effect`) and `apply_player_effect` with the versions below. The closure bodies are unchanged; each is wrapped with its spec.

```rust
pub fn all_resources_effect(resource_types: ResourceTypes<'static>) -> Effect {
    Effect::new(
        EffectSpec::Produces { resources: resource_types.to_vec(), choice: false, tradable: true },
        apply_player_effect(Box::new(move |player_state| {
            player_state.resources_produced.add_all_resources(resource_types);
            player_state.tradable_resources.add_all_resources(resource_types);
        })),
    )
}

pub fn any_resources_effect(resource_types: ResourceTypes<'static>) -> Effect {
    Effect::new(
        EffectSpec::Produces { resources: resource_types.to_vec(), choice: true, tradable: true },
        apply_player_effect(Box::new(move |player_state| {
            player_state.resources_produced.add_any_resources(resource_types);
            player_state.tradable_resources.add_any_resources(resource_types);
        })),
    )
}

/// A choice resource only its owner may use (yellow cards, wonder stages).
pub fn owner_any_resources_effect(resource_types: ResourceTypes<'static>) -> Effect {
    Effect::new(
        EffectSpec::Produces { resources: resource_types.to_vec(), choice: true, tradable: false },
        apply_player_effect(Box::new(move |player_state| {
            player_state.resources_produced.add_any_resources(resource_types);
        })),
    )
}

pub fn point_effect(point_category: PointCategory, points: Point) -> Effect {
    let spec = EffectSpec::Points { category: point_category.clone(), points };
    let action = Box::new(move |_: &_, points_by_categories: &mut PointsMap| {
        points_by_categories.add(&point_category, points);
    });
    Effect::new(
        spec,
        apply_player_effect(Box::new(move |player_state| {
            player_state.point_actions.push(action.clone());
        })),
    )
}

pub fn military_effect(military_symbols: MilitarySymbolCount) -> Effect {
    Effect::new(
        EffectSpec::Military(military_symbols),
        apply_player_effect(Box::new(move |player_state| {
            player_state.military_symbols += military_symbols;
        })),
    )
}

pub fn all_scientific_effect(scientific_symbols: ScientificSymbols<'static>) -> Effect {
    Effect::new(
        EffectSpec::Science { symbols: scientific_symbols.to_vec(), choice: false },
        apply_player_effect(Box::new(move |player_state| {
            player_state.scientific_symbols_produced.add_all_symbols(scientific_symbols)
        })),
    )
}

pub fn any_scientific_effect(scientific_symbols: ScientificSymbols<'static>) -> Effect {
    Effect::new(
        EffectSpec::Science { symbols: scientific_symbols.to_vec(), choice: true },
        apply_player_effect(Box::new(move |player_state| {
            player_state.scientific_symbols_produced.add_any_symbols(scientific_symbols)
        })),
    )
}

pub fn coin_effect(coins: Coin) -> Effect {
    Effect::new(
        EffectSpec::Coins(coins),
        apply_player_effect(Box::new(move |player_state| {
            player_state.coins += coins;
        })),
    )
}

pub fn trade_effect(
    effect_directions: PlayerDirections<'static>,
    resource_types: ResourceTypes<'static>,
) -> Effect {
    Effect::new(
        EffectSpec::TradeDiscount {
            directions: effect_directions.to_vec(),
            resources: resource_types.to_vec(),
        },
        Box::new(move |game_state, player_name| {
            let affected_players: HashSet<PName> = game_state
                .neighbours
                .get_player_names_from_directions(&player_name, effect_directions);
            let action = Box::new(move |pname: &PName, resource_type: &ResourceType| {
                if affected_players.contains(pname) && resource_types.contains(resource_type) {
                    1
                } else {
                    2
                }
            });
            game_state
                .get_mut_player_state(&player_name)
                .add_trade_action_mut(action.clone());
        }),
    )
}

pub fn dynamic_coin_effect(
    effect_directions: PlayerDirections<'static>,
    categories: Categories<'static>,
    coins: Coin,
) -> Effect {
    Effect::new(
        EffectSpec::CoinsPerCard {
            directions: effect_directions.to_vec(),
            categories: categories.to_vec(),
            coins,
        },
        Box::new(move |game_state, player_name| {
            let affected_players: HashSet<PName> = game_state
                .neighbours
                .get_player_names_from_directions(&player_name, effect_directions);
            let structures_count =
                game_state.count_structures_for_players(&categories, &affected_players);
            game_state.get_mut_player_state(&player_name).coins +=
                coins * structures_count as Coin;
        }),
    )
}

pub fn dynamic_point_effect(
    point_category: PointCategory,
    effect_directions: PlayerDirections<'static>,
    categories: Categories<'static>,
    points: Point,
) -> Effect {
    Effect::new(
        EffectSpec::PointsPerCard {
            category: point_category.clone(),
            directions: effect_directions.to_vec(),
            categories: categories.to_vec(),
            points,
        },
        Box::new(move |game_state, player_name| {
            let affected_players: HashSet<PName> = game_state
                .neighbours
                .get_player_names_from_directions(&player_name, effect_directions);
            let point_category = point_category.clone();
            let action = Box::new(
                move |gs: &GameState, points_by_categories: &mut PointsMap| {
                    let structures_count =
                        gs.count_structures_for_players(&categories, &affected_players);
                    points_by_categories
                        .add(&point_category, points * structures_count as Point);
                },
            );
            game_state
                .get_mut_player_state(&player_name)
                .point_actions
                .push(action);
        }),
    )
}

pub fn dynamic_wonder_coin_effect(
    effect_directions: PlayerDirections<'static>,
    coins: Coin,
) -> Effect {
    Effect::new(
        EffectSpec::CoinsPerWonderStage { directions: effect_directions.to_vec(), coins },
        Box::new(move |game_state, player_name| {
            let affected_players: HashSet<PName> = game_state
                .neighbours
                .get_player_names_from_directions(&player_name, effect_directions);
            let wonder_stages_count = game_state.count_wonder_stages_for_players(&affected_players);
            game_state.get_mut_player_state(&player_name).coins +=
                coins * wonder_stages_count as Coin;
        }),
    )
}

pub fn dynamic_wonder_point_effect(
    point_category: PointCategory,
    effect_directions: PlayerDirections<'static>,
    points: Point,
) -> Effect {
    Effect::new(
        EffectSpec::PointsPerWonderStage {
            category: point_category.clone(),
            directions: effect_directions.to_vec(),
            points,
        },
        Box::new(move |game_state, player_name| {
            let affected_players: HashSet<PName> = game_state
                .neighbours
                .get_player_names_from_directions(&player_name, effect_directions);
            let point_category = point_category.clone();
            let action = Box::new(
                move |gs: &GameState, points_by_categories: &mut PointsMap| {
                    let wonder_stages_count =
                        gs.count_wonder_stages_for_players(&affected_players);
                    points_by_categories
                        .add(&point_category, points * wonder_stages_count as Point);
                },
            );
            game_state
                .get_mut_player_state(&player_name)
                .point_actions
                .push(action);
        }),
    )
}

pub fn dynamic_battle_lost_point_effect(
    point_category: PointCategory,
    effect_directions: PlayerDirections<'static>,
    points: Point,
) -> Effect {
    Effect::new(
        EffectSpec::PointsPerDefeat {
            category: point_category.clone(),
            directions: effect_directions.to_vec(),
            points,
        },
        Box::new(move |game_state, player_name| {
            let affected_players: HashSet<PName> = game_state
                .neighbours
                .get_player_names_from_directions(&player_name, effect_directions);
            let point_category = point_category.clone();
            let action = Box::new(
                move |gs: &GameState, points_by_categories: &mut PointsMap| {
                    let battles_lost_count =
                        gs.count_battles_lost_for_players(&affected_players);
                    points_by_categories
                        .add(&point_category, points * battles_lost_count as Point);
                },
            );
            game_state
                .get_mut_player_state(&player_name)
                .point_actions
                .push(action);
        }),
    )
}

pub fn play_last_card_effect() -> Effect {
    Effect::new(
        EffectSpec::PlayLastCard,
        apply_player_effect(Box::new(move |player_state| {
            player_state.can_play_last_card = true;
        })),
    )
}

pub fn construct_free_effect() -> Effect {
    Effect::new(
        EffectSpec::BuildFreeOncePerAge,
        Box::new(move |game_state: &mut GameState, player_name: String| {
            let current_age = game_state.current_age;
            game_state
                .get_mut_player_state(&player_name)
                .structure_builder
                .apply_construct_free_once_per_age(current_age);
        }),
    )
}

pub fn copy_guild_effect() -> Effect {
    Effect::new(
        EffectSpec::CopyGuild,
        apply_player_effect(Box::new(move |player_state| {
            player_state.can_copy_guild = true;
        })),
    )
}

pub fn build_free_from_discarded_cards_effect() -> Effect {
    Effect::new(
        EffectSpec::BuildFromDiscard,
        Box::new(|game_state, player_name| {
            game_state
                .events
                .push((player_name, EventType::ConstructFromDiscarded))
        }),
    )
}

fn apply_player_effect(
    player_state_mapper: Box<dyn Fn(&mut PlayerState) + Sync + Send>,
) -> EffectFn {
    Box::new(move |game_state: &mut GameState, player_name: String| {
        player_state_mapper(game_state.get_mut_player_state(&player_name));
    })
}
```

Then replace the two appliers in `game_state.rs` (`init` and `apply_player_decisions`):

```rust
        for (effect, player_name) in effects {
            effect.apply(self, player_name);
        }
```

and the two in `core/src/game/scoring.rs`:

```rust
                for effect in guild.effects() {
                    effect.apply(&mut self.state, name.clone());
                }
```

```rust
        for effect in guild.effects() {
            effect.apply(&mut self.state, name.clone());
        }
```

`cargo fmt` will reflow the long `Effect::new(EffectSpec::Produces { … })` lines.

- [ ] **Step 5: Run all core checks**

Run: `cd core && cargo fmt && cargo clippy --all-targets -- -D warnings && cargo test`
Expected: PASS, including the unchanged simulation/determinism tests. If clippy flags `type_complexity` on `EffectFn`, keep the alias (it is the fix).

- [ ] **Step 6: Commit**

```bash
git add core/src/domain/effect_spec.rs core/src/domain/mod.rs core/src/domain/game_state.rs core/src/domain/player.rs core/src/domain/supply/scientific.rs core/src/game/scoring.rs core/src/lib.rs core/src/tests/effect_specs.rs
git commit -m "refactor(core): carry a data EffectSpec next to every effect closure"
```

---

### Task 2: Card and wonder catalogue in `game_settings`

**Files:**
- Modify: `core/src/domain/structure.rs` (`dependents()` public), `core/src/game/settings.rs`, `core/src/game/mod.rs` (re-exports), `core/src/nif/dto.rs`
- Test: `core/src/tests/game_settings.rs`, `core/src/tests/nif_dto.rs`, `helios/test/helios/core_shapes_test.exs`

**Interfaces:**
- Consumes: `Effect.spec` (Task 1).
- Produces (Rust): `CardInfo { name, category, age, cost: CostInfo, chains_from: Vec<String>, chains_to: Vec<String>, effects: Vec<EffectSpec> }`; `CostInfo { coins: u32, resources: Vec<(ResourceType, u8)> }`; `WonderInfo { name, sides, boards: Vec<WonderBoardInfo> }`; `WonderBoardInfo { side, effects, stages: Vec<StageInfo> }`; `StageInfo { cost: CostInfo, effects }`.
- Produces (Elixir, via the NIF): `Core.game_settings()` cards gain `cost: %{coins: integer, resources: [{resource_atom, count}]}`, `chains_from: [String.t()]`, `chains_to: [String.t()]`, `effects: [effect]`; wonders gain `boards: [%{side: :a | :b, effects: [effect], stages: [%{cost: cost, effects: [effect]}]}]`. An `effect` is one of:
  `{:produces, %{resources: [atom], choice: boolean, tradable: boolean}}`, `{:points, %{category: point_category, points: integer}}`, `{:military, n}`, `{:science, %{symbols: [:compass | :gears | :tablet], choice: boolean}}`, `{:coins, n}`, `{:trade_discount, %{directions: [direction], resources: [atom]}}`, `{:coins_per_card, %{directions: [direction], categories: [category], coins: n}}`, `{:points_per_card, %{category: point_category, directions: [direction], categories: [category], points: n}}`, `{:coins_per_wonder_stage, %{directions: [direction], coins: n}}`, `{:points_per_wonder_stage, %{category: point_category, directions: [direction], points: n}}`, `{:points_per_defeat, %{category: point_category, directions: [direction], points: n}}`, `:play_last_card`, `:build_free_once_per_age`, `:copy_guild`, `:build_from_discard`; `direction` ∈ `:west | :east | :own`; `point_category` ∈ `:military | :treasury | :wonder | :civilian | :scientific | :commercial | :guild`; `category` as in the view (`:raw_material`, `:manufactured_good`, …).

- [ ] **Step 1: Write the failing tests**

Replace `settings_list_every_wonder_and_card` in `core/src/tests/game_settings.rs` with:

```rust
use crate::game::{CostInfo, EffectSpec, PointCategory, ResourceType::*};

#[test]
fn settings_list_every_wonder_and_card() {
    let s = settings();
    assert_eq!(s.engine_version, ENGINE_VERSION);
    assert_eq!(s.wonders.len(), 7);
    assert_eq!(s.wonders[0].name, "Rhódos");
    assert!(s.wonders.iter().all(|w| w.sides == vec![Side::A, Side::B]));
    assert_eq!(s.cards.len(), 78);
    for (name, category, age) in [
        ("Loom", Category::MG, 1),
        ("Loom", Category::MG, 2),
        ("Altar", Category::Civilian, 1),
        ("Builders Guild", Category::Guild, 3),
    ] {
        assert!(
            s.cards
                .iter()
                .any(|c| c.name == name && c.category == category && c.age == age),
            "{name} age {age} missing"
        );
    }
}

#[test]
fn cards_carry_cost_chains_and_effects() {
    let s = settings();
    let card = |name: &str| s.cards.iter().find(|c| c.name == name).unwrap();
    let temple = card("Temple");
    assert_eq!(
        temple.cost,
        CostInfo { coins: 0, resources: vec![(Wood, 1), (Clay, 1), (Glass, 1)] }
    );
    assert_eq!(temple.chains_from, vec!["Altar".to_string()]);
    assert_eq!(temple.chains_to, vec!["Pantheon".to_string()]);
    assert_eq!(
        temple.effects,
        vec![EffectSpec::Points { category: PointCategory::CivilianP, points: 3 }]
    );
    assert_eq!(card("Tree Farm").cost, CostInfo { coins: 1, resources: vec![] });
    assert_eq!(card("Aqueduct").cost.resources, vec![(Stone, 3)]);
}

#[test]
fn wonders_carry_both_boards_with_stages() {
    let s = settings();
    let gizah = s.wonders.iter().find(|w| w.name == "Gizah").unwrap();
    assert_eq!(gizah.boards.len(), 2);
    assert_eq!(gizah.boards[0].side, Side::A);
    assert_eq!(gizah.boards[0].stages.len(), 3);
    assert_eq!(gizah.boards[1].stages.len(), 4);
    assert_eq!(
        gizah.boards[0].effects,
        vec![EffectSpec::Produces { resources: vec![Stone], choice: false, tradable: true }]
    );
    let stage_two = &gizah.boards[0].stages[1];
    assert_eq!(stage_two.cost, CostInfo { coins: 0, resources: vec![(Wood, 3)] });
    assert_eq!(
        stage_two.effects,
        vec![EffectSpec::Points { category: PointCategory::WonderP, points: 5 }]
    );
}
```

(Remove `CardInfo` from that file's `use crate::game::{…}` line if it becomes unused.)

Append to `core/src/tests/nif_dto.rs`:

```rust
#[test]
fn settings_convert_with_catalogue() {
    use crate::nif::dto::{CostInfoDto, EffectSpecDto, GameSettingsDto, PointCategoryDto};
    let dto = GameSettingsDto::from(crate::game::settings());
    let altar = dto.cards.iter().find(|c| c.name == "Altar").unwrap();
    assert_eq!(altar.cost, CostInfoDto { coins: 0, resources: vec![] });
    assert_eq!(altar.chains_to, vec!["Temple".to_string()]);
    assert_eq!(
        altar.effects,
        vec![EffectSpecDto::Points { category: PointCategoryDto::Civilian, points: 2 }]
    );
    assert_eq!(dto.wonders[0].boards.len(), 2);
}
```

Extend `test "game_settings/0"` in `helios/test/helios/core_shapes_test.exs`:

```elixir
    altar = Enum.find(settings.cards, &(&1.name == "Altar"))
    assert %{cost: %{coins: 0, resources: []}, chains_from: [], chains_to: ["Temple"]} = altar
    assert [{:points, %{category: :civilian, points: 2}}] = altar.effects

    forum = Enum.find(settings.cards, &(&1.name == "Forum"))
    assert [{:produces, %{resources: [:loom, :glass, :papyrus], choice: true, tradable: false}}] =
             forum.effects

    gizah = Enum.find(settings.wonders, &(&1.name == "Gizah"))
    assert [%{side: :a, stages: [_, _, _]}, %{side: :b, stages: [_, _, _, _]}] = gizah.boards
    assert %{cost: %{coins: 0, resources: [{:wood, 3}]}, effects: [{:points, %{category: :wonder, points: 5}}]} =
             Enum.at(hd(gizah.boards).stages, 1)

    babylon_b = Enum.find(settings.wonders, &(&1.name == "Babylon")).boards |> Enum.at(1)
    assert [:play_last_card] = Enum.at(babylon_b.stages, 1).effects
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cd core && cargo test game_settings nif_dto`
Expected: compile errors (`CostInfo`, `boards`, `EffectSpecDto` unknown).

- [ ] **Step 3: Make dependents public**

`core/src/domain/structure.rs`: remove `#[cfg(test)]` above `pub fn dependents`.

- [ ] **Step 4: Build the catalogue**

Replace `core/src/game/settings.rs` with:

```rust
//! Static game metadata for clients: wonders with both boards, every card with
//! its cost, chains and effect descriptions.
use super::types::Side;
use super::ENGINE_VERSION;
use crate::domain::{
    resource_counts, Category, Coin, Effect, EffectSpec, ResourceCost, ResourceType, Structure,
    WonderSide, ALL_RESOURCE_TYPES,
};
use crate::engine::data::{
    AGE_III_STRUCTURES, AGE_II_STRUCTURES, AGE_I_STRUCTURES, GUILD_STRUCTURES, WONDERS,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GameSettings {
    pub engine_version: u32,
    pub wonders: Vec<WonderInfo>,
    pub cards: Vec<CardInfo>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WonderInfo {
    pub name: String,
    pub sides: Vec<Side>,
    pub boards: Vec<WonderBoardInfo>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WonderBoardInfo {
    pub side: Side,
    /// The starting resource.
    pub effects: Vec<EffectSpec>,
    pub stages: Vec<StageInfo>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StageInfo {
    pub cost: CostInfo,
    pub effects: Vec<EffectSpec>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CardInfo {
    pub name: String,
    pub category: Category,
    pub age: u8,
    pub cost: CostInfo,
    /// Built cards that make this one free (in the card's order).
    pub chains_from: Vec<String>,
    /// Cards this one makes free.
    pub chains_to: Vec<String>,
    pub effects: Vec<EffectSpec>,
}

/// Coins plus resources, summed per resource in `ALL_RESOURCE_TYPES` order.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CostInfo {
    pub coins: u32,
    pub resources: Vec<(ResourceType, u8)>,
}

pub fn settings() -> GameSettings {
    GameSettings {
        engine_version: ENGINE_VERSION,
        wonders: WONDERS
            .iter()
            .map(|wonder| WonderInfo {
                name: wonder.0.to_string(),
                sides: vec![Side::A, Side::B],
                boards: vec![board(Side::A, &wonder.1), board(Side::B, &wonder.2)],
            })
            .collect(),
        cards: AGE_I_STRUCTURES
            .iter()
            .chain(AGE_II_STRUCTURES.iter())
            .chain(AGE_III_STRUCTURES.iter())
            .chain(GUILD_STRUCTURES.iter())
            .map(card)
            .collect(),
    }
}

fn card(structure: &'static Structure<'static, Effect>) -> CardInfo {
    let (coins, resources) = *structure.cost();
    CardInfo {
        name: structure.name().to_string(),
        category: structure.category(),
        age: structure.age().number(),
        cost: cost(coins, resources),
        chains_from: structure.dependencies().iter().map(|s| s.to_string()).collect(),
        chains_to: structure.dependents().iter().map(|s| s.to_string()).collect(),
        effects: specs(structure.effects()),
    }
}

fn board(side: Side, wonder_side: &'static WonderSide<'static, Effect>) -> WonderBoardInfo {
    WonderBoardInfo {
        side,
        effects: specs(wonder_side.initial_effects()),
        stages: (1..=wonder_side.stages_total())
            .map(|n| {
                let stage = wonder_side.wonder_stage_with_idx(n);
                StageInfo {
                    cost: cost(0, stage.cost()),
                    effects: specs(stage.effects()),
                }
            })
            .collect(),
    }
}

fn specs(effects: &[Effect]) -> Vec<EffectSpec> {
    effects.iter().map(|effect| effect.spec.clone()).collect()
}

fn cost(coins: Coin, resources: &[ResourceCost]) -> CostInfo {
    CostInfo {
        coins,
        resources: ALL_RESOURCE_TYPES
            .iter()
            .zip(resource_counts(resources))
            .filter(|(_, count)| *count > 0)
            .map(|(resource, count)| (*resource, count))
            .collect(),
    }
}
```

In `core/src/game/mod.rs` change the re-exports:

```rust
pub use crate::domain::{Category, EffectSpec, PlayerDirection, PointCategory, ResourceType, ScientificSymbol};
pub use settings::{settings, CardInfo, CostInfo, GameSettings, StageInfo, WonderBoardInfo, WonderInfo};
```

(`ALL_RESOURCE_TYPES` order is Wood, Stone, Ore, Clay, Glass, Loom, Papyrus — the Temple test expects `[(Wood,1),(Clay,1),(Glass,1)]`, which follows that order.)

- [ ] **Step 5: DTOs**

In `core/src/nif/dto.rs` extend the `use crate::game::{…}` list with `CostInfo, EffectSpec, PlayerDirection, PointCategory, ScientificSymbol, StageInfo, WonderBoardInfo`, then add:

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq, NifUnitEnum)]
pub enum PointCategoryDto {
    Military,
    Treasury,
    Wonder,
    Civilian,
    Scientific,
    Commercial,
    Guild,
}

impl From<PointCategory> for PointCategoryDto {
    fn from(category: PointCategory) -> Self {
        match category {
            PointCategory::MilitaryP => Self::Military,
            PointCategory::TreasuryP => Self::Treasury,
            PointCategory::WonderP => Self::Wonder,
            PointCategory::CivilianP => Self::Civilian,
            PointCategory::ScientificP => Self::Scientific,
            PointCategory::CommercialP => Self::Commercial,
            PointCategory::GuildP => Self::Guild,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, NifUnitEnum)]
pub enum ScienceSymbolDto {
    Compass,
    Gears,
    Tablet,
}

impl From<ScientificSymbol> for ScienceSymbolDto {
    fn from(symbol: ScientificSymbol) -> Self {
        match symbol {
            ScientificSymbol::Compass => Self::Compass,
            ScientificSymbol::Gears => Self::Gears,
            ScientificSymbol::Tablet => Self::Tablet,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, NifUnitEnum)]
pub enum EffectDirectionDto {
    West,
    East,
    Own,
}

impl From<PlayerDirection> for EffectDirectionDto {
    fn from(direction: PlayerDirection) -> Self {
        match direction {
            PlayerDirection::West => Self::West,
            PlayerDirection::East => Self::East,
            PlayerDirection::Own => Self::Own,
        }
    }
}

fn convert<A, B: From<A>>(items: Vec<A>) -> Vec<B> {
    items.into_iter().map(Into::into).collect()
}

/// See `Helios.Core` docs for the Elixir shapes.
#[derive(Debug, Clone, PartialEq, Eq, NifTaggedEnum)]
pub enum EffectSpecDto {
    Produces { resources: Vec<ResourceDto>, choice: bool, tradable: bool },
    Points { category: PointCategoryDto, points: i32 },
    Military(u32),
    Science { symbols: Vec<ScienceSymbolDto>, choice: bool },
    Coins(u32),
    TradeDiscount { directions: Vec<EffectDirectionDto>, resources: Vec<ResourceDto> },
    CoinsPerCard { directions: Vec<EffectDirectionDto>, categories: Vec<CategoryDto>, coins: u32 },
    PointsPerCard {
        category: PointCategoryDto,
        directions: Vec<EffectDirectionDto>,
        categories: Vec<CategoryDto>,
        points: i32,
    },
    CoinsPerWonderStage { directions: Vec<EffectDirectionDto>, coins: u32 },
    PointsPerWonderStage { category: PointCategoryDto, directions: Vec<EffectDirectionDto>, points: i32 },
    PointsPerDefeat { category: PointCategoryDto, directions: Vec<EffectDirectionDto>, points: i32 },
    PlayLastCard,
    BuildFreeOncePerAge,
    CopyGuild,
    BuildFromDiscard,
}

impl From<EffectSpec> for EffectSpecDto {
    fn from(spec: EffectSpec) -> Self {
        match spec {
            EffectSpec::Produces { resources, choice, tradable } => Self::Produces {
                resources: convert(resources),
                choice,
                tradable,
            },
            EffectSpec::Points { category, points } => Self::Points { category: category.into(), points },
            EffectSpec::Military(n) => Self::Military(n),
            EffectSpec::Science { symbols, choice } => Self::Science { symbols: convert(symbols), choice },
            EffectSpec::Coins(n) => Self::Coins(n),
            EffectSpec::TradeDiscount { directions, resources } => Self::TradeDiscount {
                directions: convert(directions),
                resources: convert(resources),
            },
            EffectSpec::CoinsPerCard { directions, categories, coins } => Self::CoinsPerCard {
                directions: convert(directions),
                categories: convert(categories),
                coins,
            },
            EffectSpec::PointsPerCard { category, directions, categories, points } => {
                Self::PointsPerCard {
                    category: category.into(),
                    directions: convert(directions),
                    categories: convert(categories),
                    points,
                }
            }
            EffectSpec::CoinsPerWonderStage { directions, coins } => Self::CoinsPerWonderStage {
                directions: convert(directions),
                coins,
            },
            EffectSpec::PointsPerWonderStage { category, directions, points } => {
                Self::PointsPerWonderStage {
                    category: category.into(),
                    directions: convert(directions),
                    points,
                }
            }
            EffectSpec::PointsPerDefeat { category, directions, points } => Self::PointsPerDefeat {
                category: category.into(),
                directions: convert(directions),
                points,
            },
            EffectSpec::PlayLastCard => Self::PlayLastCard,
            EffectSpec::BuildFreeOncePerAge => Self::BuildFreeOncePerAge,
            EffectSpec::CopyGuild => Self::CopyGuild,
            EffectSpec::BuildFromDiscard => Self::BuildFromDiscard,
        }
    }
}

/// `%{coins: 1, resources: [{:wood, 2}]}`
#[derive(Debug, Clone, PartialEq, Eq, NifMap)]
pub struct CostInfoDto {
    pub coins: u32,
    pub resources: Vec<(ResourceDto, u8)>,
}

impl From<CostInfo> for CostInfoDto {
    fn from(cost: CostInfo) -> Self {
        Self {
            coins: cost.coins,
            resources: cost.resources.into_iter().map(|(r, n)| (r.into(), n)).collect(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, NifMap)]
pub struct StageInfoDto {
    pub cost: CostInfoDto,
    pub effects: Vec<EffectSpecDto>,
}

impl From<StageInfo> for StageInfoDto {
    fn from(stage: StageInfo) -> Self {
        Self { cost: stage.cost.into(), effects: convert(stage.effects) }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, NifMap)]
pub struct WonderBoardInfoDto {
    pub side: SideDto,
    pub effects: Vec<EffectSpecDto>,
    pub stages: Vec<StageInfoDto>,
}

impl From<WonderBoardInfo> for WonderBoardInfoDto {
    fn from(board: WonderBoardInfo) -> Self {
        Self {
            side: board.side.into(),
            effects: convert(board.effects),
            stages: convert(board.stages),
        }
    }
}
```

Extend the existing DTOs:

```rust
#[derive(Debug, Clone, PartialEq, Eq, NifMap)]
pub struct WonderInfoDto {
    pub name: String,
    pub sides: Vec<SideDto>,
    pub boards: Vec<WonderBoardInfoDto>,
}

impl From<WonderInfo> for WonderInfoDto {
    fn from(wonder: WonderInfo) -> Self {
        Self {
            name: wonder.name,
            sides: wonder.sides.into_iter().map(Into::into).collect(),
            boards: convert(wonder.boards),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, NifMap)]
pub struct CardInfoDto {
    pub name: String,
    pub category: CategoryDto,
    pub age: u8,
    pub cost: CostInfoDto,
    pub chains_from: Vec<String>,
    pub chains_to: Vec<String>,
    pub effects: Vec<EffectSpecDto>,
}

impl From<CardInfo> for CardInfoDto {
    fn from(card: CardInfo) -> Self {
        Self {
            name: card.name,
            category: card.category.into(),
            age: card.age,
            cost: card.cost.into(),
            chains_from: card.chains_from,
            chains_to: card.chains_to,
            effects: convert(card.effects),
        }
    }
}
```

`CategoryDto: From<Category>` already exists; `ResourceDto: From<ResourceType>` already exists.

- [ ] **Step 6: Run the checks**

Run: `cd core && cargo fmt && cargo clippy --all-targets -- -D warnings && cargo test`
Expected: PASS.
Run: `cd helios && mix test test/helios/core_shapes_test.exs`
Expected: PASS (the NIF is recompiled by `mix`).

- [ ] **Step 7: Commit**

```bash
cd helios && mix precommit
git add core/src/domain/structure.rs core/src/game/settings.rs core/src/game/mod.rs core/src/nif/dto.rs core/src/tests/game_settings.rs core/src/tests/nif_dto.rs helios/test/helios/core_shapes_test.exs
git commit -m "feat(core): expose card and wonder catalogue (cost, chains, effects) in game_settings"
```

---

### Task 3: `Chain` build option ("Free (you have …)")

**Files:**
- Modify: `core/src/domain/structure_builder.rs`, `core/src/game/legality.rs`, `core/src/game/view.rs`, `core/src/nif/dto.rs`
- Modify: `helios/lib/helios/games/choice.ex`, `helios/lib/helios_web/game_format.ex`, `helios/lib/helios/core.ex` (typespec doc), `helios/test/support/game_driver.ex`
- Test: `core/src/tests/game_view.rs`, `core/src/tests/game_simulation.rs`, `core/src/tests/nif_dto.rs`, `helios/test/helios/games/choice_test.exs`, `helios/test/helios/core_shapes_test.exs`, `helios/test/helios_web/game_format_test.exs`

**Interfaces:**
- Produces: `BuildOption::Chain(String)` in the view (Elixir: `{:chain, "Altar"}`), submitted with an empty payment; `StructureBuilder::built_dependency(&self, structure) -> Option<&'static str>`.
- Produces (Helios): `Choice.available?({:chain, _}) == true`; `GameFormat.option_label({:chain, card}, _) == "Free (you have #{card})"`.

- [ ] **Step 1: Write the failing core tests**

Append to `core/src/tests/game_view.rs`:

```rust
#[test]
fn chain_builds_name_the_card_that_makes_them_free() {
    let mut game = plain_game();
    game.give("p1", "Altar");
    game.set_hand("p1", &["Temple", "Altar"]);
    let hand = game.view("p1").unwrap().hand;
    assert_eq!(hand[0].build, BuildOption::Chain("Altar".to_string()));
    assert_eq!(
        hand[1].build,
        BuildOption::Unavailable { reason: ActionError::AlreadyBuilt }
    );
    game.submit("p1", super::game_helpers::build("Temple")).unwrap();
}

#[test]
fn forum_chains_from_either_trading_post_in_card_order() {
    for (posts, expected) in [
        (vec!["East trading post"], "East trading post"),
        (vec!["West trading post"], "West trading post"),
        (vec!["West trading post", "East trading post"], "East trading post"),
    ] {
        let mut game = plain_game();
        for post in &posts {
            game.give("p1", post);
        }
        game.set_hand("p1", &["Forum"]);
        for _ in 0..5 {
            let hand = game.view("p1").unwrap().hand;
            assert_eq!(hand[0].build, BuildOption::Chain(expected.to_string()), "{posts:?}");
        }
    }
}

#[test]
fn own_production_is_free_not_chain() {
    let mut game = plain_game();
    game.give("p1", "Lumber Yard");
    game.set_hand("p1", &["Stockade"]);
    assert_eq!(game.view("p1").unwrap().hand[0].build, BuildOption::Free);
}
```

Append to `core/src/tests/nif_dto.rs` (inside `view_parts_convert` or as a new test):

```rust
#[test]
fn chain_option_converts() {
    assert_eq!(
        BuildOptionDto::from(BuildOption::Chain("Altar".to_string())),
        BuildOptionDto::Chain("Altar".to_string())
    );
}
```

In `core/src/tests/game_simulation.rs` change the free arm of `push_options`:

```rust
        BuildOption::Free | BuildOption::Chain(_) | BuildOption::Coins(_) => {
            out.push(make(Payment::default()))
        }
```

- [ ] **Step 2: Run them to verify they fail**

Run: `cd core && cargo test chain forum own_production`
Expected: compile error — `BuildOption::Chain` does not exist.

- [ ] **Step 3: Implement in the engine**

`core/src/domain/structure_builder.rs` — replace `can_build_structure_from_dependencies` with:

```rust
    /// The first of `structure`'s prerequisites already built, in the card's
    /// dependency order (deterministic: the `HashMap` is only queried).
    pub fn built_dependency<T>(
        &self,
        structure: &'static Structure<'static, T>,
    ) -> Option<SName<'static>> {
        let built: HashSet<SName<'static>> =
            self.built_structures.values().flatten().cloned().collect();
        structure
            .dependencies()
            .iter()
            .find(|dependency| built.contains(*dependency))
            .copied()
    }
```

`core/src/game/legality.rs`:

```rust
pub(super) enum Requirement {
    Free,
    /// Free because the named prerequisite is built.
    Chain(String),
    Coins(u32),
    Trade {
        coin_cost: u32,
        resources: Vec<ResourceCost>,
    },
}
```

in `structure_requirement`:

```rust
        if let Some(from) = builder.built_dependency(card.0) {
            return Ok(Requirement::Chain(from.to_string()));
        }
```

in `settle`:

```rust
            Requirement::Free | Requirement::Chain(_) if payment.is_empty() => Ok((0, 0, 0)),
            Requirement::Coins(coins) if payment.is_empty() => Ok((*coins, 0, 0)),
            Requirement::Free | Requirement::Chain(_) | Requirement::Coins(_) => {
                Err(ActionError::InvalidPayment)
            }
```

`core/src/game/view.rs` — `BuildOption`:

```rust
    /// No cost at all, or own production covers it: submit with an empty payment.
    Free,
    /// Free because the named prerequisite is built: submit with an empty payment.
    Chain(String),
```

and in `option_for`: `Ok(Requirement::Chain(from)) => BuildOption::Chain(from),`.

`core/src/nif/dto.rs` — `BuildOptionDto` gains `Chain(String),` (doc: `{:chain, "Altar"}`), and `From<BuildOption>` gains `BuildOption::Chain(from) => Self::Chain(from),`.

- [ ] **Step 4: Run the core checks**

Run: `cd core && cargo fmt && cargo clippy --all-targets -- -D warnings && cargo test`
Expected: PASS. If an existing test asserted `BuildOption::Free` for a chain build, change it to `Chain(<prerequisite>)` — that is the intended behaviour change in the view (not in the rules).

- [ ] **Step 5: Write the failing Helios tests**

`helios/test/helios/games/choice_test.exs` — add inside `describe "resolve/2"`:

```elixir
    test "a chain build resolves to an empty payment", %{view: view} do
      hand = [%{name: "Temple", category: :civilian, age: 2, build: {:chain, "Altar"}, wonder_stage: {:unavailable, :cannot_afford}, free_build: false}]
      assert Choice.resolve(%{view | hand: hand}, %{card: "Temple", kind: "build", option: 0}) ==
               {:ok, {:build, %{card: "Temple", payment: @empty}}}

      assert Choice.available?({:chain, "Altar"})
    end
```

`helios/test/helios/core_shapes_test.exs` — add `defp build_option?({:chain, from}) when is_binary(from), do: true` above the catch-all clause.

`helios/test/helios_web/game_format_test.exs` — add to the option label test:

```elixir
    assert GameFormat.option_label({:chain, "Altar"}, neighbours) == "Free (you have Altar)"
```

- [ ] **Step 6: Implement in Helios**

`helios/lib/helios/games/choice.ex`:

```elixir
  def available?(:free), do: true
  def available?({:chain, _from}), do: true
```

```elixir
  defp payment(:free, _option), do: {:ok, @empty_payment}
  defp payment({:chain, _from}, _option), do: {:ok, @empty_payment}
```

`helios/lib/helios_web/game_format.ex`: `def option_label({:chain, from}, _neighbours), do: "Free (you have #{from})"` next to the `:free` clause.

`helios/test/support/game_driver.ex`: `defp first_payment({:chain, _}), do: @empty_payment`.

`helios/lib/helios/core.ex`: add to the moduledoc of `view/2` a line `Build options: :free | {:chain, card} | {:coins, n} | {:trade, [option]} | {:unavailable, reason}.`

- [ ] **Step 7: Run and commit**

Run: `cd helios && mix precommit`
Expected: PASS.

```bash
git add core/src/domain/structure_builder.rs core/src/game/legality.rs core/src/game/view.rs core/src/nif/dto.rs core/src/tests/game_view.rs core/src/tests/game_simulation.rs core/src/tests/nif_dto.rs helios/lib/helios/games/choice.ex helios/lib/helios_web/game_format.ex helios/lib/helios/core.ex helios/test/support/game_driver.ex helios/test/helios/games/choice_test.exs helios/test/helios/core_shapes_test.exs helios/test/helios_web/game_format_test.exs
git commit -m "feat(core): name the prerequisite of chain builds ({:chain, card})"
```

---

### Task 4: `Helios.Catalogue` and `HeliosWeb.CardText`

**Files:**
- Create: `helios/lib/helios/catalogue.ex`, `helios/lib/helios_web/card_text.ex`
- Test: `helios/test/helios/catalogue_test.exs`, `helios/test/helios_web/card_text_test.exs`

**Interfaces:**
- Consumes: `Core.game_settings/0` catalogue shape (Task 2).
- Produces: `Helios.Catalogue.card(name) :: card_info | nil`, `Helios.Catalogue.wonder_board(wonder, side) :: board | nil`, `Helios.Catalogue.stage(wonder, side, n) :: %{cost: cost, effects: [effect]} | nil`, `Helios.Catalogue.cards() :: [card_info]` (unique by name), `Helios.Catalogue.boards() :: [{{wonder, side}, board}]`.
- Produces: `HeliosWeb.CardText.effects([effect], neighbours) :: [String.t()]`, `effect(effect, neighbours) :: String.t()`, `cost(cost) :: String.t()` where `neighbours :: %{optional(:west) => String.t(), optional(:east) => String.t()}`.

- [ ] **Step 1: Write the failing tests**

`helios/test/helios/catalogue_test.exs`:

```elixir
defmodule Helios.CatalogueTest do
  use ExUnit.Case, async: true

  alias Helios.Catalogue

  test "cards are looked up by name; duplicate names return one entry" do
    assert %{name: "Altar", age: 1, chains_to: ["Temple"]} = Catalogue.card("Altar")
    assert %{name: "Loom", age: 1} = Catalogue.card("Loom")
    assert Catalogue.card("Not A Card") == nil
    assert length(Catalogue.cards()) == length(Enum.uniq_by(Catalogue.cards(), & &1.name))
  end

  test "wonder boards and stages" do
    assert %{side: :b, stages: [_, _, _, _]} = Catalogue.wonder_board("Gizah", :b)
    assert %{effects: [{:points, %{points: 5}}]} = Catalogue.stage("Gizah", :a, 2)
    assert Catalogue.stage("Gizah", :a, 9) == nil
    assert Catalogue.wonder_board("Atlantis", :a) == nil
  end
end
```

`helios/test/helios_web/card_text_test.exs`:

```elixir
defmodule HeliosWeb.CardTextTest do
  use ExUnit.Case, async: true

  alias Helios.Catalogue
  alias HeliosWeb.CardText

  test "single effects" do
    assert CardText.effect({:produces, %{resources: [:wood, :wood], choice: false, tradable: true}}, %{}) ==
             "Produces 2 wood"

    assert CardText.effect({:produces, %{resources: [:wood, :clay], choice: true, tradable: true}}, %{}) ==
             "Produces wood or clay (one per turn)"

    assert CardText.effect({:produces, %{resources: [:loom, :glass, :papyrus], choice: true, tradable: false}}, %{}) ==
             "Produces loom, glass or papyrus (one per turn, only for you)"

    assert CardText.effect({:points, %{category: :civilian, points: 3}}, %{}) == "Worth 3 victory points"
    assert CardText.effect({:military, 2}, %{}) == "+2 military strength"
    assert CardText.effect({:science, %{symbols: [:gears], choice: false}}, %{}) == "Science symbol: gears"

    assert CardText.effect({:science, %{symbols: [:compass, :gears, :tablet], choice: true}}, %{}) ==
             "One science symbol of your choice: compass, gears or tablet"

    assert CardText.effect({:coins, 5}, %{}) == "Gain 5 coins (once, when built)"

    assert CardText.effect(
             {:trade_discount, %{directions: [:east], resources: [:wood, :stone, :ore, :clay]}},
             %{east: "Cid"}
           ) == "Buy wood, stone, ore or clay from Cid for 1 coin"

    assert CardText.effect(
             {:trade_discount, %{directions: [:east, :west], resources: [:loom, :glass, :papyrus]}},
             %{}
           ) == "Buy loom, glass or papyrus from both neighbours for 1 coin"

    assert CardText.effect(
             {:coins_per_card, %{directions: [:east, :west, :own], categories: [:raw_material], coins: 1}},
             %{}
           ) == "Gain 1 coin per brown card in your city and your neighbours' cities (once, when built)"

    assert CardText.effect(
             {:points_per_card,
              %{category: :guild, directions: [:own], categories: [:raw_material, :manufactured_good, :guild], points: 1}},
             %{}
           ) == "1 victory point per brown, grey or purple card in your city (at game end)"

    assert CardText.effect({:points_per_defeat, %{category: :guild, directions: [:east, :west], points: 1}}, %{}) ==
             "1 victory point per defeat token in your neighbours' cities (at game end)"

    assert CardText.effect(:play_last_card, %{}) == "Play the last card of each age instead of discarding it"
  end

  test "costs" do
    assert CardText.cost(%{coins: 0, resources: []}) == "Free"
    assert CardText.cost(%{coins: 1, resources: []}) == "1 coin"
    assert CardText.cost(%{coins: 0, resources: [{:wood, 1}, {:clay, 2}]}) == "1 wood, 2 clay"
  end

  test "every card and every wonder stage in the catalogue renders" do
    for card <- Catalogue.cards() do
      lines = CardText.effects(card.effects, %{})
      assert lines != [] and Enum.all?(lines, &(is_binary(&1) and &1 != "")), card.name
      assert is_binary(CardText.cost(card.cost))
    end

    for {{wonder, side}, board} <- Catalogue.boards(), stage <- board.stages do
      assert [_ | _] = CardText.effects(stage.effects, %{}), "#{wonder} #{side}"
    end
  end
end
```

- [ ] **Step 2: Run them to verify they fail**

Run: `cd helios && mix test test/helios/catalogue_test.exs test/helios_web/card_text_test.exs`
Expected: FAIL (modules undefined).

- [ ] **Step 3: Implement `Helios.Catalogue`**

```elixir
defmodule Helios.Catalogue do
  @moduledoc """
  Static card and wonder data from the engine (`Helios.Core.game_settings/0`),
  indexed once and cached in `:persistent_term`. Loom, Glassworks and Press
  appear in two ages with identical data; the first entry wins.
  """

  alias Helios.Core

  @key {__MODULE__, :index}

  def card(name), do: Map.get(index().cards, name)
  def cards, do: Map.values(index().cards)
  def wonder_board(wonder, side), do: Map.get(index().boards, {wonder, side})
  def boards, do: Map.to_list(index().boards)

  def stage(wonder, side, n) when is_integer(n) and n >= 1 do
    case wonder_board(wonder, side) do
      nil -> nil
      board -> Enum.at(board.stages, n - 1)
    end
  end

  defp index do
    case :persistent_term.get(@key, nil) do
      nil ->
        index = build(Core.game_settings())
        :persistent_term.put(@key, index)
        index

      index ->
        index
    end
  end

  defp build(settings) do
    %{
      cards: settings.cards |> Enum.reverse() |> Map.new(&{&1.name, &1}),
      boards:
        for(wonder <- settings.wonders, board <- wonder.boards, into: %{}, do: {{wonder.name, board.side}, board})
    }
  end
end
```

- [ ] **Step 4: Implement `HeliosWeb.CardText`**

```elixir
defmodule HeliosWeb.CardText do
  @moduledoc """
  English for the engine's effect specs and costs (data from `Helios.Catalogue`).
  `neighbours` optionally names the card owner's neighbours
  (`%{west: "Bob", east: "Cid"}`); without it the text says "your west neighbour".
  """

  @colours %{
    raw_material: "brown",
    manufactured_good: "grey",
    civilian: "blue",
    commercial: "yellow",
    military: "red",
    scientific: "green",
    guild: "purple"
  }

  def effects(specs, neighbours \\ %{}), do: Enum.map(specs, &effect(&1, neighbours))

  def effect({:produces, %{resources: resources, choice: false}}, _n),
    do: "Produces " <> counted(resources)

  def effect({:produces, %{resources: resources, choice: true, tradable: true}}, _n),
    do: "Produces #{alternatives(resources, &to_string/1)} (one per turn)"

  def effect({:produces, %{resources: resources, choice: true, tradable: false}}, _n),
    do: "Produces #{alternatives(resources, &to_string/1)} (one per turn, only for you)"

  def effect({:points, %{points: points}}, _n), do: "Worth #{points(points)}"
  def effect({:military, n}, _n), do: "+#{n} military strength"

  def effect({:science, %{symbols: [symbol], choice: false}}, _n),
    do: "Science symbol: #{symbol}"

  def effect({:science, %{symbols: symbols, choice: true}}, _n),
    do: "One science symbol of your choice: #{alternatives(symbols, &to_string/1)}"

  def effect({:coins, n}, _n), do: "Gain #{coins(n)} (once, when built)"

  def effect({:trade_discount, %{directions: directions, resources: resources}}, n),
    do: "Buy #{alternatives(resources, &to_string/1)} from #{whom(directions, n)} for 1 coin"

  def effect({:coins_per_card, %{directions: d, categories: c, coins: coins}}, n),
    do: "Gain #{coins(coins)} per #{colours(c)} card #{where(d, n)} (once, when built)"

  def effect({:points_per_card, %{directions: d, categories: c, points: points}}, n),
    do: "#{points(points)} per #{colours(c)} card #{where(d, n)} (at game end)"

  def effect({:coins_per_wonder_stage, %{directions: d, coins: coins}}, n),
    do: "Gain #{coins(coins)} per wonder stage built #{where(d, n)} (once, when built)"

  def effect({:points_per_wonder_stage, %{directions: d, points: points}}, n),
    do: "#{points(points)} per wonder stage built #{where(d, n)} (at game end)"

  def effect({:points_per_defeat, %{directions: d, points: points}}, n),
    do: "#{points(points)} per defeat token #{where(d, n)} (at game end)"

  def effect(:play_last_card, _n), do: "Play the last card of each age instead of discarding it"
  def effect(:build_free_once_per_age, _n), do: "Once per age, build a structure for free"
  def effect(:copy_guild, _n), do: "At game end, copy a guild built by a neighbour"
  def effect(:build_from_discard, _n), do: "Build a card from the discard pile for free"

  def cost(%{coins: 0, resources: []}), do: "Free"

  def cost(%{coins: coins, resources: resources}) do
    parts = Enum.map(resources, fn {resource, count} -> "#{count} #{resource}" end)
    parts = if coins > 0, do: parts ++ [coins(coins)], else: parts
    Enum.join(parts, ", ")
  end

  defp counted(resources) do
    resources
    |> Enum.chunk_by(& &1)
    |> Enum.map_join(" and ", fn
      [one] -> to_string(one)
      [first | _] = many -> "#{length(many)} #{first}"
    end)
  end

  defp alternatives([only], fun), do: fun.(only)

  defp alternatives(items, fun) do
    {init, [last]} = Enum.split(items, -1)
    Enum.map_join(init, ", ", fun) <> " or " <> fun.(last)
  end

  defp colours(categories), do: alternatives(categories, &Map.fetch!(@colours, &1))

  defp points(1), do: "1 victory point"
  defp points(n), do: "#{n} victory points"
  defp coins(1), do: "1 coin"
  defp coins(n), do: "#{n} coins"

  defp neighbour(:west, n), do: Map.get(n, :west, "your west neighbour")
  defp neighbour(:east, n), do: Map.get(n, :east, "your east neighbour")

  defp whom(directions, n) do
    case Enum.sort(directions) do
      [:east, :west] -> "both neighbours"
      [one] -> neighbour(one, n)
    end
  end

  defp where(directions, n) do
    case Enum.sort(directions) do
      [:own] -> "in your city"
      [:east, :west] -> "in your neighbours' cities"
      [:east, :own, :west] -> "in your city and your neighbours' cities"
      [one] -> "in #{neighbour(one, n)}'s city"
    end
  end
end
```

- [ ] **Step 5: Run and commit**

Run: `cd helios && mix test test/helios/catalogue_test.exs test/helios_web/card_text_test.exs && mix precommit`
Expected: PASS. If a catalogue card fails to render, the failing message names it; add the missing clause rather than a catch-all.

```bash
git add helios/lib/helios/catalogue.ex helios/lib/helios_web/card_text.ex helios/test/helios/catalogue_test.exs helios/test/helios_web/card_text_test.exs
git commit -m "feat(web): cached card catalogue and English card text"
```

---

### Task 5: Cheapest-cost badge on hand cards

**Files:**
- Modify: `helios/lib/helios_web/game_format.ex` (`cheapest/1`, `cheapest_label/1`), `helios/lib/helios_web/game_assets.ex` (`token_path(:check)`), `helios/lib/helios_web/components/game_components/hand.ex` (`hand/1`)
- Test: `game_format_test.exs`, `game_assets_test.exs`, `hand_test.exs`

**Interfaces:**
- Produces: `GameFormat.cheapest(build_option) :: :free | {:chain, String.t()} | {:coins, non_neg_integer()} | {:unavailable, atom()}`; `GameFormat.cheapest_label(build_option) :: String.t()` ("Free", "Free (you have Altar)", "1 coin", "3 coins", or the error message); `GameAssets.token_path(:check)`.

- [ ] **Step 1: Write the failing tests**

`game_format_test.exs`:

```elixir
  test "cheapest way to build" do
    trade =
      {:trade,
       [
         %{west_coins: 2, east_coins: 2, bank_coins: 1, payment: %{}},
         %{west_coins: 1, east_coins: 0, bank_coins: 1, payment: %{}}
       ]}

    assert GameFormat.cheapest(:free) == :free
    assert GameFormat.cheapest({:chain, "Altar"}) == {:chain, "Altar"}
    assert GameFormat.cheapest({:coins, 1}) == {:coins, 1}
    assert GameFormat.cheapest(trade) == {:coins, 2}
    assert GameFormat.cheapest({:unavailable, :cannot_afford}) == {:unavailable, :cannot_afford}

    assert GameFormat.cheapest_label({:chain, "Altar"}) == "Free (you have Altar)"
    assert GameFormat.cheapest_label(trade) == "2 coins"
    assert GameFormat.cheapest_label({:coins, 1}) == "1 coin"
    assert GameFormat.cheapest_label({:unavailable, :already_built}) == "You already built that structure"
  end
```

`game_assets_test.exs`: `assert GameAssets.token_path(:check) == "/images/tokens/check.png"`.

`hand_test.exs`:

```elixir
  test "each hand card shows the cheapest way to build it" do
    html = render_component(&Hand.hand/1, hand: SampleViews.hand(), selected: nil, pending: nil)
    assert attrs(html, "#hand-card-0 .cost-badge", "data-cost") == ["unavailable"]
    assert attrs(html, "#hand-card-0 .cost-badge", "title") == ["You can't afford that"]
    assert attrs(html, "#hand-card-1 .cost-badge", "data-cost") == ["free"]
    assert attrs(html, "#hand-card-1 .cost-badge img", "src") == ["/images/tokens/check.png"]
    assert attrs(html, "#hand-card-2 .cost-badge", "data-cost") == ["1"]
    assert text(html, "#hand-card-2 .cost-badge") == "1"
    assert [class] = attrs(html, "#hand-card-0 img[alt='Baths']", "class")
    assert class =~ "grayscale"
  end
```

- [ ] **Step 2: Run to verify they fail**

Run: `cd helios && mix test test/helios_web/game_format_test.exs test/helios_web/game_assets_test.exs test/helios_web/components/game_components/hand_test.exs`
Expected: FAIL.

- [ ] **Step 3: Implement**

`game_format.ex`:

```elixir
  alias Helios.Games

  def cheapest(:free), do: :free
  def cheapest({:chain, from}), do: {:chain, from}
  def cheapest({:coins, n}), do: {:coins, n}
  def cheapest({:trade, options}), do: {:coins, options |> Enum.map(&option_total/1) |> Enum.min()}
  def cheapest({:unavailable, reason}), do: {:unavailable, reason}

  def option_total(%{west_coins: w, east_coins: e, bank_coins: b}), do: w + e + b

  def cheapest_label(option) do
    case cheapest(option) do
      :free -> "Free"
      {:chain, from} -> "Free (you have #{from})"
      {:coins, 1} -> "1 coin"
      {:coins, n} -> "#{n} coins"
      {:unavailable, reason} -> Games.error_message(reason)
    end
  end
```

(`HeliosWeb.GameFormat` already aliases `Helios.Games.Choice`; add `Helios.Games` to that alias.)

`game_assets.ex`: `def token_path(:check), do: static("/images/tokens/check.png")` next to `:coin`.

`hand.ex` — inside each hand card button, after the `<img>`, and make the image dim when unavailable:

```heex
          <img
            src={GameAssets.card_path(card.name)}
            alt={card.name}
            class={[
              "h-[98px] w-16 rounded-lg object-cover sm:h-[134px] sm:w-22 lg:h-[183px] lg:w-[120px] short:h-[86px] short:w-14",
              !GameFormat.available?(card.build) && "opacity-70 grayscale"
            ]}
          />
          <.cost_badge option={card.build} />
```

and add to the module:

```elixir
  attr :option, :any, required: true

  defp cost_badge(assigns) do
    assigns = assign(assigns, :cheapest, GameFormat.cheapest(assigns.option))

    ~H"""
    <span
      class={[
        "cost-badge pointer-events-none absolute -right-1.5 -top-1.5 flex h-6 min-w-6 items-center justify-center gap-0.5 rounded-full px-1 text-xs font-extrabold shadow ring-1",
        badge_class(@cheapest)
      ]}
      data-cost={badge_data(@cheapest)}
      title={GameFormat.cheapest_label(@option)}
    >
      <%= case @cheapest do %>
        <% {:coins, n} -> %>
          {n}<img src={GameAssets.token_path(:coin)} alt="" class="size-3.5" />
        <% {:unavailable, _} -> %>
          <.icon name="hero-x-mark" class="size-4" />
        <% _free -> %>
          <img src={GameAssets.token_path(:check)} alt="" class="size-4" />
      <% end %>
    </span>
    """
  end

  defp badge_data({:coins, n}), do: to_string(n)
  defp badge_data({:unavailable, _}), do: "unavailable"
  defp badge_data(_free), do: "free"

  defp badge_class({:coins, _}), do: "bg-amber-50 text-zinc-900 ring-amber-500"
  defp badge_class({:unavailable, _}), do: "bg-zinc-500 text-white ring-zinc-700"
  defp badge_class(_free), do: "bg-emerald-600 text-white ring-emerald-800"
```

(`data-cost="1"` for `{:coins, 1}`, so `text(…)` of the badge is "1": the coin `img` has empty alt.)

- [ ] **Step 4: Run and commit**

Run: `cd helios && mix test && mix precommit`
Expected: PASS.

```bash
git add helios/lib/helios_web/game_format.ex helios/lib/helios_web/game_assets.ex helios/lib/helios_web/components/game_components/hand.ex helios/test/helios_web/game_format_test.exs helios/test/helios_web/game_assets_test.exs helios/test/helios_web/components/game_components/hand_test.exs
git commit -m "feat(web): cheapest-cost badge on hand cards"
```

---

### Task 6: Action sheet — explanation box, action rows, payments on tap

**Files:**
- Create: `helios/lib/helios_web/components/game_components/card_info.ex`
- Modify: `helios/lib/helios_web/components/game_components/hand.ex` (`action_panel/1`; `option_group/1` replaced by `action_row/1`), `helios/lib/helios_web/game_format.ex` (`payment_label/2`), `helios/lib/helios_web/live/game_live.ex` (new attrs)
- Modify: `e2e/tests/support/game.ts`, `e2e/tests/game.spec.ts`
- Test: `hand_test.exs`, `helios/test/helios_web/components/game_components/card_info_test.exs` (new), `game_format_test.exs`, `helios/test/helios_web/live/game_live_test.exs`

**Interfaces:**
- Consumes: `Catalogue.card/1`, `CardText.effects/2`, `GameFormat.cheapest_label/1`, `GameFormat.option_total/1`, `CardZoom` (Phase A).
- Produces: `CardInfo.card_info/1` — attrs `name`, `neighbours` (map), `built` (list of the viewer's built card names); renders `#card-info`. `CardInfo.cost_icons/1` — attr `cost`. Phase C's read-only card sheet and stage popovers reuse both.
- Produces: `Hand.action_panel/1` attrs `card`, `neighbours`, `built` (list of names), `next_stage` (integer or nil). Ids: `#build-action`, `#wonder-action`, `#build-payments` › `#build-option-N`, `#wonder-payments` › `#wonder-option-N`, `#build-free-button`, `#discard-button`.
- Produces: `GameFormat.payment_label(option, neighbours) :: String.t()` e.g. "Buy 1 ore from ← Bob · 1 wood from Cid →".

- [ ] **Step 1: Write the failing tests**

`card_info_test.exs`:

```elixir
defmodule HeliosWeb.GameComponents.CardInfoTest do
  use ExUnit.Case, async: true

  import Phoenix.LiveViewTest
  import HeliosWeb.HTMLHelpers

  alias HeliosWeb.GameComponents.CardInfo

  test "explains effects, cost and chains" do
    html = render_component(&CardInfo.card_info/1, name: "Temple", neighbours: %{}, built: ["Altar"])
    assert text(html, "#card-info .effect-line") == "Worth 3 victory points"
    assert count(html, "#card-cost img") == 3
    assert text(html, "#card-chains-from") =~ "Altar"
    assert count(html, "#card-chains-from .hero-check") == 1
    assert text(html, "#card-chains-to") =~ "Pantheon"
  end

  test "a free card says so; unknown cards render nothing" do
    html = render_component(&CardInfo.card_info/1, name: "Lumber Yard", neighbours: %{}, built: [])
    assert text(html, "#card-cost") =~ "Free"
    assert count(html, "#card-chains-from") == 0
    assert render_component(&CardInfo.card_info/1, name: "Nope", neighbours: %{}, built: []) =~ ~r/^\s*$/
  end
end
```

`game_format_test.exs`:

```elixir
  test "payment labels say what is bought from whom" do
    option = %{payment: %{west: [{:ore, 1}], east: [{:wood, 2}]}, west_coins: 2, east_coins: 2, bank_coins: 0}
    assert GameFormat.payment_label(option, %{west: "Bob", east: "Cid"}) ==
             "Buy 1 ore from ← Bob · 2 wood from Cid →"
  end
```

Rewrite `hand_test.exs` action-panel tests (keep the hand tests):

```elixir
  @neighbours %{west: "Dee", east: "Bob"}

  defp panel(card, opts \\ []) do
    render_component(&Hand.action_panel/1,
      card: card,
      neighbours: @neighbours,
      built: Keyword.get(opts, :built, []),
      next_stage: Keyword.get(opts, :next_stage, 2)
    )
  end

  test "a row with one way to pay submits directly" do
    html = panel(card("Tree Farm"))
    assert attrs(html, "#build-action", "phx-click") == ["submit"]
    assert attrs(html, "#build-action", "phx-value-kind") == ["build"]
    assert attrs(html, "#build-action", "phx-value-option") == ["0"]
    assert text(html, "#build-action") =~ "1 coin"
    assert count(html, "#build-payments") == 0
  end

  test "a row with several trades reveals them sorted by cost, keeping the engine index" do
    html = panel(card("Baths"))
    assert text(html, "#wonder-action") =~ "Build wonder stage 2"
    assert text(html, "#wonder-action") =~ "from 2 coins"
    assert [js] = attrs(html, "#wonder-action", "phx-click")
    assert js =~ "#wonder-payments"
    assert [class] = attrs(html, "#wonder-payments", "class")
    assert class =~ "hidden"
    assert attrs(html, "#wonder-payments button", "id") == ["wonder-option-0", "wonder-option-1"]
    assert text(html, "#wonder-option-0") =~ "Buy 1 stone from ← Dee"
    assert text(html, "#wonder-option-1") =~ "Buy 1 stone from Bob →"
    assert attrs(html, "#wonder-option-1", "phx-value-option") == ["1"]
  end

  test "sorted display never changes the index sent" do
    expensive = %{payment: %{west: [{:ore, 2}], east: []}, west_coins: 4, east_coins: 0, bank_coins: 0}
    cheap = %{payment: %{west: [], east: [{:ore, 2}]}, west_coins: 0, east_coins: 2, bank_coins: 0}
    card = %{card("Baths") | build: {:trade, [expensive, cheap]}}
    html = panel(card)
    assert attrs(html, "#build-payments button", "id") == ["build-option-1", "build-option-0"]
    assert attrs(html, "#build-option-1", "phx-value-option") == ["1"]
  end

  test "unavailable rows are disabled with the reason; chain rows name the prerequisite" do
    html = panel(card("Baths"))
    assert attrs(html, "#build-action", "disabled") == [""]
    assert text(html, "#build-action") =~ "You can't afford that"

    chain = %{card("Tavern") | build: {:chain, "Altar"}}
    assert text(panel(chain), "#build-action") =~ "Free (you have Altar)"
  end

  test "Olympía free build and discard stay; the explanation box is present" do
    html = panel(card("Tavern"))
    assert attrs(html, "#build-free-button", "phx-value-kind") == ["build_free"]
    assert text(html, "#discard-button") =~ "+3"
    assert count(html, "#card-info") == 1
    assert text(html, "#wonder-action") =~ "Your wonder is complete"
  end
```

Keep "the action panel is a dismissable dialog" and "the card art opens a zoom", updated to call `panel/1`. Delete the old "unavailable builds show the reason; trade options list who gets paid", "free, coin and Olympía free-build options" and "a coin-cost card shows its cost once…" tests (their behaviour is covered above; the card cost now appears in the payments header, asserted next):

```elixir
  test "a coin cost on top of trading is stated once in the payments list" do
    two = %{payment: %{west: [{:wood, 1}], east: []}, west_coins: 2, east_coins: 0, bank_coins: 1}
    other = %{payment: %{west: [], east: [{:wood, 1}]}, west_coins: 0, east_coins: 2, bank_coins: 1}
    html = panel(%{card("Tree Farm") | build: {:trade, [two, other]}})
    assert text(html, "#build-card-cost") == "includes 1 coin card cost"
  end
```

`game_live_test.exs` — in "selecting a card toggles its action panel" replace the option assertions with:

```elixir
    assert has_element?(view, "#build-action")
    assert has_element?(view, "#wonder-action")
    assert has_element?(view, "#discard-button")
    assert has_element?(view, "#card-info")
```

- [ ] **Step 2: Run to verify they fail**

Run: `cd helios && mix test test/helios_web/components/game_components test/helios_web/game_format_test.exs test/helios_web/live/game_live_test.exs`
Expected: FAIL.

- [ ] **Step 3: `payment_label/2`**

In `game_format.ex`:

```elixir
  def payment_label(%{payment: %{west: west, east: east}}, %{west: west_name, east: east_name}) do
    parts =
      [{west, "← #{west_name}"}, {east, "#{east_name} →"}]
      |> Enum.reject(fn {bought, _who} -> bought == [] end)
      |> Enum.map(fn {bought, who} ->
        Enum.map_join(bought, ", ", fn {resource, count} -> "#{count} #{resource}" end) <> " from " <> who
      end)

    "Buy " <> Enum.join(parts, " · ")
  end
```

- [ ] **Step 4: `CardInfo` component**

`helios/lib/helios_web/components/game_components/card_info.ex`:

```elixir
defmodule HeliosWeb.GameComponents.CardInfo do
  @moduledoc "What a card does, what it costs, and how it chains."
  use HeliosWeb, :html

  alias Helios.Catalogue
  alias HeliosWeb.{CardText, GameAssets}

  attr :name, :string, required: true
  attr :neighbours, :map, default: %{}
  attr :built, :list, default: [], doc: "names of the viewer's built cards (for the ✓ on chains)"

  def card_info(assigns) do
    assigns = assign(assigns, :info, Catalogue.card(assigns.name))

    ~H"""
    <div
      :if={@info}
      id="card-info"
      class="rounded-lg bg-stone-50 px-3 py-2 text-sm leading-snug text-zinc-700 ring-1 ring-stone-200"
    >
      <p :for={line <- CardText.effects(@info.effects, @neighbours)} class="effect-line">{line}</p>
      <div class="mt-1 hidden flex-wrap items-center gap-x-3 gap-y-0.5 text-xs text-zinc-600 sm:flex">
        <span id="card-cost" class="flex items-center gap-1">
          <strong>Cost:</strong> <.cost_icons cost={@info.cost} />
        </span>
        <span :if={@info.chains_from != []} id="card-chains-from" class="flex items-center gap-1">
          <strong>Free with:</strong>
          <span :for={from <- @info.chains_from} class="flex items-center gap-0.5">
            {from}
            <.icon
              name={if from in @built, do: "hero-check", else: "hero-x-mark"}
              class={["size-3.5", if(from in @built, do: "text-emerald-600", else: "text-zinc-400")]}
            />
          </span>
        </span>
        <span :if={@info.chains_to != []} id="card-chains-to">
          <strong>Leads to:</strong> {Enum.join(@info.chains_to, ", ")}
        </span>
      </div>
    </div>
    """
  end

  attr :cost, :map, required: true

  def cost_icons(%{cost: %{coins: 0, resources: []}} = assigns), do: ~H"<span>Free</span>"

  def cost_icons(assigns) do
    ~H"""
    <span class="inline-flex flex-wrap items-center gap-0.5" title={HeliosWeb.CardText.cost(@cost)}>
      <%= for {resource, count} <- @cost.resources, _ <- 1..count//1 do %>
        <img src={GameAssets.token_path(resource)} alt={to_string(resource)} class="size-4" />
      <% end %>
      <span :if={@cost.coins > 0} class="inline-flex items-center gap-0.5">
        {@cost.coins}<img src={GameAssets.token_path(:coin)} alt="coins" class="size-4" />
      </span>
    </span>
    """
  end
end
```

- [ ] **Step 5: Rewrite `action_panel/1` around action rows**

In `hand.ex`, `import HeliosWeb.GameComponents.CardInfo, only: [card_info: 1]`. `action_panel/1` attrs: `card` (required), `neighbours` (required), `built` (list, default `[]`), `next_stage` (integer, default nil). Body of the right-hand column becomes:

```heex
        <div class="flex min-w-0 flex-1 flex-col gap-2">
          <h3 id="action-panel-title" class="pr-10 text-lg font-semibold text-zinc-900">
            {@card.name}
          </h3>
          <.card_info name={@card.name} neighbours={@neighbours} built={@built} />
          <.action_row
            id="build"
            kind="build"
            title="Build"
            card={@card.name}
            option={@card.build}
            neighbours={@neighbours}
          />
          <.action_row
            id="wonder"
            kind="wonder_stage"
            title={if @next_stage, do: "Build wonder stage #{@next_stage}", else: "Build wonder stage"}
            card={@card.name}
            option={@card.wonder_stage}
            neighbours={@neighbours}
          />
          <button
            :if={@card.free_build}
            id="build-free-button"
            type="button"
            phx-click="submit"
            phx-value-card={@card.name}
            phx-value-kind="build_free"
            phx-value-option="0"
            class={@row_class}
          >
            <span>Build for free (Olympía)</span><span class="font-bold">Free</span>
          </button>
          <button
            id="discard-button"
            type="button"
            phx-click="submit"
            phx-value-card={@card.name}
            phx-value-kind="discard"
            phx-value-option="0"
            class={[@row_class, "bg-white text-zinc-900 ring-1 ring-zinc-300 hover:bg-zinc-100"]}
          >
            <span class="flex items-center gap-1"><.icon name="hero-trash" class="size-4" /> Discard</span>
            <span class="flex items-center gap-1 font-bold">
              +3 <img src={GameAssets.token_path(:coin)} alt="coins" class="size-4" />
            </span>
          </button>
        </div>
```

with `assign(assigns, :row_class, @row)` where

```elixir
  @row "flex w-full items-center justify-between gap-3 rounded-lg bg-zinc-900 px-3 py-2 text-left text-sm font-semibold text-white shadow transition hover:bg-zinc-700 active:bg-zinc-800 pointer-coarse:min-h-11"
  @row_disabled "flex w-full cursor-not-allowed items-center justify-between gap-3 rounded-lg bg-zinc-200 px-3 py-2 text-left text-sm text-zinc-500 pointer-coarse:min-h-11"
  @payment "flex w-full items-center justify-between gap-3 rounded-lg bg-sky-50 px-3 py-2 text-left text-sm font-semibold text-sky-900 ring-1 ring-sky-200 transition hover:bg-sky-100 active:bg-sky-200 pointer-coarse:min-h-11"
```

Replace `option_group/1` with:

```elixir
  attr :id, :string, required: true
  attr :kind, :string, required: true
  attr :title, :string, required: true
  attr :card, :string, required: true
  attr :option, :any, required: true
  attr :neighbours, :map, required: true

  defp action_row(assigns) do
    assigns = assign(assigns, row: @row, row_disabled: @row_disabled, payment: @payment)

    ~H"""
    <%= case @option do %>
      <% {:unavailable, reason} -> %>
        <button id={"#{@id}-action"} type="button" disabled class={@row_disabled}>
          <span>{@title}</span><span>{Games.error_message(reason)}</span>
        </button>
      <% {:trade, [_, _ | _] = options} -> %>
        <button
          id={"#{@id}-action"}
          type="button"
          aria-expanded="false"
          aria-controls={"#{@id}-payments"}
          phx-click={
            JS.toggle(to: "##{@id}-payments", display: "flex")
            |> JS.toggle_attribute({"aria-expanded", "true", "false"})
          }
          class={@row}
        >
          <span>{@title}</span>
          <span class="flex items-center gap-1 font-bold">
            from {GameFormat.cheapest_label(@option)} <.icon name="hero-chevron-down" class="size-4" />
          </span>
        </button>
        <div
          id={"#{@id}-payments"}
          class="ml-2 hidden flex-col gap-1.5 border-l-4 border-sky-400 pl-2"
        >
          <p :if={GameFormat.card_cost(@option)} id={"#{@id}-card-cost"} class="text-xs text-zinc-600">
            includes {GameFormat.card_cost(@option)} {if GameFormat.card_cost(@option) == 1, do: "coin", else: "coins"} card cost
          </p>
          <button
            :for={{payment_option, index} <- options |> Enum.with_index() |> Enum.sort_by(fn {o, i} -> {GameFormat.option_total(o), i} end)}
            id={"#{@id}-option-#{index}"}
            type="button"
            phx-click="submit"
            phx-value-card={@card}
            phx-value-kind={@kind}
            phx-value-option={index}
            class={@payment}
          >
            <span>{GameFormat.payment_label(payment_option, @neighbours)}</span>
            <span class="shrink-0 font-bold">{GameFormat.option_total(payment_option)} coins</span>
          </button>
        </div>
      <% single -> %>
        <button
          id={"#{@id}-action"}
          type="button"
          phx-click="submit"
          phx-value-card={@card}
          phx-value-kind={@kind}
          phx-value-option="0"
          class={@row}
        >
          <span>{@title}</span><span class="font-bold">{GameFormat.cheapest_label(single)}</span>
        </button>
    <% end %>
    """
  end
```

(Single trade options keep `phx-value-option="0"`; `cheapest_label` gives their total.) Remove the now-unused `@action_button`/`@disabled_button` attributes and the `option_label/2` trade clause **only if** nothing references them (`mix compile --warnings-as-errors` in precommit will tell).

- [ ] **Step 6: GameLive passes the new attrs**

```heex
            <.action_panel
              :if={@selected_card}
              card={@selected_card}
              neighbours={%{
                west: GameFormat.player_name(@names, @view.west),
                east: GameFormat.player_name(@names, @view.east)
              }}
              built={Enum.map(@me.built, & &1.name)}
              next_stage={if @me.stages_built < @me.stages_total, do: @me.stages_built + 1}
            />
```

- [ ] **Step 7: e2e helpers**

`e2e/tests/support/game.ts` — add and use a build helper:

```ts
/** Taps Build in the open action sheet; picks the first payment if a choice is offered. */
export async function chooseBuild(page: Page): Promise<void> {
  await page.locator("#build-action").click();
  const payments = page.locator("#build-payments");
  if ((await payments.count()) > 0 && (await payments.isVisible())) {
    await payments.locator("button").first().click();
  }
}
```

In `playFirstBuildableOrDiscard` replace `await page.locator("#build-option-0").click();` with `await chooseBuild(page);`. In `e2e/tests/game.spec.ts` ("a player can change their choice…") replace `await a.page.locator("#build-option-0").click();` with `await chooseBuild(a.page);` and import `chooseBuild`.

- [ ] **Step 8: Run, phase-check the sheet on phones, commit**

```bash
cd helios && mix test && mix precommit
cd ../e2e && npx playwright test
```

Expected: PASS. If `responsive.spec.ts` "action sheet" fails because a button is below the fold on the 375×667 or 360×740 viewports, reduce the explanation to the effect lines on phones by making `#card-info` `text-xs` below `sm` (`text-xs sm:text-sm`) — do not hide `#build-action`/`#wonder-action`.

```bash
git add helios/lib/helios_web/components/game_components/card_info.ex helios/lib/helios_web/components/game_components/hand.ex helios/lib/helios_web/game_format.ex helios/lib/helios_web/live/game_live.ex helios/test/helios_web/components/game_components/card_info_test.exs helios/test/helios_web/components/game_components/hand_test.exs helios/test/helios_web/game_format_test.exs helios/test/helios_web/live/game_live_test.exs e2e/tests/support/game.ts e2e/tests/game.spec.ts
git commit -m "feat(web): explain cards in the action sheet; reveal payments on tap"
```

---

### Task 7: Stage strip with popovers on every wonder

**Files:**
- Create: `helios/lib/helios_web/components/game_components/stage_strip.ex`
- Modify: `helios/lib/helios_web/components/game_components/board.ex` (use the strip), `helios/lib/helios_web/components/game_components/player_panels.ex` (neighbour details image gets the strip)
- Test: `helios/test/helios_web/components/game_components/stage_strip_test.exs` (new), `board_test.exs`, `player_panels_test.exs`

**Interfaces:**
- Consumes: `Catalogue.stage/3`, `CardText.effects/2`, `CardInfo.cost_icons/1`.
- Produces: `StageStrip.stage_strip/1` — attrs `id` (container id), `player` (public player map), `owner` (display name). Slots are buttons `#stage-<player>-<n>` with `data-built`; popovers `#stage-info-<player>-<n>` (hidden; `role="dialog"`). Phase C's `PlayerBoard` renders it on every board.

- [ ] **Step 1: Write the failing tests**

`stage_strip_test.exs`:

```elixir
defmodule HeliosWeb.GameComponents.StageStripTest do
  use ExUnit.Case, async: true

  import Phoenix.LiveViewTest
  import HeliosWeb.HTMLHelpers

  alias Helios.SampleViews
  alias HeliosWeb.GameComponents.StageStrip

  test "every stage is a button opening an explanation" do
    html =
      render_component(&StageStrip.stage_strip/1,
        id: "wonder-stages-2",
        player: SampleViews.player("2"),
        owner: "Bob"
      )

    assert attrs(html, "#wonder-stages-2 button", "id") == ["stage-2-1", "stage-2-2", "stage-2-3"]
    assert attrs(html, "#stage-2-1", "data-built") == ["true"]
    assert attrs(html, "#stage-2-2", "data-built") == ["false"]
    assert [js] = attrs(html, "#stage-2-2", "phx-click")
    assert js =~ "#stage-info-2-2"
    assert attrs(html, "#stage-info-2-2", "role") == ["dialog"]
    assert text(html, "#stage-info-2-2") =~ "Gizah A · Stage 2"
    assert text(html, "#stage-info-2-2") =~ "Not built yet"
    assert text(html, "#stage-info-2-2") =~ "Worth 5 victory points"
    assert text(html, "#stage-info-2-1") =~ "Built"
  end
end
```

In `board_test.exs` change `#stage-1 img` / `#stage-2 img` to `#stage-1-1 img` / `#stage-1-2 img`. In `player_panels_test.exs` add to the first test:

```elixir
    assert count(html, "#east-panel-details #stage-2-1") == 1
```

- [ ] **Step 2: Run to verify they fail**

Run: `cd helios && mix test test/helios_web/components/game_components`
Expected: FAIL.

- [ ] **Step 3: Implement the strip**

```elixir
defmodule HeliosWeb.GameComponents.StageStrip do
  @moduledoc "Wonder stage slots along the top of a wonder image; each opens its cost and effect."
  use HeliosWeb, :html

  import HeliosWeb.GameComponents.CardInfo, only: [cost_icons: 1]

  alias Helios.Catalogue
  alias HeliosWeb.{CardText, GameAssets, GameFormat}

  attr :id, :string, required: true
  attr :player, :map, required: true
  attr :owner, :string, required: true

  def stage_strip(assigns) do
    ~H"""
    <div
      id={@id}
      class="absolute inset-x-0 top-0 flex justify-center gap-2 bg-linear-to-b from-black/50 to-transparent p-2"
    >
      <%= for n <- 1..@player.stages_total//1 do %>
        <button
          id={"stage-#{@player.name}-#{n}"}
          type="button"
          data-built={to_string(n <= @player.stages_built)}
          aria-haspopup="dialog"
          aria-label={"Stage #{n} of #{@owner}'s wonder"}
          phx-click={JS.toggle(to: "#stage-info-#{@player.name}-#{n}", display: "block")}
          class={[
            "h-12 w-8 overflow-hidden rounded ring-2 transition hover:scale-105 sm:h-16 sm:w-11 pointer-coarse:min-h-11 pointer-coarse:min-w-11",
            if(n <= @player.stages_built, do: "ring-amber-400", else: "bg-black/30 ring-white/60")
          ]}
        >
          <img
            :if={n <= @player.stages_built}
            src={GameAssets.card_back_path(min(n, 3))}
            alt={"Stage #{n} built"}
            class="h-full w-full object-cover"
          />
        </button>
        <.stage_info player={@player} n={n} owner={@owner} />
      <% end %>
    </div>
    """
  end

  attr :player, :map, required: true
  attr :n, :integer, required: true
  attr :owner, :string, required: true

  defp stage_info(assigns) do
    assigns = assign(assigns, :stage, Catalogue.stage(assigns.player.wonder, assigns.player.side, assigns.n))

    ~H"""
    <div
      id={"stage-info-#{@player.name}-#{@n}"}
      role="dialog"
      aria-label={"Stage #{@n}"}
      phx-click-away={JS.hide(to: "#stage-info-#{@player.name}-#{@n}")}
      phx-window-keydown={JS.hide(to: "#stage-info-#{@player.name}-#{@n}")}
      phx-key="Escape"
      class="fixed inset-x-4 top-1/3 z-50 mx-auto hidden max-w-xs rounded-xl bg-white p-3 text-left text-sm text-zinc-800 shadow-2xl ring-1 ring-black/10"
    >
      <p class="font-semibold">
        {@player.wonder} {GameFormat.side_label(@player.side)} · Stage {@n}
      </p>
      <p class="text-xs text-zinc-500">
        {@owner} · {if @n <= @player.stages_built, do: "Built", else: "Not built yet"}
      </p>
      <%= if @stage do %>
        <p class="mt-1 flex items-center gap-1"><strong>Cost:</strong> <.cost_icons cost={@stage.cost} /></p>
        <p :for={line <- CardText.effects(@stage.effects)} class="effect-line">{line}</p>
      <% end %>
    </div>
    """
  end
end
```

- [ ] **Step 4: Use it**

`board.ex`: replace the whole `#wonder-stages` div with

```heex
        <.stage_strip id="wonder-stages" player={@player} owner="You" />
```

(import `HeliosWeb.GameComponents.StageStrip, only: [stage_strip: 1]`). The Phase A test "built stages sit on the top edge" keeps passing (`top-0`).

`player_panels.ex`: in the details block replace the plain wonder `<img>` with

```heex
        <div class="relative overflow-hidden rounded">
          <img
            src={GameAssets.wonder_path(@player.wonder, @player.side)}
            alt={"#{@player.wonder} #{GameFormat.side_label(@player.side)}"}
            class="aspect-[16/5] w-full object-cover"
          />
          <.stage_strip
            id={"wonder-stages-#{@player.name}"}
            player={@player}
            owner={GameFormat.player_name(@names, @player.name)}
          />
        </div>
```

The popovers are `fixed`, so the `overflow-hidden` image wrapper does not clip them.

- [ ] **Step 5: Phase gate and commit**

```bash
cd core && cargo fmt --check && cargo clippy --all-targets -- -D warnings && cargo test
cd ../helios && mix precommit
cd ../e2e && npx playwright test
```

Expected: all green. On touch viewports the stage buttons are ≥ 44px (`pointer-coarse:min-h-11 min-w-11`).

```bash
git add helios/lib/helios_web/components/game_components/stage_strip.ex helios/lib/helios_web/components/game_components/board.ex helios/lib/helios_web/components/game_components/player_panels.ex helios/test/helios_web/components/game_components/stage_strip_test.exs helios/test/helios_web/components/game_components/board_test.exs helios/test/helios_web/components/game_components/player_panels_test.exs
git commit -m "feat(web): explain any player's wonder stages"
```
