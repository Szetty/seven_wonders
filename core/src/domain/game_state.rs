use super::effects::Effects;
use super::player::{Neighbours, PName, Player, PlayerDirections};
use super::point::PointCategory::{MilitaryP, ScientificP, TreasuryP};
use super::point::{Point, PointCategory, PointsMap};
use super::structure::{Age, Categories, Structure};
use super::structure_builder::StructureBuilder;
use super::supply::{
    calculate_military_points, calculate_treasury_points, BattleTokens, Coin, MilitarySymbolCount,
    ResourceCost, ResourceCostOptions, ResourceType, ResourceTypes, ResourcesProduced,
    ScientificSymbols, ScientificSymbolsProduced, TradeValue,
};
use super::wonder::WonderSide;
use serde::ser::{Serialize, SerializeStruct, Serializer};
use std::collections::{HashMap, HashSet};
use std::fmt;

#[derive(Default, Debug)]
pub struct GameState {
    pub deck: Deck,
    pub player_states: HashMap<PName, PlayerState>,
    pub neighbours: Neighbours,
    pub cards_discarded: Cards,
    pub current_age: Age,
    pub current_age_cards: HashMap<PName, Cards>,
    pub events: Events,
}

impl GameState {
    pub fn new(deck: Deck, players_with_wonders: PlayersWithWonders) -> Self {
        Self {
            deck,
            neighbours: Neighbours::new(
                players_with_wonders
                    .iter()
                    .map(|(player, _)| player.0.clone())
                    .collect(),
            ),
            player_states: players_with_wonders
                .into_iter()
                .map(|(player, wonder_side)| {
                    (player.0.clone(), PlayerState::new(player, wonder_side))
                })
                .collect(),
            ..Default::default()
        }
    }
    pub fn init(&mut self) {
        let mut effects: Vec<(&Effect, PName)> = Default::default();
        for player_state in self.player_states.values_mut() {
            effects.extend(
                player_state
                    .init()
                    .iter()
                    .map(|e| (e, player_state.player_name())),
            );
        }
        for (effect, player_name) in effects {
            (*effect)(self, player_name);
        }
    }
    pub fn get_neighbour_player_states(&self, player_name: &PName) -> (&PlayerState, &PlayerState) {
        let (west, east) = self.neighbours.get_neighbours(player_name);
        (self.get_player_state(west), self.get_player_state(east))
    }
    pub fn get_player_state(&self, player_name: &PName) -> &PlayerState {
        self.player_states.get(player_name).unwrap()
    }
    fn get_mut_player_state(&mut self, player_name: &PName) -> &mut PlayerState {
        self.player_states.get_mut(player_name).unwrap()
    }
    pub fn apply_player_decisions(&mut self, player_decisions: HashMap<PName, PlayerDecision>) {
        let current_age = self.current_age;
        let mut effects: Vec<(&Effect, PName)> = Default::default();
        for (player_name, player_decision) in player_decisions {
            let player_state = self.get_mut_player_state(&player_name);
            match player_decision {
                PlayerDecision::BuildStructure(Card(structure)) => {
                    effects.extend(
                        player_state
                            .build_structure(structure)
                            .iter()
                            .map(|e| (e, player_name.clone())),
                    );
                }
                PlayerDecision::BuildNextWonderStage(Card(_)) => {
                    // TODO store cards used for stages?
                    effects.extend(
                        player_state
                            .build_next_wonder_stage()
                            .iter()
                            .map(|e| (e, player_name.clone())),
                    );
                }
                PlayerDecision::Discard(card) => {
                    player_state.coins += 3;
                    self.cards_discarded.push(card);
                }
                PlayerDecision::ConstructForFreeOncePerAge(Card(structure)) => {
                    effects.extend(
                        player_state
                            .build_structure(structure)
                            .iter()
                            .map(|e| (e, player_name.clone())),
                    );
                    player_state
                        .structure_builder
                        .apply_used_construct_for_free_for_age(&current_age);
                }
            }
        }
        for (effect, player_name) in effects {
            (*effect)(self, player_name);
        }
    }
    pub fn calculate_points(&self) -> HashMap<PName, HashMap<PointCategory, Point>> {
        self.player_states
            .iter()
            .map(|(player_name, player_state)| {
                (player_name.clone(), player_state.calculate_points(self))
            })
            .collect()
    }
    fn count_structures_for_players(
        &self,
        categories: &Categories,
        players: &HashSet<PName>,
    ) -> usize {
        players
            .iter()
            .map(|player_name| {
                self.get_player_state(player_name)
                    .structure_builder
                    .count_structures_for_categories(categories)
            })
            .sum()
    }

    fn count_wonder_stages_for_players(&self, players: &HashSet<PName>) -> WonderStagesBuilt {
        players
            .iter()
            .map(|player_name| {
                let player_state = self.get_player_state(player_name);
                player_state.wonder_stages_built
            })
            .sum()
    }

    fn count_battles_lost_for_players(&self, players: &HashSet<PName>) -> usize {
        players
            .iter()
            .map(|player_name| {
                let player_state = self.get_player_state(player_name);
                player_state
                    .battle_tokens
                    .iter()
                    .filter(|bt| **bt < 0)
                    .count()
            })
            .sum()
    }
}

impl Serialize for GameState {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut s = serializer.serialize_struct("GameState", 9)?;
        s.serialize_field("type", "GameState")?;
        s.serialize_field("deck", &self.deck)?;
        s.serialize_field("player_states", &self.player_states)?;
        s.serialize_field("neighbours", &self.neighbours)?;
        s.serialize_field("cards_discarded", &self.cards_discarded)?;
        s.serialize_field("current_age", &self.current_age)?;
        s.serialize_field("current_age_cards", &self.current_age_cards)?;
        s.serialize_field("events", &self.events)?;
        s.serialize_field("points", &self.calculate_points())?;
        s.end()
    }
}

pub type Deck = (Cards, Cards, Cards);
pub type Cards = Vec<Card>;
#[derive(Debug, PartialEq, serde::Serialize)]
pub struct Card(pub &'static Structure<'static, Effect>);
pub type PlayersWithWonders = Vec<(Player, &'static WonderSide<'static, Effect>)>;
pub enum PlayerDecision {
    BuildStructure(Card),
    BuildNextWonderStage(Card),
    Discard(Card),
    ConstructForFreeOncePerAge(Card),
}

pub struct PlayerState {
    pub player: Player,
    pub wonder: &'static WonderSide<'static, Effect>,
    pub wonder_stages_built: WonderStagesBuilt,
    pub coins: Coin,
    pub military_symbols: MilitarySymbolCount,
    pub battle_tokens: BattleTokens,
    pub scientific_symbols_produced: ScientificSymbolsProduced,
    pub resources_produced: ResourcesProduced,
    pub structure_builder: StructureBuilder,
    pub point_actions: Actions<dyn Fn(&GameState, &mut PointsMap) + Sync + Send>,
    pub trade_actions: Actions<dyn Fn(&PName, &ResourceType) -> TradeValue + Sync + Send>,
    pub can_play_last_card: bool,
    pub can_copy_guild: bool,
}

impl PlayerState {
    pub fn new(player: Player, wonder_side: &'static WonderSide<'static, Effect>) -> Self {
        Self {
            player,
            wonder: wonder_side,
            military_symbols: 0,
            coins: 0,
            wonder_stages_built: 0,
            battle_tokens: vec![],
            scientific_symbols_produced: Default::default(),
            resources_produced: Default::default(),
            structure_builder: Default::default(),
            point_actions: vec![],
            trade_actions: vec![],
            can_play_last_card: false,
            can_copy_guild: false,
        }
    }
    pub fn init(&mut self) -> &'static Effects<Effect> {
        // coins, trade actions, wonder effects
        self.coins = 3;
        self.trade_actions.push(Box::new(|_, _| 2));
        self.wonder.initial_effects()
    }
    pub fn player_name(&self) -> PName {
        self.player.name().clone()
    }
    pub fn calculate_points(&self, game_state: &GameState) -> HashMap<PointCategory, Point> {
        let mut points_map = PointsMap::new();
        points_map.add(&TreasuryP, calculate_treasury_points(self.coins));
        points_map.add(&MilitaryP, calculate_military_points(&self.battle_tokens));
        points_map.add(
            &ScientificP,
            self.scientific_symbols_produced
                .calculate_scientific_points(),
        );
        for action in self.point_actions.iter() {
            (*action)(game_state, &mut points_map)
        }
        points_map.to_hash_map()
    }
    pub fn cover_resource_costs(&self, resource_costs: Vec<ResourceCost>) -> ResourceCostOptions {
        self.resources_produced.cover_resource_costs(resource_costs)
    }
    pub fn apply_trading(&self, player_name: &PName, resource_type: &ResourceType) -> TradeValue {
        self.trade_actions
            .iter()
            .map(|trade_action| trade_action(player_name, resource_type))
            .min()
            .unwrap()
    }
    pub fn add_trade_action_mut(
        &mut self,
        action: Action<dyn Fn(&PName, &ResourceType) -> TradeValue + Sync + Send>,
    ) -> &Self {
        self.trade_actions.push(action);
        self
    }
    pub fn add_trade_action_move(
        mut self,
        action: Action<dyn Fn(&PName, &ResourceType) -> TradeValue + Sync + Send>,
    ) -> Self {
        self.trade_actions.push(action);
        self
    }
    pub fn build_structure(
        &mut self,
        structure: &'static Structure<'static, Effect>,
    ) -> &'static Effects<Effect> {
        self.structure_builder.build_structure(structure);
        structure.effects()
    }
    pub fn build_next_wonder_stage(&mut self) -> &'static Effects<Effect> {
        self.wonder_stages_built += 1;
        self.wonder
            .wonder_stage_with_idx(self.wonder_stages_built.into())
            .effects()
    }
}

impl fmt::Debug for PlayerState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("PlayerState")
            .field("name", &self.player.name())
            .field("wonder", &self.wonder)
            .field("coins", &self.coins)
            .field("military_symbols", &self.military_symbols)
            .field("battle_tokens", &self.battle_tokens)
            .field(
                "scientific_symbols_produced",
                &self.scientific_symbols_produced,
            )
            .field("structure_builder", &self.structure_builder)
            .field("resources_produced", &self.resources_produced)
            .field("wonder_stages_built", &self.wonder_stages_built)
            .field("point_actions_len", &self.point_actions.len())
            .field("trade_actions_len", &self.trade_actions.len())
            .field("can_play_last_card", &self.can_play_last_card)
            .field("can_copy_guild", &self.can_copy_guild)
            .finish()
    }
}

impl Serialize for PlayerState {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut s = serializer.serialize_struct("PlayerState", 13)?;
        s.serialize_field("type", "PlayerState")?;
        s.serialize_field("name", &self.player.name())?;
        s.serialize_field("wonder", &self.wonder)?;
        s.serialize_field("coins", &self.coins)?;
        s.serialize_field("military_symbols", &self.military_symbols)?;
        s.serialize_field("battle_tokens", &self.battle_tokens)?;
        s.serialize_field(
            "scientific_symbols_produced",
            &self.scientific_symbols_produced,
        )?;
        s.serialize_field("structure_builder", &self.structure_builder)?;
        s.serialize_field("resources_produced", &self.resources_produced)?;
        s.serialize_field("wonder_stages_built", &self.wonder_stages_built)?;
        s.serialize_field("point_actions_len", &self.point_actions.len())?;
        s.serialize_field("trade_actions_len", &self.trade_actions.len())?;
        s.serialize_field("can_play_last_card", &self.can_play_last_card)?;
        s.serialize_field("can_copy_guild", &self.can_copy_guild)?;
        s.end()
    }
}

pub type WonderStagesBuilt = u8;

type Actions<T> = Vec<Action<T>>;
type Action<T> = Box<T>;

pub type Effect = Box<dyn Fn(&mut GameState, PName) + Sync>;

pub type Events = Vec<Event>;
pub type Event = (PName, EventType);
#[derive(PartialEq, Debug, serde::Serialize)]
pub enum EventType {
    ConstructFromDiscarded,
    PlayLastCard,
    CopyGuild,
}

pub fn all_resources_effect(resource_types: ResourceTypes<'static>) -> Effect {
    apply_player_effect(Box::new(move |player_state| {
        player_state
            .resources_produced
            .add_all_resources(resource_types)
    }))
}

pub fn any_resources_effect(resource_types: ResourceTypes<'static>) -> Effect {
    apply_player_effect(Box::new(move |player_state| {
        player_state
            .resources_produced
            .add_any_resources(resource_types)
    }))
}

pub fn point_effect(point_category: PointCategory, points: Point) -> Effect {
    let action = Box::new(move |_: &_, points_by_categories: &mut PointsMap| {
        points_by_categories.add(&point_category, points);
    });
    apply_player_effect(Box::new(move |player_state| {
        player_state.point_actions.push(action.clone());
    }))
}

pub fn military_effect(military_symbols: MilitarySymbolCount) -> Effect {
    apply_player_effect(Box::new(move |player_state| {
        player_state.military_symbols += military_symbols;
    }))
}

pub fn all_scientific_effect(scientific_symbols: ScientificSymbols<'static>) -> Effect {
    apply_player_effect(Box::new(move |player_state| {
        player_state
            .scientific_symbols_produced
            .add_all_symbols(scientific_symbols)
    }))
}

pub fn any_scientific_effect(scientific_symbols: ScientificSymbols<'static>) -> Effect {
    apply_player_effect(Box::new(move |player_state| {
        player_state
            .scientific_symbols_produced
            .add_any_symbols(scientific_symbols)
    }))
}

pub fn coin_effect(coins: Coin) -> Effect {
    apply_player_effect(Box::new(move |player_state| {
        player_state.coins += coins;
    }))
}

pub fn trade_effect(
    effect_directions: PlayerDirections<'static>,
    resource_types: ResourceTypes<'static>,
) -> Effect {
    Box::new(move |game_state, player_name| {
        let affected_players: HashSet<PName> = game_state
            .neighbours
            .get_player_names_from_directions(&player_name, &effect_directions);
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
    })
}

pub fn dynamic_coin_effect(
    effect_directions: PlayerDirections<'static>,
    categories: Categories<'static>,
    coins: Coin,
) -> Effect {
    Box::new(move |game_state, player_name| {
        let affected_players: HashSet<PName> = game_state
            .neighbours
            .get_player_names_from_directions(&player_name, &effect_directions);
        let structures_count =
            game_state.count_structures_for_players(&categories, &affected_players);
        game_state.get_mut_player_state(&player_name).coins += coins * structures_count as u8;
    })
}

pub fn dynamic_point_effect(
    point_category: PointCategory,
    effect_directions: PlayerDirections<'static>,
    categories: Categories<'static>,
    points: Point,
) -> Effect {
    Box::new(move |game_state, player_name| {
        let affected_players: HashSet<PName> = game_state
            .neighbours
            .get_player_names_from_directions(&player_name, &effect_directions);
        let point_category = point_category.clone();
        let action = Box::new(
            move |gs: &GameState, points_by_categories: &mut PointsMap| {
                let structures_count =
                    gs.count_structures_for_players(&categories, &affected_players);
                points_by_categories.add(&point_category, points * structures_count as Point);
            },
        );
        game_state
            .get_mut_player_state(&player_name)
            .point_actions
            .push(action);
    })
}

pub fn dynamic_wonder_coin_effect(
    effect_directions: PlayerDirections<'static>,
    coins: Coin,
) -> Effect {
    Box::new(move |game_state, player_name| {
        let affected_players: HashSet<PName> = game_state
            .neighbours
            .get_player_names_from_directions(&player_name, &effect_directions);
        let wonder_stages_count = game_state.count_wonder_stages_for_players(&affected_players);
        game_state.get_mut_player_state(&player_name).coins += coins * wonder_stages_count as Coin;
    })
}

pub fn dynamic_wonder_point_effect(
    point_category: PointCategory,
    effect_directions: PlayerDirections<'static>,
    points: Point,
) -> Effect {
    Box::new(move |game_state, player_name| {
        let affected_players: HashSet<PName> = game_state
            .neighbours
            .get_player_names_from_directions(&player_name, &effect_directions);
        let point_category = point_category.clone();
        let action = Box::new(
            move |gs: &GameState, points_by_categories: &mut PointsMap| {
                let wonder_stages_count = gs.count_wonder_stages_for_players(&affected_players);
                points_by_categories.add(&point_category, points * wonder_stages_count as Point);
            },
        );
        game_state
            .get_mut_player_state(&player_name)
            .point_actions
            .push(action);
    })
}

pub fn dynamic_battle_lost_point_effect(
    point_category: PointCategory,
    effect_directions: PlayerDirections<'static>,
    points: Point,
) -> Effect {
    Box::new(move |game_state, player_name| {
        let affected_players: HashSet<PName> = game_state
            .neighbours
            .get_player_names_from_directions(&player_name, &effect_directions);
        let point_category = point_category.clone();
        let action = Box::new(
            move |gs: &GameState, points_by_categories: &mut PointsMap| {
                let battles_lost_count = gs.count_battles_lost_for_players(&affected_players);
                points_by_categories.add(&point_category, points * battles_lost_count as Point);
            },
        );
        game_state
            .get_mut_player_state(&player_name)
            .point_actions
            .push(action);
    })
}

pub fn play_last_card_effect() -> Effect {
    apply_player_effect(Box::new(move |player_state| {
        player_state.can_play_last_card = true;
    }))
}

pub fn construct_free_effect() -> Effect {
    Box::new(move |game_state: &mut GameState, player_name: String| {
        let current_age = game_state.current_age;
        game_state
            .get_mut_player_state(&player_name)
            .structure_builder
            .apply_construct_free_once_per_age(current_age);
    })
}

pub fn copy_guild_effect() -> Effect {
    apply_player_effect(Box::new(move |player_state| {
        player_state.can_copy_guild = true;
    }))
}

pub fn build_free_from_discarded_cards_effect() -> Effect {
    Box::new(|game_state, player_name| {
        game_state
            .events
            .push((player_name, EventType::ConstructFromDiscarded))
    })
}

fn apply_player_effect(player_state_mapper: Box<dyn Fn(&mut PlayerState) + Sync + Send>) -> Effect {
    Box::new(move |game_state: &mut GameState, player_name: String| {
        player_state_mapper(game_state.get_mut_player_state(&player_name));
    })
}
