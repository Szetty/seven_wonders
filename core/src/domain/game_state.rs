use super::effects::Effects;
use super::player::{
    Neighbours, PName, Player,
    PlayerDirection::{East, Own, West},
    PlayerDirections,
};
use super::point::PointCategory::{
    CivilianP, CommercialP, GuildP, MilitaryP, ScientificP, TreasuryP, WonderP,
};
use super::point::{Point, PointCategory, PointsMap};
use super::structure::Age::{I, II, III};
use super::structure::Category::{Civilian, Commercial, Guild, Military, Scientific, MG, RM};
use super::structure::{Age, Categories, Category, SName, Structure};
use super::structure_builder::StructureBuilder;
use super::supply::ResourceType::{Clay, Glass, Loom, Ore, Papyrus, Stone, Wood};
use super::supply::ScientificSymbol::{Compass, Gears, Tablet};
use super::supply::{
    calculate_military_points, calculate_treasury_points, BattleTokens, Coin, MilitarySymbolCount,
    ResourceCost as RCost, ResourceCost, ResourceCostOptions, ResourceCosts, ResourceType,
    ResourceTypes, ResourcesProduced, ScientificSymbols, ScientificSymbolsProduced, TradeValue,
};
use super::wonder::{Wonder, WonderSide, WonderStage};
use lazy_static::lazy_static;
use maplit::hashset;
use std::collections::{HashMap, HashSet};
use std::fmt;

#[derive(Default)]
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
        unimplemented!()
        // coins, trade actions, wonder effects
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
}

pub type Deck = (Cards, Cards, Cards);
pub type Cards = Vec<Card>;
#[derive(Debug, PartialEq)]
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
    pub point_actions: Actions<dyn Fn(&GameState, &mut PointsMap) + Sync>,
    pub trade_actions: Actions<dyn Fn(&PName, &ResourceType) -> TradeValue + Sync>,
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
        action: Action<dyn Fn(&PName, &ResourceType) -> TradeValue + Sync>,
    ) -> &Self {
        self.trade_actions.push(action);
        self
    }
    pub fn add_trade_action_move(
        mut self,
        action: Action<dyn Fn(&PName, &ResourceType) -> TradeValue + Sync>,
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

impl Default for PlayerState {
    fn default() -> Self {
        Self::new(Default::default(), &WONDERS[0].2)
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

type WonderStagesBuilt = u8;

type Actions<T> = Vec<Action<T>>;
type Action<T> = Box<T>;

pub type Effect = Box<dyn Fn(&mut GameState, PName) + Sync>;

pub type Events = Vec<Event>;
pub type Event = (PName, EventType);
pub enum EventType {
    ConstructFromDiscarded,
    PlayLastCard,
    CopyGuild,
}

use all_resources_effect as allre;
use all_scientific_effect as allse;
use any_resources_effect as anyre;
use any_scientific_effect as anyse;
use build_free_from_discarded_cards_effect as bffdce;
use coin_effect as ce;
use construct_free_effect as cfe;
use copy_guild_effect as cge;
use dynamic_battle_lost_point_effect as dblpe;
use dynamic_coin_effect as dce;
use dynamic_point_effect as dpe;
use dynamic_wonder_coin_effect as dwce;
use dynamic_wonder_point_effect as dwpe;
use military_effect as me;
use play_last_card_effect as plce;
use point_effect as pe;
use trade_effect as te;
lazy_static! {
    #[rustfmt::skip]
    pub static ref AGE_I_STRUCTURES: [Structure<'static, Effect>; 27] = [
        // Raw Materials (Brown)
        Structure("Lumber Yard", RM, I, vec![allre(&[Wood])],        &[], &[], (0, &[]), &[3, 4]),
        Structure("Stone Pit",   RM, I, vec![allre(&[Stone])],       &[], &[], (0, &[]), &[3, 5]),
        Structure("Clay Pool",   RM, I, vec![allre(&[Clay])],        &[], &[], (0, &[]), &[3, 5]),
        Structure("Ore Vein",    RM, I, vec![allre(&[Ore])],         &[], &[], (0, &[]), &[3, 4]),
        Structure("Tree Farm",   RM, I, vec![anyre(&[Wood, Clay])],  &[], &[], (1, &[]), &[6]),
        Structure("Excavation",  RM, I, vec![anyre(&[Stone, Clay])], &[], &[], (1, &[]), &[4]),
        Structure("Clay Pit",    RM, I, vec![anyre(&[Clay, Ore])],   &[], &[], (1, &[]), &[3]),
        Structure("Timber Yard", RM, I, vec![anyre(&[Stone, Wood])], &[], &[], (1, &[]), &[3]),
        Structure("Forest Cave", RM, I, vec![anyre(&[Wood, Ore])],   &[], &[], (1, &[]), &[5]),
        Structure("Mine",        RM, I, vec![anyre(&[Ore, Stone])],  &[], &[], (1, &[]), &[6]),
        // Manufactured Goods (Gray)
        Structure("Loom"      , MG, I, vec![allre(&[Loom])]   , &[], &[], (0, &[]), &[3, 6]),
        Structure("Glassworks", MG, I, vec![allre(&[Glass])]  , &[], &[], (0, &[]), &[3, 6]),
        Structure("Press"     , MG, I, vec![allre(&[Papyrus])], &[], &[], (0, &[]), &[3, 6]),
        // Civilian (Blue)
        Structure("Pawnshop", Civilian, I, vec![pe(CivilianP, 3)], &[], &[]          , (0, &[]),                &[4, 7]),
        Structure("Baths"   , Civilian, I, vec![allre(&[])], &[], &["Aqueduct"], (0, &[RCost(Stone, 1)]), &[3, 7]),
        Structure("Altar"   , Civilian, I, vec![pe(CivilianP, 2)], &[], &["Temple"]  , (0, &[]),                &[3, 5]),
        Structure("Theater" , Civilian, I, vec![pe(CivilianP, 2)], &[], &["Statue"]  , (0, &[]),                &[3, 6]),
        // Military (Red)
        Structure("Stockade"   , Military, I, vec![me(1)], &[], &[], (0, &[RCost(Wood, 1)]), &[3, 7]),
        Structure("Barracks"   , Military, I, vec![me(1)], &[], &[], (0, &[RCost(Ore, 1)]) , &[3, 5]),
        Structure("Guard tower", Military, I, vec![me(1)], &[], &[], (0, &[RCost(Clay, 1)]), &[3, 4]),
        // Scientific (Green)
        Structure("Apothecary" , Scientific, I, vec![allse(&[Compass])], &[], &["Stables", "Dispensary"]      , (0, &[RCost(Loom, 1)])   , &[3, 5]),
        Structure("Workshop"   , Scientific, I, vec![allse(&[Gears])]  , &[], &["Archery Range", "Laboratory"], (0, &[RCost(Glass, 1)])  , &[3, 7]),
        Structure("Scriptorium", Scientific, I, vec![allse(&[Tablet])] , &[], &["Courthouse", "Library"]      , (0, &[RCost(Papyrus, 1)]), &[3, 4]),
        // Commercial (Yellow)
        Structure("Tavern"           , Commercial, I, vec![ce(5)]                                     , &[], &[]             , (0, &[]), &[4, 5, 7]),
        Structure("East trading post", Commercial, I, vec![te(&[East], &[Wood, Stone, Ore, Clay])]    , &[], &["Forum"]      , (0, &[]), &[3, 7]),
        Structure("West trading post", Commercial, I, vec![te(&[West], &[Wood, Stone, Ore, Clay])]    , &[], &["Forum"]      , (0, &[]), &[3, 7]),
        Structure("Marketplace"      , Commercial, I, vec![te(&[East, West], &[Loom, Glass, Papyrus])], &[], &["Caravansery"], (0, &[]), &[3, 6]),
    ];
    #[rustfmt::skip]
    pub static ref AGE_II_STRUCTURES: [Structure<'static, Effect>; 23] = [
        // Raw Materials (Brown)
        Structure("Sawmill"  , RM, II, vec![allre(&[Wood, Wood])]  , &[], &[], (1, &[]), &[3, 4]),
        Structure("Quarry"   , RM, II, vec![allre(&[Stone, Stone])], &[], &[], (1, &[]), &[3, 4]),
        Structure("Brickyard", RM, II, vec![allre(&[Clay, Clay])]  , &[], &[], (1, &[]), &[3, 4]),
        Structure("Foundry"  , RM, II, vec![allre(&[Ore, Ore])]    , &[], &[], (1, &[]), &[3, 4]),
        // Manufactured Goods (Gray)
        Structure("Loom"      , MG, II, vec![allre(&[Loom])]   , &[], &[], (0, &[]), &[3, 5]),
        Structure("Glassworks", MG, II, vec![allre(&[Glass])]  , &[], &[], (0, &[]), &[3, 5]),
        Structure("Press"     , MG, II, vec![allre(&[Papyrus])], &[], &[], (0, &[]), &[3, 5]),
        // Civilian (Blue)
        Structure("Courthouse", Civilian, II, vec![pe(CivilianP, 4)], &["Scriptorium"], &["Senate"]  , (0, &[RCost(Clay, 2), RCost(Loom, 1)])                 , &[3, 5]),
        Structure("Aqueduct"  , Civilian, II, vec![pe(CivilianP, 5)], &["Baths"]      , &[]          , (0, &[RCost(Stone, 3)])                                , &[3, 7]),
        Structure("Temple"    , Civilian, II, vec![pe(CivilianP, 3)], &["Altar"]      , &["Pantheon"], (0, &[RCost(Wood, 1), RCost(Clay, 1), RCost(Glass, 1)]), &[3, 6]),
        Structure("Statue"    , Civilian, II, vec![pe(CivilianP, 4)], &["Theater"]    , &["Gardens"] , (0, &[RCost(Wood, 1), RCost(Ore, 2)])                  , &[3, 7]),
        // Military (Red)
        Structure("Walls"          , Military, II, vec![me(2)], &[]            , &["Fortifications"], (0, &[RCost(Stone, 3)])                              , &[3, 7]),
        Structure("Training Ground", Military, II, vec![me(2)], &[]            , &["Circus"]        , (0, &[RCost(Wood, 1), RCost(Ore, 2)])                , &[4, 6, 7]),
        Structure("Stables"        , Military, II, vec![me(2)], &["Apothecary"], &[]                , (0, &[RCost(Ore, 1), RCost(Clay, 1), RCost(Wood, 1)]), &[3, 5]),
        Structure("Archery Range"  , Military, II, vec![me(2)], &["Workshop"]  , &[]                , (0, &[RCost(Wood, 2), RCost(Ore, 1)])                , &[3, 6]),
        // Scientific (Green)
        Structure("Dispensary", Scientific, II, vec![allse(&[Compass])], &["Apothecary"] , &["Lodge", "Arena"]               , (0, &[RCost(Ore, 2), RCost(Glass, 1)])   , &[3, 4]),
        Structure("Laboratory", Scientific, II, vec![allse(&[Gears])]  , &["Workshop"]   , &["Siege Workshop", "Observatory"], (0, &[RCost(Clay, 2), RCost(Papyrus, 1)]), &[3, 5]),
        Structure("Library"   , Scientific, II, vec![allse(&[Tablet])] , &["Scriptorium"], &["Senate", "University"]         , (0, &[RCost(Stone, 2), RCost(Loom, 1)])  , &[3, 6]),
        Structure("School"    , Scientific, II, vec![allse(&[Tablet])] , &[]             , &["Academy", "Study"]             , (0, &[RCost(Wood, 1), RCost(Papyrus, 1)]), &[3, 7]),
        // Commercial (Yellow)
        Structure("Forum"      , Commercial, II, vec![anyre(&[Loom, Glass, Papyrus])]   , &["East trading post", "West trading post"], &["Haven"]     , (0, &[RCost(Clay, 2)]), &[3, 6, 7]),
        Structure("Caravansery", Commercial, II, vec![anyre(&[Wood, Stone, Ore, Clay])] , &["Marketplace"]                           , &["Lighthouse"], (0, &[RCost(Wood, 2)]), &[3, 5, 6]),
        Structure("Vineyard"   , Commercial, II, vec![dce(&[East, West, Own], &[RM], 1)], &[]                                        , &[]            , (0, &[])              , &[3, 6]),
        Structure("Bazar"      , Commercial, II, vec![dce(&[East, West, Own], &[MG], 2)], &[]                                        , &[]            , (0, &[])              , &[4, 7]),
    ];
    #[rustfmt::skip]
    pub static ref AGE_III_STRUCTURES: [Structure<'static, Effect>; 18] = [
        // Civilian (Blue)
        Structure("Pantheon" , Civilian, III, vec![pe(CivilianP, 7)], &["Temple"] , &[], (0, &[RCost(Clay, 2), RCost(Ore, 1), RCost(Papyrus, 1), RCost(Loom, 1), RCost(Glass, 1)])                                 , &[3, 6]),
        Structure("Gardens"  , Civilian, III, vec![pe(CivilianP, 5)], &["Statue"] , &[], (0, &[RCost(Clay, 2), RCost(Wood, 1)])                                                                                    , &[3, 4]),
        Structure("Town hall", Civilian, III, vec![pe(CivilianP, 6)], &[]         , &[], (0, &[RCost(Stone, 2), RCost(Ore, 1), RCost(Glass, 1)])                                                                   , &[3, 5, 6]),
        Structure("Palace"   , Civilian, III, vec![pe(CivilianP, 8)], &[]         , &[], (0, &[RCost(Wood, 1), RCost(Stone, 1), RCost(Ore, 1), RCost(Clay, 1), RCost(Loom, 1), RCost(Glass, 1), RCost(Papyrus, 1)]), &[3, 7]),
        Structure("Senate"   , Civilian, III, vec![pe(CivilianP, 6)], &["Library"], &[], (0, &[RCost(Wood, 2), RCost(Ore, 1), RCost(Stone, 1)])                                                                    , &[3, 5]),
        // Military (Red)
        Structure("Fortifications", Military, III, vec![me(3)], &["Walls"]          , &[], (0, &[RCost(Ore, 3), RCost(Stone, 1)])               , &[3, 7]),
        Structure("Circus"        , Military, III, vec![me(3)], &["Training Ground"], &[], (0, &[RCost(Stone, 3), RCost(Ore, 1)])               , &[4, 5, 6]),
        Structure("Arsenal"       , Military, III, vec![me(3)], &[]                 , &[], (0, &[RCost(Ore, 1), RCost(Wood, 2), RCost(Loom, 1)]), &[3, 4, 7]),
        Structure("Siege Workshop", Military, III, vec![me(3)], &["Laboratory"]     , &[], (0, &[RCost(Wood, 1), RCost(Clay, 3)])               , &[3, 5]),
        // Scientific (Green)
        Structure("Lodge"      , Scientific, III, vec![allse(&[Compass])], &["Dispensary"], &[], (0, &[RCost(Clay, 2), RCost(Loom, 1), RCost(Papyrus, 1)]) , &[3, 6]),
        Structure("Observatory", Scientific, III, vec![allse(&[Gears])]  , &["Laboratory"], &[], (0, &[RCost(Ore, 2), RCost(Glass, 1), RCost(Loom, 1)])    , &[3, 7]),
        Structure("University" , Scientific, III, vec![allse(&[Tablet])] , &["Library"]   , &[], (0, &[RCost(Wood, 2), RCost(Papyrus, 1), RCost(Glass, 1)]), &[3, 4]),
        Structure("Academy"    , Scientific, III, vec![allse(&[Compass])], &["School"]    , &[], (0, &[RCost(Stone, 3), RCost(Glass, 1)])                  , &[3, 7]),
        Structure("Study"      , Scientific, III, vec![allse(&[Gears])]  , &["School"]    , &[], (0, &[RCost(Wood, 1), RCost(Papyrus, 1), RCost(Loom, 1)]) , &[3, 5]),
        // Commercial (Yellow)
        Structure("Haven"              , Commercial, III, vec![dce(&[Own], &[RM], 1), dpe(CommercialP, &[Own], &[RM], 1)]                , &["Forum"]      , &[], (0, &[RCost(Ore, 1), RCost(Wood, 1), RCost(Loom, 1)]), &[3, 4]),
        Structure("Lighthouse"         , Commercial, III, vec![dce(&[Own], &[Commercial], 1), dpe(CommercialP, &[Own], &[Commercial], 1)], &["Caravansery"], &[], (0, &[RCost(Stone, 1), RCost(Glass, 1)])             , &[3, 6]),
        Structure("Chamber of commerce", Commercial, III, vec![dce(&[Own], &[MG], 2), dpe(CommercialP, &[Own], &[MG], 2)]                , &[]             , &[], (0, &[RCost(Clay, 2), RCost(Papyrus, 1)])            , &[4, 6]),
        Structure("Arena"              , Commercial, III, vec![dwce(&[Own], 3), dwpe(CommercialP, &[Own], 1)]                            , &["Dispensary"] , &[], (0, &[RCost(Stone, 2), RCost(Ore, 1)])               , &[3, 5, 7]),
    ];
    #[rustfmt::skip]
    pub static ref GUILD_STRUCTURES: [Structure<'static, Effect>; 10] = [
        Structure("Workers Guild"     , Guild, III, vec![dpe(GuildP, &[East, West], &[RM], 1)]        , &[], &[], (0, &[RCost(Ore, 2), RCost(Clay, 1), RCost(Stone, 1), RCost(Wood, 1)]), &[]),
        Structure("Craftsmens Guild"  , Guild, III, vec![dpe(GuildP, &[East, West], &[MG], 2)]        , &[], &[], (0, &[RCost(Ore, 2), RCost(Stone, 2)])                                , &[]),
        Structure("Traders Guild"     , Guild, III, vec![dpe(GuildP, &[East, West], &[Commercial], 1)], &[], &[], (0, &[RCost(Loom, 1), RCost(Papyrus, 1), RCost(Glass, 1)])            , &[]),
        Structure("Philosophers Guild", Guild, III, vec![dpe(GuildP, &[East, West], &[Scientific], 1)], &[], &[], (0, &[RCost(Clay, 3), RCost(Loom, 1), RCost(Papyrus, 1)])             , &[]),
        Structure("Spies Guild"       , Guild, III, vec![dpe(GuildP, &[East, West], &[Military], 1)]  , &[], &[], (0, &[RCost(Clay, 3), RCost(Glass, 1)])                               , &[]),
        Structure("Strategists Guild" , Guild, III, vec![dblpe(GuildP, &[East, West], 1)]             , &[], &[], (0, &[RCost(Ore, 2), RCost(Stone, 1), RCost(Loom, 1)])                , &[]),
        Structure("Shipowners Guild"  , Guild, III, vec![dpe(GuildP, &[Own], &[RM, MG, Guild], 1)]    , &[], &[], (0, &[RCost(Wood, 3), RCost(Papyrus, 1), RCost(Glass, 1)])            , &[]),
        Structure("Scientists Guild"  , Guild, III, vec![anyse(&[Compass, Gears, Tablet])]             , &[], &[], (0, &[RCost(Wood, 2), RCost(Ore, 2), RCost(Papyrus, 1)])              , &[]),
        Structure("Magistrates Guild" , Guild, III, vec![dpe(GuildP, &[East, West], &[Civilian], 2)]  , &[], &[], (0, &[RCost(Wood, 3), RCost(Stone, 1), RCost(Loom, 1)])               , &[]),
        Structure("Builders Guild"    , Guild, III, vec![dwpe(GuildP, &[East, West, Own], 1)]         , &[], &[], (0, &[RCost(Stone, 2), RCost(Clay, 2), RCost(Glass, 1)])              , &[]),
    ];
    pub static ref STRUCTURES_BY_NAME: HashMap<String, &'static Structure<'static, Effect>> = AGE_I_STRUCTURES.iter().chain(AGE_II_STRUCTURES.iter()).chain(AGE_III_STRUCTURES.iter()).chain(GUILD_STRUCTURES.iter()).map(|s| (s.0.to_string(), s)).collect();
    #[rustfmt::skip]
    pub static ref WONDERS: [Wonder<'static, Effect>; 7] = [
        Wonder(
            "Rhódos",
            WonderSide(
                "Rhódos - A",
                vec![allre(&[Ore])],
                vec![
                    WonderStage(&[RCost(Wood, 2)], vec![pe(WonderP, 3)]),
                    WonderStage(&[RCost(Clay, 3)], vec![me(2)]),
                    WonderStage(&[RCost(Ore, 4)], vec![pe(WonderP, 7)])
                ]
            ),
            WonderSide(
                "Rhódos - B",
                vec![allre(&[Ore])],
                vec![
                    WonderStage(&[RCost(Stone, 3)], vec![me(1), pe(WonderP, 3), ce(3)]),
                    WonderStage(&[RCost(Ore, 4)], vec![me(1), pe(WonderP, 4), ce(4)]),
                ]
            )
        ),
        Wonder(
            "Alexandria",
            WonderSide(
                "Alexandria - A",
                vec![allre(&[Glass])],
                vec![
                    WonderStage(&[RCost(Stone, 2)], vec![pe(WonderP, 3)]),
                    WonderStage(&[RCost(Ore, 2)],   vec![anyre(&[Wood, Stone, Ore, Clay])]),
                    WonderStage(&[RCost(Glass, 2)], vec![pe(WonderP, 7)]),
                ]
            ),
            WonderSide(
                "Alexandria - B",
                vec![allre(&[Glass])],
                vec![
                    WonderStage(&[RCost(Clay, 2)], vec![anyre(&[Wood, Stone, Ore, Clay])]),
                    WonderStage(&[RCost(Wood, 2)], vec![anyre(&[Loom, Glass, Papyrus])]),
                    WonderStage(&[RCost(Stone, 3)], vec![pe(WonderP, 7)]),
                ]
            )
        ),
        Wonder(
            "Éphesos",
            WonderSide(
                "Éphesos - A",
                vec![allre(&[Papyrus])],
                vec![
                    WonderStage(&[RCost(Stone, 2)], vec![pe(WonderP, 3)]),
                    WonderStage(&[RCost(Wood, 2)], vec![ce(9)]),
                    WonderStage(&[RCost(Papyrus, 2)], vec![pe(WonderP, 7)]),
                ]
            ),
            WonderSide(
                "Éphesos - B",
                vec![allre(&[Papyrus])],
                vec![
                    WonderStage(&[RCost(Stone, 2)], vec![pe(WonderP, 2), ce(4)]),
                    WonderStage(&[RCost(Wood, 2)], vec![pe(WonderP, 3), ce(4)]),
                    WonderStage(&[RCost(Papyrus, 1), RCost(Glass, 1), RCost(Loom, 1)], vec![pe(WonderP, 5), ce(4)]),
                ]
            )
        ),
        Wonder(
            "Babylon",
            WonderSide(
                "Babylon - A",
                vec![allre(&[Clay])],
                vec![
                    WonderStage(&[RCost(Clay, 2)], vec![pe(WonderP, 3)]),
                    WonderStage(&[RCost(Wood, 3)], vec![anyse(&[Compass, Gears, Tablet])]),
                    WonderStage(&[RCost(Clay, 4)], vec![pe(WonderP, 7)]),
                ]
            ),
            WonderSide(
                "Babylon - B",
                vec![allre(&[Clay])],
                vec![
                    WonderStage(&[RCost(Loom, 1), RCost(Clay, 1)], vec![pe(WonderP, 3)]),
                    WonderStage(&[RCost(Glass, 1), RCost(Wood, 2)], vec![plce()]),
                    WonderStage(&[RCost(Papyrus, 1), RCost(Clay, 3)], vec![anyse(&[Compass, Gears, Tablet])]),
                ]
            )
        ),
        Wonder(
            "Olympía",
            WonderSide(
                "Olympía - A",
                vec![allre(&[Wood])],
                vec![
                    WonderStage(&[RCost(Wood, 2)], vec![pe(WonderP, 3)]),
                    WonderStage(&[RCost(Stone, 2)], vec![cfe()]),
                    WonderStage(&[RCost(Ore, 2)], vec![pe(WonderP, 7)]),
                ]
            ),
            WonderSide(
                "Olympía - B",
                vec![allre(&[Wood])],
                vec![
                    WonderStage(&[RCost(Wood, 2)], vec![te(&[East, West], &[Wood, Stone, Ore, Clay])]),
                    WonderStage(&[RCost(Stone, 2)], vec![pe(WonderP, 5)]),
                    WonderStage(&[RCost(Loom, 1), RCost(Ore, 2)], vec![cge()]),
                ]
            )
        ),
        Wonder(
            "Halikarnassós",
            WonderSide(
                "Halikarnassós - A",
                vec![allre(&[Loom])],
                vec![
                    WonderStage(&[RCost(Clay, 2)], vec![pe(WonderP, 3)]),
                    WonderStage(&[RCost(Ore, 3)], vec![bffdce()]),
                    WonderStage(&[RCost(Loom, 2)], vec![pe(WonderP, 7)]),
                ]
            ),
            WonderSide(
                "Halikarnassós - B",
                vec![allre(&[Loom])],
                vec![
                    WonderStage(&[RCost(Ore, 2)], vec![pe(WonderP, 2), bffdce()]),
                    WonderStage(&[RCost(Clay, 3)], vec![pe(WonderP, 1), bffdce()]),
                    WonderStage(&[RCost(Papyrus, 1), RCost(Glass, 1), RCost(Loom, 1)], vec![bffdce()]),
                ]
            )
        ),
        Wonder(
            "Gizah",
            WonderSide(
                "Gizah - A",
                vec![allre(&[Stone])],
                vec![
                    WonderStage(&[RCost(Stone, 2)], vec![pe(WonderP, 3)]),
                    WonderStage(&[RCost(Wood, 3)], vec![pe(WonderP, 5)]),
                    WonderStage(&[RCost(Stone, 4)], vec![pe(WonderP, 7)]),
                ]
            ),
            WonderSide(
                "Gizah - B",
                vec![allre(&[Stone])],
                vec![
                    WonderStage(&[RCost(Wood, 2)], vec![pe(WonderP, 3)]),
                    WonderStage(&[RCost(Stone, 3)], vec![pe(WonderP, 5)]),
                    WonderStage(&[RCost(Clay, 3)], vec![pe(WonderP, 5)]),
                    WonderStage(&[RCost(Papyrus, 1), RCost(Stone, 4)], vec![pe(WonderP, 7)]),
                ]
            )
        )
    ];
    pub static ref WONDERS_BY_NAME: HashMap<String, &'static Wonder<'static, Effect>> = WONDERS.iter().map(|s| (s.0.to_string(), s)).collect();
}

fn all_resources_effect(resource_types: ResourceTypes<'static>) -> Effect {
    apply_player_effect(Box::new(move |player_state| {
        player_state
            .resources_produced
            .add_all_resources(resource_types)
    }))
}

fn any_resources_effect(resource_types: ResourceTypes<'static>) -> Effect {
    apply_player_effect(Box::new(move |player_state| {
        player_state
            .resources_produced
            .add_any_resources(resource_types)
    }))
}

fn point_effect(point_category: PointCategory, points: Point) -> Effect {
    let action = Box::new(move |_: &_, points_by_categories: &mut PointsMap| {
        points_by_categories.add(&point_category, points);
    });
    apply_player_effect(Box::new(move |player_state| {
        player_state.point_actions.push(action.clone());
    }))
}

fn military_effect(military_symbols: MilitarySymbolCount) -> Effect {
    apply_player_effect(Box::new(move |player_state| {
        player_state.military_symbols += military_symbols;
    }))
}

fn all_scientific_effect(scientific_symbols: ScientificSymbols<'static>) -> Effect {
    apply_player_effect(Box::new(move |player_state| {
        player_state
            .scientific_symbols_produced
            .add_all_symbols(scientific_symbols)
    }))
}

fn any_scientific_effect(scientific_symbols: ScientificSymbols<'static>) -> Effect {
    apply_player_effect(Box::new(move |player_state| {
        player_state
            .scientific_symbols_produced
            .add_any_symbols(scientific_symbols)
    }))
}

fn coin_effect(coins: Coin) -> Effect {
    apply_player_effect(Box::new(move |player_state| {
        player_state.coins += coins;
    }))
}

fn trade_effect(
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

fn dynamic_coin_effect(
    effect_directions: PlayerDirections<'static>,
    categories: Categories<'static>,
    coins: Coin,
) -> Effect {
    Box::new(move |game_state, player_name| {
        let affected_players: HashSet<PName> = game_state
            .neighbours
            .get_player_names_from_directions(&player_name, &effect_directions);
        let structures_count =
            count_structures_for_players(&game_state, &categories, &affected_players);
        game_state.get_mut_player_state(&player_name).coins += coins * structures_count as u8;
    })
}

fn dynamic_point_effect(
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
                    count_structures_for_players(gs, &categories, &affected_players);
                points_by_categories.add(&point_category, points * structures_count as Point);
            },
        );
        game_state
            .get_mut_player_state(&player_name)
            .point_actions
            .push(action);
    })
}

fn dynamic_wonder_coin_effect(effect_directions: PlayerDirections<'static>, coins: Coin) -> Effect {
    Box::new(move |game_state, player_name| {
        let affected_players: HashSet<PName> = game_state
            .neighbours
            .get_player_names_from_directions(&player_name, &effect_directions);
        let wonder_stages_count = count_wonder_stages_for_players(&game_state, &affected_players);
        game_state.get_mut_player_state(&player_name).coins += coins * wonder_stages_count as Coin;
    })
}

fn dynamic_wonder_point_effect(
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
                let wonder_stages_count = count_wonder_stages_for_players(gs, &affected_players);
                points_by_categories.add(&point_category, points * wonder_stages_count as Point);
            },
        );
        game_state
            .get_mut_player_state(&player_name)
            .point_actions
            .push(action);
    })
}

fn dynamic_battle_lost_point_effect(
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
                let battles_lost_count = count_battles_lost_for_players(gs, &affected_players);
                points_by_categories.add(&point_category, points * battles_lost_count as Point);
            },
        );
        game_state
            .get_mut_player_state(&player_name)
            .point_actions
            .push(action);
    })
}

fn play_last_card_effect() -> Effect {
    apply_player_effect(Box::new(move |player_state| {
        player_state.can_play_last_card = true;
    }))
}

fn construct_free_effect() -> Effect {
    Box::new(move |game_state: &mut GameState, player_name: String| {
        let current_age = game_state.current_age;
        game_state
            .get_mut_player_state(&player_name)
            .structure_builder
            .apply_construct_free_once_per_age(current_age);
    })
}

fn copy_guild_effect() -> Effect {
    apply_player_effect(Box::new(move |player_state| {
        player_state.can_copy_guild = true;
    }))
}

fn build_free_from_discarded_cards_effect() -> Effect {
    Box::new(|game_state, player_name| {
        game_state
            .events
            .push((player_name, EventType::ConstructFromDiscarded))
    })
}

fn apply_player_effect(player_state_mapper: Box<dyn Fn(&mut PlayerState) + Sync>) -> Effect {
    Box::new(move |game_state: &mut GameState, player_name: String| {
        player_state_mapper(game_state.get_mut_player_state(&player_name));
    })
}

fn count_structures_for_players(
    game_state: &GameState,
    categories: &Categories,
    players: &HashSet<PName>,
) -> usize {
    players
        .iter()
        .map(|player_name| {
            game_state
                .get_player_state(player_name)
                .structure_builder
                .count_structures_for_categories(categories)
        })
        .sum()
}

fn count_wonder_stages_for_players(
    game_state: &GameState,
    players: &HashSet<PName>,
) -> WonderStagesBuilt {
    players
        .iter()
        .map(|player_name| {
            let player_state = game_state.get_player_state(player_name);
            player_state.wonder_stages_built
        })
        .sum()
}

fn count_battles_lost_for_players(game_state: &GameState, players: &HashSet<PName>) -> usize {
    players
        .iter()
        .map(|player_name| {
            let player_state = game_state.get_player_state(player_name);
            player_state
                .battle_tokens
                .iter()
                .filter(|bt| **bt < 0)
                .count()
        })
        .sum()
}
