use super::helpers::default_game_state;
use crate::domain::{
    Age, Card,
    Category::{Commercial, Guild, Military, Scientific, MG, RM},
    GameState, Neighbours, Player, PlayerDecision, PlayerState,
    PointCategory::*,
    ResourceCost, ResourceCostOptions, ResourceCosts, ResourceCount, ResourceType,
    ResourceType::*,
    ResourceTypes, ResourcesProduced,
    ScientificSymbol::*,
    ScientificSymbolsProduced, StructureBuilder, STRUCTURES_BY_NAME, WONDERS_BY_NAME,
};
use maplit::{hashmap, hashset};
use std::collections::HashMap;

#[test]
fn test_card_dismissed() {
    let mut game_state = default_game_state();
    let tavern_structure = STRUCTURES_BY_NAME.get(&"Tavern".to_string()).unwrap();
    let tavern_card = Card(tavern_structure);
    game_state
        .apply_player_decisions(hashmap! {"a".to_string() => PlayerDecision::Discard(tavern_card)});
    let player_state = game_state.get_player_state(&"a".to_string());
    assert_eq!(player_state.coins, 3);
    assert_eq!(
        player_state
            .structure_builder
            .built_structures
            .get(&Commercial),
        None
    );
    assert_eq!(game_state.cards_discarded, vec![Card(tavern_structure)]);
}

#[test]
fn test_tavern() {
    let mut game_state = default_game_state();
    let tavern_structure = STRUCTURES_BY_NAME.get(&"Tavern".to_string()).unwrap();
    let tavern_card = Card(tavern_structure);
    game_state.apply_player_decisions(
        hashmap! {"a".to_string() => PlayerDecision::BuildStructure(tavern_card)},
    );
    let player_state = game_state.get_player_state(&"a".to_string());
    assert_eq!(player_state.coins, 5);
    assert_eq!(
        *player_state
            .structure_builder
            .built_structures
            .get(&Commercial)
            .unwrap(),
        hashset![tavern_structure.name()]
    );
}

#[test]
fn test_bazar() {
    let mut game_state = GameState {
        player_states: hashmap! {
            "a".to_string() => PlayerState {
                player: Player("a".to_string()),
                structure_builder: StructureBuilder {
                    built_structures: hashmap!{
                        MG => hashset!{"Loom", "Glassworks"}
                    },
                    ..Default::default()
                },
                ..Default::default()
            },
            "b".to_string() => PlayerState {
                player: Player("b".to_string()),
                structure_builder: StructureBuilder{
                    built_structures: hashmap!{
                        MG => hashset!{"Loom"}
                    },
                    ..Default::default()
                },
                ..Default::default()
            },
            "c".to_string() => PlayerState {
                player: Player("c".to_string()),
                structure_builder: StructureBuilder{
                    built_structures: hashmap!{
                        MG => hashset!{"Press", "Glassworks"}
                    },
                    ..Default::default()
                },
                ..Default::default()
            },
        },
        ..default_game_state()
    };
    let bazar_structure = STRUCTURES_BY_NAME.get(&"Bazar".to_string()).unwrap();
    let bazar_card = Card(bazar_structure);
    game_state.apply_player_decisions(
        hashmap! {"a".to_string() => PlayerDecision::BuildStructure(bazar_card)},
    );
    let player_state = game_state.get_player_state(&"a".to_string());
    assert_eq!(player_state.coins, 10);
    assert_eq!(
        *player_state
            .structure_builder
            .built_structures
            .get(&Commercial)
            .unwrap(),
        hashset![bazar_structure.name()]
    );
}

#[test]
fn test_guilds() {
    let mut game_state = GameState {
        player_states: hashmap! {
            "a".to_string() => PlayerState {
                player: Player("a".to_string()),
                wonder_stages_built: 3,
                battle_tokens: vec![-1, -1, -1, -1, -1, -1],
                structure_builder: StructureBuilder{
                    built_structures: hashmap!{
                        MG => hashset!{"Loom", "Glassworks"},
                        Commercial => hashset!{"Haven"},
                    },
                    ..Default::default()
                },
                ..Default::default()
            },
            "b".to_string() => PlayerState {
                player: Player("b".to_string()),
                wonder_stages_built: 3,
                battle_tokens: vec![1, 3, 3, 5, 5],
                structure_builder: StructureBuilder{
                    built_structures: hashmap!{
                        RM => hashset!{"Stone Pit", "Tree Farm"},
                        MG => hashset!{"Loom"},
                        Scientific => hashset!{"Apothecary", "Study"},
                        Military => hashset!{"Stockade", "Walls", "Circus", "Arsenal"}
                    },
                    ..Default::default()
                },
                scientific_symbols_produced: ScientificSymbolsProduced{
                    compass: 1,
                    gears: 1,
                    ..Default::default()
                },
                ..Default::default()
            },
            "c".to_string() => PlayerState {
                player: Player("c".to_string()),
                wonder_stages_built: 4,
                battle_tokens: vec![1, -1, -1, -1, -1],
                structure_builder: StructureBuilder{
                    built_structures: hashmap!{
                        RM => hashset!{"Clay pit", "Foundry", "Quarry", "Sawmill"},
                        MG => hashset!{"Press", "Glassworks"},

                        Commercial => hashset!{"Bazar", "Forum"},
                        Military => hashset!{"Barracks"},

                    },
                    ..Default::default()
                },
                ..Default::default()
            },
        },
        ..default_game_state()
    };

    let w_guild = STRUCTURES_BY_NAME
        .get(&"Workers Guild".to_string())
        .unwrap();
    let c_guild = STRUCTURES_BY_NAME
        .get(&"Craftsmens Guild".to_string())
        .unwrap();
    let t_guild = STRUCTURES_BY_NAME
        .get(&"Traders Guild".to_string())
        .unwrap();
    let p_guild = STRUCTURES_BY_NAME
        .get(&"Philosophers Guild".to_string())
        .unwrap();
    let sp_guild = STRUCTURES_BY_NAME.get(&"Spies Guild".to_string()).unwrap();
    let st_guild = STRUCTURES_BY_NAME
        .get(&"Strategists Guild".to_string())
        .unwrap();
    let sh_guild = STRUCTURES_BY_NAME
        .get(&"Shipowners Guild".to_string())
        .unwrap();
    let sc_guild = STRUCTURES_BY_NAME
        .get(&"Scientists Guild".to_string())
        .unwrap();
    let m_guild = STRUCTURES_BY_NAME
        .get(&"Magistrates Guild".to_string())
        .unwrap();
    let b_guild = STRUCTURES_BY_NAME
        .get(&"Builders Guild".to_string())
        .unwrap();

    game_state.apply_player_decisions(hashmap! {
        "a".to_string() => PlayerDecision::BuildStructure(Card(w_guild)),
        "b".to_string() => PlayerDecision::BuildStructure(Card(c_guild)),
        "c".to_string() => PlayerDecision::BuildStructure(Card(p_guild))
    });
    let a_player_state = game_state.get_player_state(&"a".to_string());
    assert_eq!(
        *a_player_state
            .structure_builder
            .built_structures
            .get(&Guild)
            .unwrap(),
        hashset![w_guild.name()]
    );
    assert_eq!(a_player_state.point_actions.len(), 1);
    let b_player_state = game_state.get_player_state(&"b".to_string());
    assert_eq!(
        *b_player_state
            .structure_builder
            .built_structures
            .get(&Guild)
            .unwrap(),
        hashset![c_guild.name()]
    );
    assert_eq!(b_player_state.point_actions.len(), 1);
    let c_player_state = game_state.get_player_state(&"c".to_string());
    assert_eq!(
        *c_player_state
            .structure_builder
            .built_structures
            .get(&Guild)
            .unwrap(),
        hashset![p_guild.name()]
    );
    assert_eq!(c_player_state.point_actions.len(), 1);
    assert_eq!(
        game_state.calculate_points(),
        hashmap! {
            "a".to_string() => hashmap!{
                GuildP => 6,
                MilitaryP => -6,
                TreasuryP => 0,
                WonderP => 0,
                CivilianP => 0,
                ScientificP => 0,
                CommercialP => 0,
            },
            "b".to_string() => hashmap!{
                GuildP => 8,
                MilitaryP => 17,
                TreasuryP => 0,
                WonderP => 0,
                CivilianP => 0,
                ScientificP => 2,
                CommercialP => 0,
            },
            "c".to_string() => hashmap!{
                GuildP => 2,
                MilitaryP => -3,
                TreasuryP => 0,
                WonderP => 0,
                CivilianP => 0,
                ScientificP => 0,
                CommercialP => 0,
            },
        }
    );

    game_state.apply_player_decisions(hashmap! {
        "a".to_string() => PlayerDecision::BuildStructure(Card(sp_guild)),
        "b".to_string() => PlayerDecision::BuildStructure(Card(t_guild)),
        "c".to_string() => PlayerDecision::BuildStructure(Card(b_guild))
    });
    let a_player_state = game_state.get_player_state(&"a".to_string());
    assert_eq!(
        *a_player_state
            .structure_builder
            .built_structures
            .get(&Guild)
            .unwrap(),
        hashset![w_guild.name(), sp_guild.name()]
    );
    assert_eq!(a_player_state.point_actions.len(), 2);
    let b_player_state = game_state.get_player_state(&"b".to_string());
    assert_eq!(
        *b_player_state
            .structure_builder
            .built_structures
            .get(&Guild)
            .unwrap(),
        hashset![c_guild.name(), t_guild.name()]
    );
    assert_eq!(b_player_state.point_actions.len(), 2);
    let c_player_state = game_state.get_player_state(&"c".to_string());
    assert_eq!(
        *c_player_state
            .structure_builder
            .built_structures
            .get(&Guild)
            .unwrap(),
        hashset![p_guild.name(), b_guild.name()]
    );
    assert_eq!(c_player_state.point_actions.len(), 2);
    assert_eq!(
        game_state.calculate_points(),
        hashmap! {
            "a".to_string() => hashmap!{
                GuildP => 11, // 6 from Workers, 5 from Spies
                MilitaryP => -6,
                TreasuryP => 0,
                WonderP => 0,
                CivilianP => 0,
                ScientificP => 0,
                CommercialP => 0,
            },
            "b".to_string() => hashmap!{
                GuildP => 11, // 8 from Craftsmens, 3 from Traders
                MilitaryP => 17,
                TreasuryP => 0,
                WonderP => 0,
                CivilianP => 0,
                ScientificP => 2,
                CommercialP => 0,
            },
            "c".to_string() => hashmap!{
                GuildP => 12, // 2 from Philosophers, 10 from Builders
                MilitaryP => -3,
                TreasuryP => 0,
                WonderP => 0,
                CivilianP => 0,
                ScientificP => 0,
                CommercialP => 0,
            },
        }
    );

    game_state.apply_player_decisions(hashmap! {
        "a".to_string() => PlayerDecision::BuildStructure(Card(m_guild)),
        "b".to_string() => PlayerDecision::BuildStructure(Card(sc_guild))
    });
    let a_player_state = game_state.get_player_state(&"a".to_string());
    assert_eq!(
        *a_player_state
            .structure_builder
            .built_structures
            .get(&Guild)
            .unwrap(),
        hashset![w_guild.name(), sp_guild.name(), m_guild.name()]
    );
    assert_eq!(a_player_state.point_actions.len(), 3);
    let b_player_state = game_state.get_player_state(&"b".to_string());
    assert_eq!(
        *b_player_state
            .structure_builder
            .built_structures
            .get(&Guild)
            .unwrap(),
        hashset![c_guild.name(), t_guild.name(), sc_guild.name()]
    );
    assert_eq!(b_player_state.point_actions.len(), 2);
    assert_eq!(
        b_player_state.scientific_symbols_produced.any_symbols.len(),
        1
    );
    let c_player_state = game_state.get_player_state(&"c".to_string());
    assert_eq!(
        *c_player_state
            .structure_builder
            .built_structures
            .get(&Guild)
            .unwrap(),
        hashset![p_guild.name(), b_guild.name()]
    );
    assert_eq!(c_player_state.point_actions.len(), 2);
    assert_eq!(
        game_state.calculate_points(),
        hashmap! {
            "a".to_string() => hashmap!{
                GuildP => 11, // 6 from Workers, 5 from Spies, 0 from Magistrates
                MilitaryP => -6,
                TreasuryP => 0,
                WonderP => 0,
                CivilianP => 0,
                ScientificP => 0,
                CommercialP => 0,
            },
            "b".to_string() => hashmap!{
                GuildP => 11, // 8 from Craftsmens, 3 from Traders
                MilitaryP => 17,
                TreasuryP => 0,
                WonderP => 0,
                CivilianP => 0,
                ScientificP => 10,
                CommercialP => 0,
            },
            "c".to_string() => hashmap!{
                GuildP => 12, // 2 from Philosophers, 10 from Builders
                MilitaryP => -3,
                TreasuryP => 0,
                WonderP => 0,
                CivilianP => 0,
                ScientificP => 0,
                CommercialP => 0,
            },
        }
    );

    game_state.apply_player_decisions(hashmap! {
        "b".to_string() => PlayerDecision::BuildStructure(Card(st_guild)),
        "c".to_string() => PlayerDecision::BuildStructure(Card(sh_guild))
    });
    let a_player_state = game_state.get_player_state(&"a".to_string());
    assert_eq!(
        *a_player_state
            .structure_builder
            .built_structures
            .get(&Guild)
            .unwrap(),
        hashset![w_guild.name(), sp_guild.name(), m_guild.name()]
    );
    assert_eq!(a_player_state.point_actions.len(), 3);
    let b_player_state = game_state.get_player_state(&"b".to_string());
    assert_eq!(
        *b_player_state
            .structure_builder
            .built_structures
            .get(&Guild)
            .unwrap(),
        hashset![
            c_guild.name(),
            t_guild.name(),
            sc_guild.name(),
            st_guild.name()
        ]
    );
    assert_eq!(b_player_state.point_actions.len(), 3);
    let c_player_state = game_state.get_player_state(&"c".to_string());
    assert_eq!(
        *c_player_state
            .structure_builder
            .built_structures
            .get(&Guild)
            .unwrap(),
        hashset![p_guild.name(), b_guild.name(), sh_guild.name()]
    );
    assert_eq!(c_player_state.point_actions.len(), 3);
    assert_eq!(
        game_state.calculate_points(),
        hashmap! {
            "a".to_string() => hashmap!{
                GuildP => 11, // 6 from Workers, 5 from Spies
                MilitaryP => -6,
                TreasuryP => 0,
                WonderP => 0,
                CivilianP => 0,
                ScientificP => 0,
                CommercialP => 0,
            },
            "b".to_string() => hashmap!{
                GuildP => 21, // 8 from Craftsmens, 3 from Traders, 10 from Strategists
                MilitaryP => 17,
                TreasuryP => 0,
                WonderP => 0,
                CivilianP => 0,
                ScientificP => 10,
                CommercialP => 0,
            },
            "c".to_string() => hashmap!{
                GuildP => 21, // 2 from Philosophers, 9 from Shipowners, 10 from Builders
                MilitaryP => -3,
                TreasuryP => 0,
                WonderP => 0,
                CivilianP => 0,
                ScientificP => 0,
                CommercialP => 0,
            },
        }
    );
}

#[test]
fn test_construct_free_once_per_age() {
    let wonder = WONDERS_BY_NAME.get(&"Olympía".to_string()).unwrap();
    let player_names = vec!["a".to_string(), "b".to_string(), "c".to_string()];
    let mut game_state = GameState {
        neighbours: Neighbours::new(player_names),
        current_age: Age::I,
        player_states: hashmap! {
            "a".to_string() => PlayerState {
                player: Player("a".to_string()),
                wonder: wonder.get_wonder_side_a(),
                ..Default::default()
            },
            "b".to_string() => PlayerState {
                player: Player("b".to_string()),
                ..Default::default()
            },
            "c".to_string() => PlayerState {
                player: Player("c".to_string()),
                ..Default::default()
            },
        },
        ..default_game_state()
    };

    let tavern_structure = STRUCTURES_BY_NAME.get(&"Tavern".to_string()).unwrap();
    let palace_structure = STRUCTURES_BY_NAME.get(&"Palace".to_string()).unwrap();

    game_state.apply_player_decisions(
        hashmap! {"a".to_string() => PlayerDecision::BuildNextWonderStage(Card(tavern_structure))},
    );
    let player_state = game_state.get_player_state(&"a".to_string());
    assert_eq!(player_state.point_actions.len(), 1);
    assert_eq!(
        player_state.calculate_points(&game_state),
        hashmap! {
            MilitaryP => 0,
            TreasuryP => 0,
            WonderP => 3,
            CivilianP => 0,
            ScientificP => 0,
            CommercialP => 0,
            GuildP => 0
        }
    );
    assert!(!player_state
        .structure_builder
        .already_built(tavern_structure));
    assert!(!player_state
        .structure_builder
        .can_build_structure_for_free(palace_structure, Age::I));

    game_state.apply_player_decisions(
        hashmap! {"a".to_string() => PlayerDecision::BuildNextWonderStage(Card(tavern_structure))},
    );
    let player_state = game_state.get_player_state(&"a".to_string());
    assert_eq!(player_state.point_actions.len(), 1);
    assert!(!player_state
        .structure_builder
        .already_built(tavern_structure));
    assert!(player_state
        .structure_builder
        .can_build_structure_for_free(palace_structure, Age::I));

    game_state.apply_player_decisions(hashmap! {"a".to_string() => PlayerDecision::ConstructForFreeOncePerAge(Card(palace_structure))});
    let player_state = game_state.get_player_state(&"a".to_string());
    assert_eq!(player_state.point_actions.len(), 2);
    assert!(player_state
        .structure_builder
        .already_built(palace_structure));
    assert!(!player_state
        .structure_builder
        .can_build_structure_for_free(palace_structure, Age::I));
    assert!(player_state
        .structure_builder
        .can_build_structure_for_free(palace_structure, Age::II));
}

#[test]
fn test_trade_raw_materials_from_both_neighbours() {
    let wonder = WONDERS_BY_NAME.get(&"Olympía".to_string()).unwrap();
    let player_names = vec![
        "a".to_string(),
        "b".to_string(),
        "c".to_string(),
        "d".to_string(),
    ];
    let mut game_state = GameState {
        neighbours: Neighbours::new(player_names),
        player_states: hashmap! {
            "a".to_string() => PlayerState {
                player: Player("a".to_string()),
                wonder: wonder.get_wonder_side_b(),
                ..Default::default()
            },
            "b".to_string() => PlayerState {
                player: Player("b".to_string()),
                ..Default::default()
            },
            "c".to_string() => PlayerState {
                player: Player("c".to_string()),
                ..Default::default()
            },
            "d".to_string() => PlayerState {
                player: Player("d".to_string()),
                ..Default::default()
            },
        },
        ..default_game_state()
    };

    let tavern_structure = STRUCTURES_BY_NAME.get(&"Tavern".to_string()).unwrap();

    game_state.apply_player_decisions(
        hashmap! {"a".to_string() => PlayerDecision::BuildNextWonderStage(Card(tavern_structure))},
    );
    let player_state = game_state.get_player_state(&"a".to_string());

    assert_eq!(player_state.apply_trading(&"b".to_string(), &Wood), 1);
    assert_eq!(player_state.apply_trading(&"b".to_string(), &Stone), 1);
    assert_eq!(player_state.apply_trading(&"b".to_string(), &Ore), 1);
    assert_eq!(player_state.apply_trading(&"b".to_string(), &Clay), 1);
    assert_eq!(player_state.apply_trading(&"b".to_string(), &Glass), 2);
    assert_eq!(player_state.apply_trading(&"b".to_string(), &Loom), 2);
    assert_eq!(player_state.apply_trading(&"b".to_string(), &Papyrus), 2);

    assert_eq!(player_state.apply_trading(&"c".to_string(), &Wood), 2);
    assert_eq!(player_state.apply_trading(&"c".to_string(), &Stone), 2);
    assert_eq!(player_state.apply_trading(&"c".to_string(), &Ore), 2);
    assert_eq!(player_state.apply_trading(&"c".to_string(), &Clay), 2);
    assert_eq!(player_state.apply_trading(&"c".to_string(), &Glass), 2);
    assert_eq!(player_state.apply_trading(&"c".to_string(), &Loom), 2);
    assert_eq!(player_state.apply_trading(&"c".to_string(), &Papyrus), 2);

    assert_eq!(player_state.apply_trading(&"d".to_string(), &Wood), 1);
    assert_eq!(player_state.apply_trading(&"d".to_string(), &Stone), 1);
    assert_eq!(player_state.apply_trading(&"d".to_string(), &Ore), 1);
    assert_eq!(player_state.apply_trading(&"d".to_string(), &Clay), 1);
    assert_eq!(player_state.apply_trading(&"d".to_string(), &Glass), 2);
    assert_eq!(player_state.apply_trading(&"d".to_string(), &Loom), 2);
    assert_eq!(player_state.apply_trading(&"d".to_string(), &Papyrus), 2);
}

#[test]
fn test_scientific_any() {
    let wonder = WONDERS_BY_NAME.get(&"Babylon".to_string()).unwrap();
    let mut game_state = GameState {
        player_states: hashmap! {
            "a".to_string() => PlayerState {
                player: Player("a".to_string()),
                wonder: wonder.get_wonder_side_a(),
                ..Default::default()
            },
            "b".to_string() => PlayerState {
                player: Player("b".to_string()),
                ..Default::default()
            },
            "c".to_string() => PlayerState {
                player: Player("c".to_string()),
                ..Default::default()
            },
        },
        ..default_game_state()
    };

    let tavern_structure = STRUCTURES_BY_NAME.get(&"Tavern".to_string()).unwrap();

    game_state.apply_player_decisions(
        hashmap! {"a".to_string() => PlayerDecision::BuildNextWonderStage(Card(tavern_structure))},
    );
    let player_state = game_state.get_player_state(&"a".to_string());
    assert_eq!(player_state.point_actions.len(), 1);
    assert_eq!(
        player_state.calculate_points(&game_state),
        hashmap! {
            MilitaryP => 0,
            TreasuryP => 0,
            WonderP => 3,
            CivilianP => 0,
            ScientificP => 0,
            CommercialP => 0,
            GuildP => 0
        }
    );

    game_state.apply_player_decisions(
        hashmap! {"a".to_string() => PlayerDecision::BuildNextWonderStage(Card(tavern_structure))},
    );
    let player_state = game_state.get_player_state(&"a".to_string());
    assert_eq!(player_state.point_actions.len(), 1);
    assert_eq!(
        player_state.scientific_symbols_produced.any_symbols,
        vec![&[Compass, Gears, Tablet]]
    );
}

#[test]
fn test_alexandria() {
    let wonder = WONDERS_BY_NAME.get(&"Alexandria".to_string()).unwrap();
    let mut game_state = GameState {
        player_states: hashmap! {
            "a".to_string() => PlayerState {
                player: Player("a".to_string()),
                wonder: wonder.get_wonder_side_a(),
                ..Default::default()
            },
            "b".to_string() => PlayerState {
                player: Player("b".to_string()),
                wonder: wonder.get_wonder_side_b(),
                ..Default::default()
            },
            "c".to_string() => PlayerState {
                player: Player("c".to_string()),
                ..Default::default()
            },
        },
        ..default_game_state()
    };

    let tavern_structure = STRUCTURES_BY_NAME.get(&"Tavern".to_string()).unwrap();

    // WONDER SIDE A

    game_state.apply_player_decisions(
        hashmap! {"a".to_string() => PlayerDecision::BuildNextWonderStage(Card(tavern_structure))},
    );
    let player_state = game_state.get_player_state(&"a".to_string());
    assert_eq!(player_state.point_actions.len(), 1);
    assert_eq!(
        player_state.calculate_points(&game_state),
        hashmap! {
            MilitaryP => 0,
            TreasuryP => 0,
            WonderP => 3,
            CivilianP => 0,
            ScientificP => 0,
            CommercialP => 0,
            GuildP => 0
        }
    );

    game_state.apply_player_decisions(
        hashmap! {"a".to_string() => PlayerDecision::BuildNextWonderStage(Card(tavern_structure))},
    );
    let player_state = game_state.get_player_state(&"a".to_string());
    assert_eq!(player_state.point_actions.len(), 1);
    assert_eq!(
        player_state.resources_produced.any_resources,
        vec![&[Wood, Stone, Ore, Clay]]
    );

    game_state.apply_player_decisions(
        hashmap! {"a".to_string() => PlayerDecision::BuildNextWonderStage(Card(tavern_structure))},
    );
    let player_state = game_state.get_player_state(&"a".to_string());
    assert_eq!(player_state.point_actions.len(), 2);
    assert_eq!(
        player_state.calculate_points(&game_state),
        hashmap! {
            MilitaryP => 0,
            TreasuryP => 0,
            WonderP => 10,
            CivilianP => 0,
            ScientificP => 0,
            CommercialP => 0,
            GuildP => 0
        }
    );

    // WONDER SIDE B

    game_state.apply_player_decisions(
        hashmap! {"b".to_string() => PlayerDecision::BuildNextWonderStage(Card(tavern_structure))},
    );
    let player_state = game_state.get_player_state(&"b".to_string());
    assert_eq!(
        player_state.resources_produced.any_resources,
        vec![&[Wood, Stone, Ore, Clay]]
    );

    game_state.apply_player_decisions(
        hashmap! {"b".to_string() => PlayerDecision::BuildNextWonderStage(Card(tavern_structure))},
    );
    let player_state = game_state.get_player_state(&"b".to_string());
    assert_eq!(
        player_state
            .resources_produced
            .any_resources
            .iter()
            .map(|r| r.to_vec())
            .collect::<Vec<_>>(),
        vec![vec![Wood, Stone, Ore, Clay], vec![Loom, Glass, Papyrus]]
    );

    game_state.apply_player_decisions(
        hashmap! {"b".to_string() => PlayerDecision::BuildNextWonderStage(Card(tavern_structure))},
    );
    let player_state = game_state.get_player_state(&"b".to_string());
    assert_eq!(player_state.point_actions.len(), 1);
    assert_eq!(
        player_state.calculate_points(&game_state),
        hashmap! {
            MilitaryP => 0,
            TreasuryP => 0,
            WonderP => 7,
            CivilianP => 0,
            ScientificP => 0,
            CommercialP => 0,
            GuildP => 0
        }
    );
}

#[test]
fn test_rhodos() {
    let wonder = WONDERS_BY_NAME.get(&"Rhódos".to_string()).unwrap();
    let mut game_state = GameState {
        player_states: hashmap! {
            "a".to_string() => PlayerState {
                player: Player("a".to_string()),
                wonder: wonder.get_wonder_side_a(),
                ..Default::default()
            },
            "b".to_string() => PlayerState {
                player: Player("b".to_string()),
                wonder: wonder.get_wonder_side_b(),
                ..Default::default()
            },
            "c".to_string() => PlayerState {
                player: Player("c".to_string()),
                ..Default::default()
            },
        },
        ..default_game_state()
    };

    let tavern_structure = STRUCTURES_BY_NAME.get(&"Tavern".to_string()).unwrap();

    // WONDER SIDE A

    game_state.apply_player_decisions(
        hashmap! {"a".to_string() => PlayerDecision::BuildNextWonderStage(Card(tavern_structure))},
    );
    let player_state = game_state.get_player_state(&"a".to_string());
    assert_eq!(player_state.point_actions.len(), 1);
    assert_eq!(
        player_state.calculate_points(&game_state),
        hashmap! {
            MilitaryP => 0,
            TreasuryP => 0,
            WonderP => 3,
            CivilianP => 0,
            ScientificP => 0,
            CommercialP => 0,
            GuildP => 0
        }
    );

    game_state.apply_player_decisions(
        hashmap! {"a".to_string() => PlayerDecision::BuildNextWonderStage(Card(tavern_structure))},
    );
    let player_state = game_state.get_player_state(&"a".to_string());
    assert_eq!(player_state.point_actions.len(), 1);
    assert_eq!(player_state.military_symbols, 2);
    assert_eq!(
        player_state.calculate_points(&game_state),
        hashmap! {
            MilitaryP => 0,
            TreasuryP => 0,
            WonderP => 3,
            CivilianP => 0,
            ScientificP => 0,
            CommercialP => 0,
            GuildP => 0
        }
    );

    game_state.apply_player_decisions(
        hashmap! {"a".to_string() => PlayerDecision::BuildNextWonderStage(Card(tavern_structure))},
    );
    let player_state = game_state.get_player_state(&"a".to_string());
    assert_eq!(player_state.point_actions.len(), 2);
    assert_eq!(player_state.military_symbols, 2);
    assert_eq!(
        player_state.calculate_points(&game_state),
        hashmap! {
            MilitaryP => 0,
            TreasuryP => 0,
            WonderP => 10,
            CivilianP => 0,
            ScientificP => 0,
            CommercialP => 0,
            GuildP => 0
        }
    );

    // WONDER SIDE B

    game_state.apply_player_decisions(
        hashmap! {"b".to_string() => PlayerDecision::BuildNextWonderStage(Card(tavern_structure))},
    );
    let player_state = game_state.get_player_state(&"b".to_string());
    assert_eq!(player_state.point_actions.len(), 1);
    assert_eq!(player_state.military_symbols, 1);
    assert_eq!(player_state.coins, 3);
    assert_eq!(
        player_state.calculate_points(&game_state),
        hashmap! {
            MilitaryP => 0,
            TreasuryP => 1,
            WonderP => 3,
            CivilianP => 0,
            ScientificP => 0,
            CommercialP => 0,
            GuildP => 0
        }
    );

    game_state.apply_player_decisions(
        hashmap! {"b".to_string() => PlayerDecision::BuildNextWonderStage(Card(tavern_structure))},
    );
    let player_state = game_state.get_player_state(&"b".to_string());
    assert_eq!(player_state.point_actions.len(), 2);
    assert_eq!(player_state.military_symbols, 2);
    assert_eq!(player_state.coins, 7);
    assert_eq!(
        player_state.calculate_points(&game_state),
        hashmap! {
            MilitaryP => 0,
            TreasuryP => 2,
            WonderP => 7,
            CivilianP => 0,
            ScientificP => 0,
            CommercialP => 0,
            GuildP => 0
        }
    );
}
