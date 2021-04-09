use crate::core::trading::{try_trading, TradingOptions};
use crate::domain::{
    GameState, Neighbours, Player, PlayerState, ResourceCost, ResourceCostOptions, ResourceCosts,
    ResourceCount, ResourceType, ResourceType::*, ResourceTypes, ResourcesProduced,
};
use maplit::{hashmap, hashset};
use std::collections::HashMap;

#[test]
fn test_nothing_to_trade() {
    let f = |resource_cost_options: &'static [&'static [ResourceCost]]| -> TradingOptions {
        let player_names = vec!["a".to_string(), "b".to_string(), "c".to_string()];
        let game_state = GameState {
            neighbours: Neighbours::new(player_names),
            player_states: hashmap! {
                "a".to_string() => PlayerState {
                    player: Player("a".to_string()),
                    ..Default::default()
                },
                "b".to_string() => PlayerState {
                    player: Player("a".to_string()),
                    ..Default::default()
                },
                "c".to_string() => PlayerState {
                    player: Player("a".to_string()),
                    ..Default::default()
                },
            },
            ..Default::default()
        };
        try_trading(
            &game_state,
            &"b".to_string(),
            resource_cost_options.iter().map(|r| r.to_vec()).collect(),
        )
    };

    assert_eq!(f(&[]), hashset![]);
    assert_eq!(f(&[&[ResourceCost(Wood, 1)]]), hashset![]);
    assert_eq!(f(&[&[ResourceCost(Wood, 2)]]), hashset![]);
    assert_eq!(
        f(&[&[ResourceCost(Wood, 1), ResourceCost(Loom, 1)]]),
        hashset![]
    );
    assert_eq!(
        f(&[&[ResourceCost(Wood, 1)], &[ResourceCost(Loom, 1)]]),
        hashset![]
    );
    assert_eq!(
        f(&[
            &[ResourceCost(Wood, 1), ResourceCost(Loom, 2)],
            &[ResourceCost(Ore, 3), ResourceCost(Papyrus, 4)]
        ]),
        hashset![]
    );
}

#[test]
fn test_trade_without_discount() {
    let f = |resource_cost_options: &'static [&'static [ResourceCost]]| -> TradingOptions {
        let player_names = vec!["a".to_string(), "b".to_string(), "c".to_string()];
        let game_state = GameState {
            neighbours: Neighbours::new(player_names),
            player_states: hashmap! {
                "a".to_string() => PlayerState {
                    player: Player("a".to_string()),
                    resources_produced: ResourcesProduced {
                        single_resources: hashmap! {
                            Ore => 1,
                            Clay => 1
                        },
                        ..Default::default()
                    },
                    ..Default::default()
                },
                "b".to_string() => PlayerState {
                    player: Player("b".to_string()),
                    ..Default::default()
                },
                "c".to_string() => PlayerState {
                    player: Player("c".to_string()),
                    resources_produced: ResourcesProduced {
                        single_resources: hashmap! {
                            Wood => 1,
                            Clay => 1
                        },
                        ..Default::default()
                    },
                    ..Default::default()
                },
            },
            ..Default::default()
        };
        try_trading(
            &game_state,
            &"b".to_string(),
            resource_cost_options.iter().map(|r| r.to_vec()).collect(),
        )
    };

    assert_eq!(
        f(&[&[ResourceCost(Wood, 1)]]),
        hashset![vec![("c".to_string(), Wood, 2)]]
    );
    assert_eq!(
        f(&[&[ResourceCost(Ore, 1)]]),
        hashset![vec![("a".to_string(), Ore, 2)]]
    );
    assert_eq!(
        f(&[&[ResourceCost(Clay, 1)]]),
        hashset![
            vec![("a".to_string(), Clay, 2)],
            vec![("c".to_string(), Clay, 2)]
        ]
    );
    assert_eq!(f(&[&[ResourceCost(Stone, 1)]]), hashset![]);
    assert_eq!(
        f(&[&[ResourceCost(Wood, 1)], &[ResourceCost(Ore, 1)]]),
        hashset![
            vec![("c".to_string(), Wood, 2)],
            vec![("a".to_string(), Ore, 2)]
        ]
    );
    assert_eq!(f(&[&[ResourceCost(Wood, 2)]]), hashset![]);
    assert_eq!(f(&[&[ResourceCost(Ore, 2)]]), hashset![]);
    assert_eq!(
        f(&[&[ResourceCost(Clay, 2)]]),
        hashset![vec![("a".to_string(), Clay, 2), ("c".to_string(), Clay, 2)]]
    );
    assert_eq!(
        f(&[&[ResourceCost(Wood, 1), ResourceCost(Ore, 1)]]),
        hashset![vec![("a".to_string(), Ore, 2), ("c".to_string(), Wood, 2)]]
    );
    assert_eq!(f(&[&[ResourceCost(Clay, 3)]]), hashset![]);
}

#[test]
fn test_trade_with_one_discount_on_all_resources() {
    let f = |resource_cost_options: &'static [&'static [ResourceCost]]| -> TradingOptions {
        let player_names = vec!["a".to_string(), "b".to_string(), "c".to_string()];
        let game_state = GameState {
            neighbours: Neighbours::new(player_names),
            player_states: hashmap! {
                "a".to_string() => PlayerState {
                    player: Player("a".to_string()),
                    resources_produced: ResourcesProduced {
                        single_resources: hashmap! {
                            Ore => 1,
                            Clay => 1
                        },
                        ..Default::default()
                    },
                    ..Default::default()
                },
                "b".to_string() => PlayerState {
                    player: Player("b".to_string()),
                    ..Default::default()
                }.add_trade_action_move(Box::new(|player_name, _| {
                    if player_name == "a" {
                        1
                    } else {
                        2
                    }
                })),
                "c".to_string() => PlayerState {
                    player: Player("c".to_string()),
                    resources_produced: ResourcesProduced {
                        single_resources: hashmap! {
                            Wood => 1,
                            Clay => 1
                        },
                        ..Default::default()
                    },
                    ..Default::default()
                },
            },
            ..Default::default()
        };
        try_trading(
            &game_state,
            &"b".to_string(),
            resource_cost_options.iter().map(|r| r.to_vec()).collect(),
        )
    };

    assert_eq!(
        f(&[&[ResourceCost(Wood, 1)]]),
        hashset![vec![("c".to_string(), Wood, 2)]]
    );
    assert_eq!(
        f(&[&[ResourceCost(Ore, 1)]]),
        hashset![vec![("a".to_string(), Ore, 1)]]
    );
    assert_eq!(
        f(&[&[ResourceCost(Clay, 1)]]),
        hashset![
            vec![("a".to_string(), Clay, 1)],
            vec![("c".to_string(), Clay, 2)]
        ]
    );
    assert_eq!(f(&[&[ResourceCost(Stone, 1)]]), hashset![]);
    assert_eq!(
        f(&[&[ResourceCost(Wood, 1)], &[ResourceCost(Ore, 1)]]),
        hashset![
            vec![("c".to_string(), Wood, 2)],
            vec![("a".to_string(), Ore, 1)]
        ]
    );
    assert_eq!(f(&[&[ResourceCost(Wood, 2)]]), hashset![]);
    assert_eq!(f(&[&[ResourceCost(Ore, 2)]]), hashset![]);
    assert_eq!(
        f(&[&[ResourceCost(Clay, 2)]]),
        hashset![vec![("a".to_string(), Clay, 1), ("c".to_string(), Clay, 2)]]
    );
    assert_eq!(
        f(&[&[ResourceCost(Wood, 1), ResourceCost(Ore, 1)]]),
        hashset![vec![("a".to_string(), Ore, 1), ("c".to_string(), Wood, 2)]]
    );
    assert_eq!(f(&[&[ResourceCost(Clay, 3)]]), hashset![]);
}

#[test]
fn test_trade_with_both_discounts_on_all_resources() {
    let f = |resource_cost_options: &'static [&'static [ResourceCost]]| -> TradingOptions {
        let player_names = vec!["a".to_string(), "b".to_string(), "c".to_string()];
        let game_state = GameState {
            neighbours: Neighbours::new(player_names),
            player_states: hashmap! {
                "a".to_string() => PlayerState {
                    player: Player("a".to_string()),
                    resources_produced: ResourcesProduced {
                        single_resources: hashmap! {
                            Ore => 1,
                            Clay => 1
                        },
                        ..Default::default()
                    },
                    ..Default::default()
                },
                "b".to_string() => PlayerState {
                    player: Player("b".to_string()),
                    ..Default::default()
                }.add_trade_action_move(Box::new(|_, _| 1)),
                "c".to_string() => PlayerState {
                    player: Player("c".to_string()),
                    resources_produced: ResourcesProduced {
                        single_resources: hashmap! {
                            Wood => 1,
                            Clay => 1
                        },
                        ..Default::default()
                    },
                    ..Default::default()
                },
            },
            ..Default::default()
        };
        try_trading(
            &game_state,
            &"b".to_string(),
            resource_cost_options.iter().map(|r| r.to_vec()).collect(),
        )
    };

    assert_eq!(
        f(&[&[ResourceCost(Wood, 1)]]),
        hashset![vec![("c".to_string(), Wood, 1)]]
    );
    assert_eq!(
        f(&[&[ResourceCost(Ore, 1)]]),
        hashset![vec![("a".to_string(), Ore, 1)]]
    );
    assert_eq!(
        f(&[&[ResourceCost(Clay, 1)]]),
        hashset![
            vec![("a".to_string(), Clay, 1)],
            vec![("c".to_string(), Clay, 1)]
        ]
    );
    assert_eq!(f(&[&[ResourceCost(Stone, 1)]]), hashset![]);
    assert_eq!(
        f(&[&[ResourceCost(Wood, 1)], &[ResourceCost(Ore, 1)]]),
        hashset![
            vec![("c".to_string(), Wood, 1)],
            vec![("a".to_string(), Ore, 1)]
        ]
    );
    assert_eq!(f(&[&[ResourceCost(Wood, 2)]]), hashset![]);
    assert_eq!(f(&[&[ResourceCost(Ore, 2)]]), hashset![]);
    assert_eq!(
        f(&[&[ResourceCost(Clay, 2)]]),
        hashset![vec![("a".to_string(), Clay, 1), ("c".to_string(), Clay, 1)]]
    );
    assert_eq!(
        f(&[&[ResourceCost(Wood, 1), ResourceCost(Ore, 1)]]),
        hashset![vec![("a".to_string(), Ore, 1), ("c".to_string(), Wood, 1)]]
    );
    assert_eq!(f(&[&[ResourceCost(Clay, 3)]]), hashset![]);
}

#[test]
fn test_trade_with_one_discount_on_some_resources() {
    let f = |resource_cost_options: &'static [&'static [ResourceCost]]| -> TradingOptions {
        let player_names = vec!["a".to_string(), "b".to_string(), "c".to_string()];
        let game_state = GameState {
            neighbours: Neighbours::new(player_names),
            player_states: hashmap! {
                "a".to_string() => PlayerState {
                    player: Player("a".to_string()),
                    resources_produced: ResourcesProduced {
                        single_resources: hashmap! {
                            Ore => 1,
                            Clay => 1
                        },
                        ..Default::default()
                    },
                    ..Default::default()
                },
                "b".to_string() => PlayerState {
                    player: Player("b".to_string()),
                    ..Default::default()
                }.add_trade_action_move(Box::new(|player_name, resource_type| {
                    if player_name == "a" && *resource_type == Clay {
                        1
                    } else {
                        2
                    }
                })),
                "c".to_string() => PlayerState {
                    player: Player("c".to_string()),
                    resources_produced: ResourcesProduced {
                        single_resources: hashmap! {
                            Wood => 1,
                            Clay => 1
                        },
                        ..Default::default()
                    },
                    ..Default::default()
                },
            },
            ..Default::default()
        };
        try_trading(
            &game_state,
            &"b".to_string(),
            resource_cost_options.iter().map(|r| r.to_vec()).collect(),
        )
    };

    assert_eq!(
        f(&[&[ResourceCost(Wood, 1)]]),
        hashset![vec![("c".to_string(), Wood, 2)]]
    );
    assert_eq!(
        f(&[&[ResourceCost(Ore, 1)]]),
        hashset![vec![("a".to_string(), Ore, 2)]]
    );
    assert_eq!(f(&[&[ResourceCost(Stone, 1)]]), hashset![]);
    assert_eq!(
        f(&[&[ResourceCost(Clay, 1)]]),
        hashset![
            vec![("a".to_string(), Clay, 1)],
            vec![("c".to_string(), Clay, 2)]
        ]
    );
    assert_eq!(
        f(&[&[ResourceCost(Wood, 1)], &[ResourceCost(Ore, 1)]]),
        hashset![
            vec![("c".to_string(), Wood, 2)],
            vec![("a".to_string(), Ore, 2)]
        ]
    );
    assert_eq!(f(&[&[ResourceCost(Wood, 2)]]), hashset![]);
    assert_eq!(f(&[&[ResourceCost(Ore, 2)]]), hashset![]);
    assert_eq!(
        f(&[&[ResourceCost(Clay, 2)]]),
        hashset![vec![("a".to_string(), Clay, 1), ("c".to_string(), Clay, 2)]]
    );
    assert_eq!(
        f(&[&[ResourceCost(Wood, 1), ResourceCost(Ore, 1)]]),
        hashset![vec![("a".to_string(), Ore, 2), ("c".to_string(), Wood, 2)]]
    );
    assert_eq!(f(&[&[ResourceCost(Clay, 3)]]), hashset![]);
}

#[test]
fn test_trade_without_discount_repeating_resources() {
    let f = |resource_cost_options: &'static [&'static [ResourceCost]]| -> TradingOptions {
        let player_names = vec!["a".to_string(), "b".to_string(), "c".to_string()];
        let game_state = GameState {
            neighbours: Neighbours::new(player_names),
            player_states: hashmap! {
                "a".to_string() => PlayerState {
                    player: Player("a".to_string()),
                    resources_produced: ResourcesProduced {
                        single_resources: hashmap! {
                            Ore => 1,
                            Clay => 1
                        },
                        ..Default::default()
                    },
                    ..Default::default()
                },
                "b".to_string() => PlayerState {
                    player: Player("b".to_string()),
                    ..Default::default()
                },
                "c".to_string() => PlayerState {
                    player: Player("c".to_string()),
                    resources_produced: ResourcesProduced {
                        single_resources: hashmap! {
                            Clay => 2
                        },
                        ..Default::default()
                    },
                    ..Default::default()
                },
            },
            ..Default::default()
        };
        try_trading(
            &game_state,
            &"b".to_string(),
            resource_cost_options.iter().map(|r| r.to_vec()).collect(),
        )
    };

    assert_eq!(
        f(&[&[ResourceCost(Clay, 1)]]),
        hashset![
            vec![("a".to_string(), Clay, 2)],
            vec![("c".to_string(), Clay, 2)]
        ]
    );
    assert_eq!(
        f(&[&[ResourceCost(Clay, 2)]]),
        hashset![
            vec![("a".to_string(), Clay, 2), ("c".to_string(), Clay, 2)],
            vec![("c".to_string(), Clay, 2), ("c".to_string(), Clay, 2)]
        ]
    );
    assert_eq!(
        f(&[&[ResourceCost(Clay, 3)]]),
        hashset![vec![
            ("a".to_string(), Clay, 2),
            ("c".to_string(), Clay, 2),
            ("c".to_string(), Clay, 2)
        ]]
    );
}
