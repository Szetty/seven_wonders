use super::helpers::default_player_state;
use crate::domain::{
    GameState, PlayerState,
    PointCategory::{CivilianP, CommercialP, GuildP, MilitaryP, ScientificP, TreasuryP, WonderP},
    PointsMap,
    ScientificSymbol::{Compass, Gears, Tablet},
    ScientificSymbolCounts, ScientificSymbolsProduced,
};
use maplit::hashmap;

#[test]
fn test_calculate_points() {
    assert_eq!(
        GameState {
            player_states: hashmap! {
                "a".to_string() => PlayerState{
                    coins: 0,
                    battle_tokens: vec![-1, -1, -1, -1, -1, -1],
                    point_actions: vec![
                        Box::new(|_:&_, points_map: &mut PointsMap| {
                            points_map.add(&WonderP, 3)
                        }),
                        Box::new(|_:&_, points_map: &mut PointsMap| {
                            points_map.add(&CivilianP, 6)
                        }),
                    ],
                    scientific_symbols_produced: ScientificSymbolsProduced{
                        compass: 1,
                        gears: 1,
                        tablet: 1,
                        ..Default::default()
                    },
                    ..default_player_state()
                },
                "b".to_string() => PlayerState{
                    coins: 9,
                    battle_tokens: vec![0, 1, -1, 3, 5, 5],
                    scientific_symbols_produced: ScientificSymbolsProduced{
                        compass: 4,
                        gears: 4,
                        tablet: 4,
                        actions: vec![
                            Box::new(|_, symbol_counts: &mut ScientificSymbolCounts| {
                                symbol_counts[1] += 1;
                                symbol_counts[2] += 1;
                            })
                        ],
                        any_symbols: vec![&[Tablet, Gears, Compass]],
                        ..Default::default()
                    },
                    ..default_player_state()
                },
                "c".to_string() => PlayerState{
                    coins: 4,
                    point_actions: vec![
                        Box::new(|_:&_, points_map: &mut PointsMap| {
                            points_map.add(&GuildP, 10);
                        }),
                        Box::new(|_:&_, points_map: &mut PointsMap| {
                            points_map.add(&GuildP, 10);
                        }),
                    ],
                    ..default_player_state()
                }
            },
            ..Default::default()
        }
        .calculate_points(),
        hashmap! {
            "a".to_string() => hashmap!{
                MilitaryP => -6,
                TreasuryP => 0,
                WonderP => 3,
                CivilianP => 6,
                ScientificP => 10,
                CommercialP => 0,
                GuildP => 0,
            },
            "b".to_string() => hashmap!{
                MilitaryP => 13,
                TreasuryP => 3,
                WonderP => 0,
                CivilianP => 0,
                ScientificP => 110,
                CommercialP => 0,
                GuildP => 0,
            },
            "c".to_string() => hashmap!{
                MilitaryP => 0,
                TreasuryP => 1,
                WonderP => 0,
                CivilianP => 0,
                ScientificP => 0,
                CommercialP => 0,
                GuildP => 20,
            },
        }
    )
}
