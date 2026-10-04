use super::game_helpers::plain_game;
use crate::game::{settings, Category, Side, ENGINE_VERSION};

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
        CostInfo {
            coins: 0,
            resources: vec![(Wood, 1), (Clay, 1), (Glass, 1)]
        }
    );
    assert_eq!(temple.chains_from, vec!["Altar".to_string()]);
    assert_eq!(temple.chains_to, vec!["Pantheon".to_string()]);
    assert_eq!(
        temple.effects,
        vec![EffectSpec::Points {
            category: PointCategory::CivilianP,
            points: 3
        }]
    );
    assert_eq!(
        card("Tree Farm").cost,
        CostInfo {
            coins: 1,
            resources: vec![]
        }
    );
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
        vec![EffectSpec::Produces {
            resources: vec![Stone],
            choice: false,
            tradable: true
        }]
    );
    let stage_two = &gizah.boards[0].stages[1];
    assert_eq!(
        stage_two.cost,
        CostInfo {
            coins: 0,
            resources: vec![(Wood, 3)]
        }
    );
    assert_eq!(
        stage_two.effects,
        vec![EffectSpec::Points {
            category: PointCategory::WonderP,
            points: 5
        }]
    );
}

#[test]
fn debug_json_is_valid_json() {
    let game = plain_game();
    let value: serde_json::Value = serde_json::from_str(&game.debug_json()).unwrap();
    assert_eq!(value["engine_version"], 1);
    assert_eq!(value["seats"], serde_json::json!(["p1", "p2", "p3"]));
    assert!(value["state"]["player_states"].is_object());
    assert_eq!(value["hands"][0].as_array().map(Vec::len), Some(7));
    assert!(value["phase"]["ChoosingCards"].is_object());
}
