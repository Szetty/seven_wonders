use super::game_helpers::plain_game;
use crate::game::{settings, CardInfo, Category, Side, ENGINE_VERSION};

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
        let card = CardInfo {
            name: name.to_string(),
            category,
            age,
        };
        assert!(s.cards.contains(&card), "{name} age {age} missing");
    }
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
