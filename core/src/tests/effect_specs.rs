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
        assert!(
            !structure.effects().is_empty(),
            "{} has no effect",
            structure.name()
        );
    }
    for wonder in WONDERS.iter() {
        for side in [&wonder.1, &wonder.2] {
            assert!(
                !side.initial_effects().is_empty(),
                "{} has no starting resource",
                side.name()
            );
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
        vec![EffectSpec::Produces {
            resources: vec![Wood],
            choice: false,
            tradable: true
        }]
    );
    assert_eq!(
        specs("Tree Farm"),
        vec![EffectSpec::Produces {
            resources: vec![Wood, Clay],
            choice: true,
            tradable: true
        }]
    );
    assert_eq!(
        specs("Forum"),
        vec![EffectSpec::Produces {
            resources: vec![Loom, Glass, Papyrus],
            choice: true,
            tradable: false
        }]
    );
    assert_eq!(specs("Tavern"), vec![EffectSpec::Coins(5)]);
    assert_eq!(specs("Stockade"), vec![EffectSpec::Military(1)]);
    assert_eq!(
        specs("Altar"),
        vec![EffectSpec::Points {
            category: PointCategory::CivilianP,
            points: 2
        }]
    );
    assert_eq!(
        specs("Workshop"),
        vec![EffectSpec::Science {
            symbols: vec![ScientificSymbol::Gears],
            choice: false
        }]
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
            EffectSpec::Points {
                category: PointCategory::WonderP,
                points: 3
            },
            EffectSpec::Coins(3),
        ]
    );
    let babylon_b = &WONDERS_BY_NAME["Babylon"].2;
    assert_eq!(
        babylon_b.wonder_stage_with_idx(2).effects()[0].spec,
        EffectSpec::PlayLastCard
    );
}
