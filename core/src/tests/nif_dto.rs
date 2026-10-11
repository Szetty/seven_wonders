use super::game_helpers::plain_game;
use crate::game::{
    Action, ActionError, BuildOption, Category, Payment, PaymentOption, PhaseKind, ResourceType::*,
    SetupError, Side, WonderSelection,
};
use crate::nif::dto::{
    ActionDto, ActionErrorDto, BuildOptionDto, CategoryDto, PaymentDto, PaymentOptionDto,
    PhaseKindDto, PlayerViewDto, ResourceDto, SetupErrorDto, SideDto, WondersDto,
};

#[test]
fn action_dtos_round_trip() {
    let pairs = [
        (
            ActionDto::Build {
                card: "Altar".to_string(),
                payment: PaymentDto {
                    west: vec![],
                    east: vec![(ResourceDto::Wood, 1)],
                },
            },
            Action::Build {
                card: "Altar".to_string(),
                payment: Payment {
                    west: vec![],
                    east: vec![(Wood, 1)],
                },
            },
        ),
        (
            ActionDto::BuildWonderStage {
                card: "Altar".to_string(),
                payment: PaymentDto {
                    west: vec![(ResourceDto::Papyrus, 2)],
                    east: vec![],
                },
            },
            Action::BuildWonderStage {
                card: "Altar".to_string(),
                payment: Payment {
                    west: vec![(Papyrus, 2)],
                    east: vec![],
                },
            },
        ),
        (
            ActionDto::Discard("Altar".to_string()),
            Action::Discard {
                card: "Altar".to_string(),
            },
        ),
        (
            ActionDto::BuildFree("Altar".to_string()),
            Action::BuildFree {
                card: "Altar".to_string(),
            },
        ),
        (
            ActionDto::BuildFromDiscard("Altar".to_string()),
            Action::BuildFromDiscard {
                card: "Altar".to_string(),
            },
        ),
        (
            ActionDto::ClaimWonder {
                wonder: "Gizah".to_string(),
                side: SideDto::B,
            },
            Action::ClaimWonder {
                wonder: "Gizah".to_string(),
                side: Side::B,
            },
        ),
        (ActionDto::ConfirmWonder, Action::ConfirmWonder),
    ];
    for (dto, action) in pairs {
        assert_eq!(Action::from(dto.clone()), action);
        assert_eq!(ActionDto::from(action), dto);
    }
}

#[test]
fn setup_inputs_and_errors_convert() {
    assert_eq!(
        WonderSelection::from(WondersDto::Random),
        WonderSelection::Random
    );
    assert_eq!(
        WonderSelection::from(WondersDto::Explicit(vec![(
            "Gizah".to_string(),
            SideDto::B
        )])),
        WonderSelection::Explicit(vec![("Gizah".to_string(), Side::B)])
    );
    assert_eq!(
        SetupErrorDto::from(SetupError::InvalidPlayersNumber(2)),
        SetupErrorDto::InvalidPlayersNumber
    );
    assert_eq!(
        SetupErrorDto::from(SetupError::WondersLengthMismatch {
            players: 3,
            wonders: 1
        }),
        SetupErrorDto::WondersLengthMismatch {
            players: 3,
            wonders: 1
        }
    );
    assert_eq!(
        SetupErrorDto::from(SetupError::DuplicatePlayer("a".to_string())),
        SetupErrorDto::DuplicatePlayer("a".to_string())
    );
}

#[test]
fn view_parts_convert() {
    assert_eq!(
        ActionErrorDto::from(ActionError::CannotAfford),
        ActionErrorDto::CannotAfford
    );
    assert_eq!(
        CategoryDto::from(Category::MG),
        CategoryDto::ManufacturedGood
    );
    assert_eq!(CategoryDto::from(Category::RM), CategoryDto::RawMaterial);
    let option = PaymentOption {
        payment: Payment {
            west: vec![(Ore, 1)],
            east: vec![],
        },
        west_coins: 2,
        east_coins: 0,
        bank_coins: 0,
    };
    assert_eq!(
        BuildOptionDto::from(BuildOption::Trade(vec![option])),
        BuildOptionDto::Trade(vec![PaymentOptionDto {
            payment: PaymentDto {
                west: vec![(ResourceDto::Ore, 1)],
                east: vec![]
            },
            west_coins: 2,
            east_coins: 0,
            bank_coins: 0,
        }])
    );
    assert_eq!(
        BuildOptionDto::from(BuildOption::Unavailable {
            reason: ActionError::AlreadyBuilt
        }),
        BuildOptionDto::Unavailable(ActionErrorDto::AlreadyBuilt)
    );
}

#[test]
fn chain_option_converts() {
    assert_eq!(
        BuildOptionDto::from(BuildOption::Chain("Altar".to_string())),
        BuildOptionDto::Chain("Altar".to_string())
    );
}

#[test]
fn a_whole_view_converts() {
    let dto = PlayerViewDto::from(plain_game().view("p1").unwrap());
    assert_eq!(dto.me, "p1");
    assert_eq!(dto.phase.kind, PhaseKindDto::ChoosingCards);
    assert_eq!(dto.players.len(), 3);
    assert_eq!(dto.hand.len(), 7);
    assert_eq!(dto.submitted.len(), 3);
    assert_eq!(dto.my_pending, None);
}

#[test]
fn history_converts() {
    use crate::nif::dto::{HistoryEventDto, PublicActionDto, TurnKindDto};
    let mut game = plain_game();
    super::game_helpers::discard_turn(&mut game);
    let dto: Vec<HistoryEventDto> = game.history().iter().cloned().map(Into::into).collect();
    let HistoryEventDto::Turn {
        kind, plays, coins, ..
    } = &dto[0]
    else {
        panic!()
    };
    assert_eq!(*kind, TurnKindDto::Regular);
    assert_eq!(plays[0].action, PublicActionDto::Discarded);
    assert_eq!(coins[0].delta, 3);
}

#[test]
fn settings_convert_with_catalogue() {
    use crate::nif::dto::{CostInfoDto, EffectSpecDto, GameSettingsDto, PointCategoryDto};
    let dto = GameSettingsDto::from(crate::game::settings());
    let altar = dto.cards.iter().find(|c| c.name == "Altar").unwrap();
    assert_eq!(
        altar.cost,
        CostInfoDto {
            coins: 0,
            resources: vec![]
        }
    );
    assert_eq!(altar.chains_to, vec!["Temple".to_string()]);
    assert_eq!(
        altar.effects,
        vec![EffectSpecDto::Points {
            category: PointCategoryDto::Civilian,
            points: 2
        }]
    );
    assert_eq!(dto.wonders[0].boards.len(), 2);
}

#[test]
fn setup_errors_and_phase_convert() {
    let pairs = [
        (ActionError::UnknownWonder, ActionErrorDto::UnknownWonder),
        (ActionError::WonderTaken, ActionErrorDto::WonderTaken),
        (ActionError::NotYourWonder, ActionErrorDto::NotYourWonder),
        (ActionError::NoClaim, ActionErrorDto::NoClaim),
        (
            ActionError::AlreadyConfirmed,
            ActionErrorDto::AlreadyConfirmed,
        ),
    ];
    for (error, dto) in pairs {
        assert_eq!(ActionErrorDto::from(error), dto);
    }
    assert_eq!(PhaseKindDto::from(PhaseKind::Setup), PhaseKindDto::Setup);
}
