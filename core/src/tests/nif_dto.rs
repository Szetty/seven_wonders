use super::game_helpers::plain_game;
use crate::game::{
    Action, ActionError, BuildOption, Category, Payment, PaymentOption, ResourceType::*,
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
fn a_whole_view_converts() {
    let dto = PlayerViewDto::from(plain_game().view("p1").unwrap());
    assert_eq!(dto.me, "p1");
    assert_eq!(dto.phase.kind, PhaseKindDto::ChoosingCards);
    assert_eq!(dto.players.len(), 3);
    assert_eq!(dto.hand.len(), 7);
    assert_eq!(dto.submitted.len(), 3);
    assert_eq!(dto.my_pending, None);
}
