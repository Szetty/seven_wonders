//! Boundary DTOs: rustler encodings of `crate::game` types. Conversions only;
//! no game logic lives here.
use crate::game::{
    Action, ActionError, BuildOption, BuiltCard, CardInfo, Category, ExtraTurnKind, FinalScore,
    GameSettings, HandCard, PassDirection, Payment, PaymentOption, PhaseKind, PhaseView,
    PlayerView, PublicPlayer, ResourceType, SetupError, Side, WonderInfo, WonderSelection,
};
use rustler::{NifMap, NifTaggedEnum, NifUnitEnum};

#[derive(Debug, Clone, Copy, PartialEq, Eq, NifUnitEnum)]
pub enum ResourceDto {
    Wood,
    Stone,
    Ore,
    Clay,
    Glass,
    Loom,
    Papyrus,
}

impl From<ResourceType> for ResourceDto {
    fn from(resource: ResourceType) -> Self {
        match resource {
            ResourceType::Wood => Self::Wood,
            ResourceType::Stone => Self::Stone,
            ResourceType::Ore => Self::Ore,
            ResourceType::Clay => Self::Clay,
            ResourceType::Glass => Self::Glass,
            ResourceType::Loom => Self::Loom,
            ResourceType::Papyrus => Self::Papyrus,
        }
    }
}

impl From<ResourceDto> for ResourceType {
    fn from(resource: ResourceDto) -> Self {
        match resource {
            ResourceDto::Wood => Self::Wood,
            ResourceDto::Stone => Self::Stone,
            ResourceDto::Ore => Self::Ore,
            ResourceDto::Clay => Self::Clay,
            ResourceDto::Glass => Self::Glass,
            ResourceDto::Loom => Self::Loom,
            ResourceDto::Papyrus => Self::Papyrus,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, NifUnitEnum)]
pub enum CategoryDto {
    Civilian,
    Commercial,
    Guild,
    ManufacturedGood,
    Military,
    RawMaterial,
    Scientific,
}

impl From<Category> for CategoryDto {
    fn from(category: Category) -> Self {
        match category {
            Category::Civilian => Self::Civilian,
            Category::Commercial => Self::Commercial,
            Category::Guild => Self::Guild,
            Category::MG => Self::ManufacturedGood,
            Category::Military => Self::Military,
            Category::RM => Self::RawMaterial,
            Category::Scientific => Self::Scientific,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, NifUnitEnum)]
pub enum SideDto {
    A,
    B,
}

impl From<Side> for SideDto {
    fn from(side: Side) -> Self {
        match side {
            Side::A => Self::A,
            Side::B => Self::B,
        }
    }
}

impl From<SideDto> for Side {
    fn from(side: SideDto) -> Self {
        match side {
            SideDto::A => Self::A,
            SideDto::B => Self::B,
        }
    }
}

/// `:random` | `{:explicit, [{"Gizah", :a}, …]}`
#[derive(Debug, Clone, PartialEq, Eq, NifTaggedEnum)]
pub enum WondersDto {
    Random,
    Explicit(Vec<(String, SideDto)>),
}

impl From<WondersDto> for WonderSelection {
    fn from(wonders: WondersDto) -> Self {
        match wonders {
            WondersDto::Random => Self::Random,
            WondersDto::Explicit(choices) => Self::Explicit(
                choices
                    .into_iter()
                    .map(|(name, side)| (name, side.into()))
                    .collect(),
            ),
        }
    }
}

/// `%{west: [{:wood, 1}], east: []}`
#[derive(Debug, Clone, PartialEq, Eq, NifMap)]
pub struct PaymentDto {
    pub west: Vec<(ResourceDto, u8)>,
    pub east: Vec<(ResourceDto, u8)>,
}

impl From<Payment> for PaymentDto {
    fn from(payment: Payment) -> Self {
        let convert =
            |side: Vec<(ResourceType, u8)>| side.into_iter().map(|(r, n)| (r.into(), n)).collect();
        Self {
            west: convert(payment.west),
            east: convert(payment.east),
        }
    }
}

impl From<PaymentDto> for Payment {
    fn from(payment: PaymentDto) -> Self {
        let convert =
            |side: Vec<(ResourceDto, u8)>| side.into_iter().map(|(r, n)| (r.into(), n)).collect();
        Self {
            west: convert(payment.west),
            east: convert(payment.east),
        }
    }
}

/// `{:build, %{card, payment}}` | `{:build_wonder_stage, %{card, payment}}` |
/// `{:discard, card}` | `{:build_free, card}` | `{:build_from_discard, card}`
#[derive(Debug, Clone, PartialEq, Eq, NifTaggedEnum)]
pub enum ActionDto {
    Build { card: String, payment: PaymentDto },
    BuildWonderStage { card: String, payment: PaymentDto },
    Discard(String),
    BuildFree(String),
    BuildFromDiscard(String),
}

impl From<ActionDto> for Action {
    fn from(action: ActionDto) -> Self {
        match action {
            ActionDto::Build { card, payment } => Self::Build {
                card,
                payment: payment.into(),
            },
            ActionDto::BuildWonderStage { card, payment } => Self::BuildWonderStage {
                card,
                payment: payment.into(),
            },
            ActionDto::Discard(card) => Self::Discard { card },
            ActionDto::BuildFree(card) => Self::BuildFree { card },
            ActionDto::BuildFromDiscard(card) => Self::BuildFromDiscard { card },
        }
    }
}

impl From<Action> for ActionDto {
    fn from(action: Action) -> Self {
        match action {
            Action::Build { card, payment } => Self::Build {
                card,
                payment: payment.into(),
            },
            Action::BuildWonderStage { card, payment } => Self::BuildWonderStage {
                card,
                payment: payment.into(),
            },
            Action::Discard { card } => Self::Discard(card),
            Action::BuildFree { card } => Self::BuildFree(card),
            Action::BuildFromDiscard { card } => Self::BuildFromDiscard(card),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, NifUnitEnum)]
pub enum ActionErrorDto {
    UnknownPlayer,
    NotYourTurn,
    GameOver,
    CardNotInHand,
    CardNotInDiscard,
    AlreadyBuilt,
    CannotAfford,
    InvalidPayment,
    NoWonderStageLeft,
    FreeBuildUnavailable,
    ActionNotAllowedNow,
}

impl From<ActionError> for ActionErrorDto {
    fn from(error: ActionError) -> Self {
        match error {
            ActionError::UnknownPlayer => Self::UnknownPlayer,
            ActionError::NotYourTurn => Self::NotYourTurn,
            ActionError::GameOver => Self::GameOver,
            ActionError::CardNotInHand => Self::CardNotInHand,
            ActionError::CardNotInDiscard => Self::CardNotInDiscard,
            ActionError::AlreadyBuilt => Self::AlreadyBuilt,
            ActionError::CannotAfford => Self::CannotAfford,
            ActionError::InvalidPayment => Self::InvalidPayment,
            ActionError::NoWonderStageLeft => Self::NoWonderStageLeft,
            ActionError::FreeBuildUnavailable => Self::FreeBuildUnavailable,
            ActionError::ActionNotAllowedNow => Self::ActionNotAllowedNow,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, NifTaggedEnum)]
pub enum SetupErrorDto {
    InvalidPlayersNumber,
    DuplicatePlayer(String),
    InvalidWonder(String),
    WondersLengthMismatch { players: usize, wonders: usize },
    DuplicateWonder(String),
}

impl From<SetupError> for SetupErrorDto {
    fn from(error: SetupError) -> Self {
        match error {
            SetupError::InvalidPlayersNumber(_) => Self::InvalidPlayersNumber,
            SetupError::DuplicatePlayer(name) => Self::DuplicatePlayer(name),
            SetupError::InvalidWonder(name) => Self::InvalidWonder(name),
            SetupError::WondersLengthMismatch { players, wonders } => {
                Self::WondersLengthMismatch { players, wonders }
            }
            SetupError::DuplicateWonder(name) => Self::DuplicateWonder(name),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, NifUnitEnum)]
pub enum ExtraTurnKindDto {
    PlayLastCard,
    BuildFromDiscard,
}

impl From<ExtraTurnKind> for ExtraTurnKindDto {
    fn from(kind: ExtraTurnKind) -> Self {
        match kind {
            ExtraTurnKind::PlayLastCard => Self::PlayLastCard,
            ExtraTurnKind::BuildFromDiscard => Self::BuildFromDiscard,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, NifUnitEnum)]
pub enum PhaseKindDto {
    ChoosingCards,
    ExtraTurn,
    GameOver,
}

impl From<PhaseKind> for PhaseKindDto {
    fn from(kind: PhaseKind) -> Self {
        match kind {
            PhaseKind::ChoosingCards => Self::ChoosingCards,
            PhaseKind::ExtraTurn => Self::ExtraTurn,
            PhaseKind::GameOver => Self::GameOver,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, NifUnitEnum)]
pub enum DirectionDto {
    West,
    East,
}

impl From<PassDirection> for DirectionDto {
    fn from(direction: PassDirection) -> Self {
        match direction {
            PassDirection::West => Self::West,
            PassDirection::East => Self::East,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, NifMap)]
pub struct PhaseViewDto {
    pub kind: PhaseKindDto,
    pub age: u8,
    pub turn: u8,
    pub direction: DirectionDto,
    pub extra_turn_player: Option<String>,
    pub extra_turn_kind: Option<ExtraTurnKindDto>,
}

impl From<PhaseView> for PhaseViewDto {
    fn from(phase: PhaseView) -> Self {
        Self {
            kind: phase.kind.into(),
            age: phase.age,
            turn: phase.turn,
            direction: phase.direction.into(),
            extra_turn_player: phase.extra_turn_player,
            extra_turn_kind: phase.extra_turn_kind.map(Into::into),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, NifMap)]
pub struct BuiltCardDto {
    pub name: String,
    pub category: CategoryDto,
    pub age: u8,
}

impl From<BuiltCard> for BuiltCardDto {
    fn from(card: BuiltCard) -> Self {
        Self {
            name: card.name,
            category: card.category.into(),
            age: card.age,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, NifMap)]
pub struct PublicPlayerDto {
    pub name: String,
    pub wonder: String,
    pub side: SideDto,
    pub stages_built: u8,
    pub stages_total: u8,
    pub built: Vec<BuiltCardDto>,
    pub coins: u32,
    pub shields: u32,
    pub military_tokens: Vec<i32>,
    pub free_build_available: bool,
}

impl From<PublicPlayer> for PublicPlayerDto {
    fn from(player: PublicPlayer) -> Self {
        Self {
            name: player.name,
            wonder: player.wonder,
            side: player.side.into(),
            stages_built: player.stages_built,
            stages_total: player.stages_total,
            built: player.built.into_iter().map(Into::into).collect(),
            coins: player.coins,
            shields: player.shields,
            military_tokens: player.military_tokens,
            free_build_available: player.free_build_available,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, NifMap)]
pub struct PaymentOptionDto {
    pub payment: PaymentDto,
    pub west_coins: u32,
    pub east_coins: u32,
    pub bank_coins: u32,
}

impl From<PaymentOption> for PaymentOptionDto {
    fn from(option: PaymentOption) -> Self {
        Self {
            payment: option.payment.into(),
            west_coins: option.west_coins,
            east_coins: option.east_coins,
            bank_coins: option.bank_coins,
        }
    }
}

/// `{:unavailable, reason}` | `:free` | `{:coins, n}` | `{:trade, [option]}`
#[derive(Debug, Clone, PartialEq, Eq, NifTaggedEnum)]
pub enum BuildOptionDto {
    Unavailable(ActionErrorDto),
    Free,
    Coins(u32),
    Trade(Vec<PaymentOptionDto>),
}

impl From<BuildOption> for BuildOptionDto {
    fn from(option: BuildOption) -> Self {
        match option {
            BuildOption::Unavailable { reason } => Self::Unavailable(reason.into()),
            BuildOption::Free => Self::Free,
            BuildOption::Coins(coins) => Self::Coins(coins),
            BuildOption::Trade(options) => {
                Self::Trade(options.into_iter().map(Into::into).collect())
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, NifMap)]
pub struct HandCardDto {
    pub name: String,
    pub category: CategoryDto,
    pub age: u8,
    pub build: BuildOptionDto,
    pub wonder_stage: BuildOptionDto,
    pub free_build: bool,
}

impl From<HandCard> for HandCardDto {
    fn from(card: HandCard) -> Self {
        Self {
            name: card.name,
            category: card.category.into(),
            age: card.age,
            build: card.build.into(),
            wonder_stage: card.wonder_stage.into(),
            free_build: card.free_build,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, NifMap)]
pub struct FinalScoreDto {
    pub player: String,
    pub military: i32,
    pub treasury: i32,
    pub wonder: i32,
    pub civilian: i32,
    pub scientific: i32,
    pub commercial: i32,
    pub guild: i32,
    pub total: i32,
    pub coins: u32,
    pub rank: u8,
}

impl From<FinalScore> for FinalScoreDto {
    fn from(score: FinalScore) -> Self {
        Self {
            player: score.player,
            military: score.military,
            treasury: score.treasury,
            wonder: score.wonder,
            civilian: score.civilian,
            scientific: score.scientific,
            commercial: score.commercial,
            guild: score.guild,
            total: score.total,
            coins: score.coins,
            rank: score.rank,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, NifMap)]
pub struct PlayerViewDto {
    pub me: String,
    pub phase: PhaseViewDto,
    pub players: Vec<PublicPlayerDto>,
    pub west: String,
    pub east: String,
    pub hand: Vec<HandCardDto>,
    pub discard_pile: Option<Vec<String>>,
    pub discard_count: usize,
    pub submitted: Vec<(String, bool)>,
    pub my_pending: Option<ActionDto>,
    pub scores: Option<Vec<FinalScoreDto>>,
}

impl From<PlayerView> for PlayerViewDto {
    fn from(view: PlayerView) -> Self {
        Self {
            me: view.me,
            phase: view.phase.into(),
            players: view.players.into_iter().map(Into::into).collect(),
            west: view.west,
            east: view.east,
            hand: view.hand.into_iter().map(Into::into).collect(),
            discard_pile: view.discard_pile,
            discard_count: view.discard_count,
            submitted: view.submitted,
            my_pending: view.my_pending.map(Into::into),
            scores: view
                .scores
                .map(|scores| scores.into_iter().map(Into::into).collect()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, NifMap)]
pub struct WonderInfoDto {
    pub name: String,
    pub sides: Vec<SideDto>,
}

impl From<WonderInfo> for WonderInfoDto {
    fn from(wonder: WonderInfo) -> Self {
        Self {
            name: wonder.name,
            sides: wonder.sides.into_iter().map(Into::into).collect(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, NifMap)]
pub struct CardInfoDto {
    pub name: String,
    pub category: CategoryDto,
    pub age: u8,
}

impl From<CardInfo> for CardInfoDto {
    fn from(card: CardInfo) -> Self {
        Self {
            name: card.name,
            category: card.category.into(),
            age: card.age,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, NifMap)]
pub struct GameSettingsDto {
    pub engine_version: u32,
    pub wonders: Vec<WonderInfoDto>,
    pub cards: Vec<CardInfoDto>,
}

impl From<GameSettings> for GameSettingsDto {
    fn from(settings: GameSettings) -> Self {
        Self {
            engine_version: settings.engine_version,
            wonders: settings.wonders.into_iter().map(Into::into).collect(),
            cards: settings.cards.into_iter().map(Into::into).collect(),
        }
    }
}
