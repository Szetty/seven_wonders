//! Boundary DTOs: rustler encodings of `crate::game` types. Conversions only;
//! no game logic lives here.
use crate::game::{
    Action, ActionError, Battle, BuildOption, BuiltCard, CardInfo, Category, CoinChange, CostInfo,
    EffectSpec, ExtraTurnKind, FinalScore, GameSettings, HandCard, HistoryEvent, PassDirection,
    Payment, PaymentOption, PhaseKind, PhaseView, Play, PlayerDirection, PlayerView, PointCategory,
    PublicAction, PublicPlayer, ResourceType, ScientificSymbol, SetupError, Side, StageInfo,
    TurnKind, WonderBoardInfo, WonderInfo, WonderSelection,
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

/// `{:unavailable, reason}` | `:free` | `{:chain, card}` | `{:coins, n}` | `{:trade, [option]}`
#[derive(Debug, Clone, PartialEq, Eq, NifTaggedEnum)]
pub enum BuildOptionDto {
    Unavailable(ActionErrorDto),
    Free,
    /// `{:chain, "Altar"}`
    Chain(String),
    Coins(u32),
    Trade(Vec<PaymentOptionDto>),
}

impl From<BuildOption> for BuildOptionDto {
    fn from(option: BuildOption) -> Self {
        match option {
            BuildOption::Unavailable { reason } => Self::Unavailable(reason.into()),
            BuildOption::Free => Self::Free,
            BuildOption::Chain(from) => Self::Chain(from),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, NifUnitEnum)]
pub enum PointCategoryDto {
    Military,
    Treasury,
    Wonder,
    Civilian,
    Scientific,
    Commercial,
    Guild,
}

impl From<PointCategory> for PointCategoryDto {
    fn from(category: PointCategory) -> Self {
        match category {
            PointCategory::MilitaryP => Self::Military,
            PointCategory::TreasuryP => Self::Treasury,
            PointCategory::WonderP => Self::Wonder,
            PointCategory::CivilianP => Self::Civilian,
            PointCategory::ScientificP => Self::Scientific,
            PointCategory::CommercialP => Self::Commercial,
            PointCategory::GuildP => Self::Guild,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, NifUnitEnum)]
pub enum ScienceSymbolDto {
    Compass,
    Gears,
    Tablet,
}

impl From<ScientificSymbol> for ScienceSymbolDto {
    fn from(symbol: ScientificSymbol) -> Self {
        match symbol {
            ScientificSymbol::Compass => Self::Compass,
            ScientificSymbol::Gears => Self::Gears,
            ScientificSymbol::Tablet => Self::Tablet,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, NifUnitEnum)]
pub enum EffectDirectionDto {
    West,
    East,
    Own,
}

impl From<PlayerDirection> for EffectDirectionDto {
    fn from(direction: PlayerDirection) -> Self {
        match direction {
            PlayerDirection::West => Self::West,
            PlayerDirection::East => Self::East,
            PlayerDirection::Own => Self::Own,
        }
    }
}

fn convert<A, B: From<A>>(items: Vec<A>) -> Vec<B> {
    items.into_iter().map(Into::into).collect()
}

/// See `Helios.Core` docs for the Elixir shapes.
#[derive(Debug, Clone, PartialEq, Eq, NifTaggedEnum)]
pub enum EffectSpecDto {
    Produces {
        resources: Vec<ResourceDto>,
        choice: bool,
        tradable: bool,
    },
    Points {
        category: PointCategoryDto,
        points: i32,
    },
    Military(u32),
    Science {
        symbols: Vec<ScienceSymbolDto>,
        choice: bool,
    },
    Coins(u32),
    TradeDiscount {
        directions: Vec<EffectDirectionDto>,
        resources: Vec<ResourceDto>,
    },
    CoinsPerCard {
        directions: Vec<EffectDirectionDto>,
        categories: Vec<CategoryDto>,
        coins: u32,
    },
    PointsPerCard {
        category: PointCategoryDto,
        directions: Vec<EffectDirectionDto>,
        categories: Vec<CategoryDto>,
        points: i32,
    },
    CoinsPerWonderStage {
        directions: Vec<EffectDirectionDto>,
        coins: u32,
    },
    PointsPerWonderStage {
        category: PointCategoryDto,
        directions: Vec<EffectDirectionDto>,
        points: i32,
    },
    PointsPerDefeat {
        category: PointCategoryDto,
        directions: Vec<EffectDirectionDto>,
        points: i32,
    },
    PlayLastCard,
    BuildFreeOncePerAge,
    CopyGuild,
    BuildFromDiscard,
}

impl From<EffectSpec> for EffectSpecDto {
    fn from(spec: EffectSpec) -> Self {
        match spec {
            EffectSpec::Produces {
                resources,
                choice,
                tradable,
            } => Self::Produces {
                resources: convert(resources),
                choice,
                tradable,
            },
            EffectSpec::Points { category, points } => Self::Points {
                category: category.into(),
                points,
            },
            EffectSpec::Military(n) => Self::Military(n),
            EffectSpec::Science { symbols, choice } => Self::Science {
                symbols: convert(symbols),
                choice,
            },
            EffectSpec::Coins(n) => Self::Coins(n),
            EffectSpec::TradeDiscount {
                directions,
                resources,
            } => Self::TradeDiscount {
                directions: convert(directions),
                resources: convert(resources),
            },
            EffectSpec::CoinsPerCard {
                directions,
                categories,
                coins,
            } => Self::CoinsPerCard {
                directions: convert(directions),
                categories: convert(categories),
                coins,
            },
            EffectSpec::PointsPerCard {
                category,
                directions,
                categories,
                points,
            } => Self::PointsPerCard {
                category: category.into(),
                directions: convert(directions),
                categories: convert(categories),
                points,
            },
            EffectSpec::CoinsPerWonderStage { directions, coins } => Self::CoinsPerWonderStage {
                directions: convert(directions),
                coins,
            },
            EffectSpec::PointsPerWonderStage {
                category,
                directions,
                points,
            } => Self::PointsPerWonderStage {
                category: category.into(),
                directions: convert(directions),
                points,
            },
            EffectSpec::PointsPerDefeat {
                category,
                directions,
                points,
            } => Self::PointsPerDefeat {
                category: category.into(),
                directions: convert(directions),
                points,
            },
            EffectSpec::PlayLastCard => Self::PlayLastCard,
            EffectSpec::BuildFreeOncePerAge => Self::BuildFreeOncePerAge,
            EffectSpec::CopyGuild => Self::CopyGuild,
            EffectSpec::BuildFromDiscard => Self::BuildFromDiscard,
        }
    }
}

/// `%{coins: 1, resources: [{:wood, 2}]}`
#[derive(Debug, Clone, PartialEq, Eq, NifMap)]
pub struct CostInfoDto {
    pub coins: u32,
    pub resources: Vec<(ResourceDto, u8)>,
}

impl From<CostInfo> for CostInfoDto {
    fn from(cost: CostInfo) -> Self {
        Self {
            coins: cost.coins,
            resources: cost
                .resources
                .into_iter()
                .map(|(r, n)| (r.into(), n))
                .collect(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, NifMap)]
pub struct StageInfoDto {
    pub cost: CostInfoDto,
    pub effects: Vec<EffectSpecDto>,
}

impl From<StageInfo> for StageInfoDto {
    fn from(stage: StageInfo) -> Self {
        Self {
            cost: stage.cost.into(),
            effects: convert(stage.effects),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, NifMap)]
pub struct WonderBoardInfoDto {
    pub side: SideDto,
    pub effects: Vec<EffectSpecDto>,
    pub stages: Vec<StageInfoDto>,
}

impl From<WonderBoardInfo> for WonderBoardInfoDto {
    fn from(board: WonderBoardInfo) -> Self {
        Self {
            side: board.side.into(),
            effects: convert(board.effects),
            stages: convert(board.stages),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, NifMap)]
pub struct WonderInfoDto {
    pub name: String,
    pub sides: Vec<SideDto>,
    pub boards: Vec<WonderBoardInfoDto>,
}

impl From<WonderInfo> for WonderInfoDto {
    fn from(wonder: WonderInfo) -> Self {
        Self {
            name: wonder.name,
            sides: wonder.sides.into_iter().map(Into::into).collect(),
            boards: convert(wonder.boards),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, NifMap)]
pub struct CardInfoDto {
    pub name: String,
    pub category: CategoryDto,
    pub age: u8,
    pub cost: CostInfoDto,
    pub chains_from: Vec<String>,
    pub chains_to: Vec<String>,
    pub effects: Vec<EffectSpecDto>,
}

impl From<CardInfo> for CardInfoDto {
    fn from(card: CardInfo) -> Self {
        Self {
            name: card.name,
            category: card.category.into(),
            age: card.age,
            cost: card.cost.into(),
            chains_from: card.chains_from,
            chains_to: card.chains_to,
            effects: convert(card.effects),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, NifUnitEnum)]
pub enum TurnKindDto {
    Regular,
    PlayLastCard,
    BuildFromDiscard,
}

impl From<TurnKind> for TurnKindDto {
    fn from(kind: TurnKind) -> Self {
        match kind {
            TurnKind::Regular => Self::Regular,
            TurnKind::PlayLastCard => Self::PlayLastCard,
            TurnKind::BuildFromDiscard => Self::BuildFromDiscard,
        }
    }
}

/// `{:built, card}` | `{:built_free, card}` | `{:built_from_discard, card}` | `{:wonder_stage, n}` | `:discarded`
#[derive(Debug, Clone, PartialEq, Eq, NifTaggedEnum)]
pub enum PublicActionDto {
    Built(String),
    BuiltFree(String),
    BuiltFromDiscard(String),
    WonderStage(u8),
    Discarded,
}

impl From<PublicAction> for PublicActionDto {
    fn from(action: PublicAction) -> Self {
        match action {
            PublicAction::Built { card } => Self::Built(card),
            PublicAction::BuiltFree { card } => Self::BuiltFree(card),
            PublicAction::BuiltFromDiscard { card } => Self::BuiltFromDiscard(card),
            PublicAction::WonderStage { stage } => Self::WonderStage(stage),
            PublicAction::Discarded => Self::Discarded,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, NifMap)]
pub struct PlayDto {
    pub player: String,
    pub action: PublicActionDto,
    pub coins_to_west: u32,
    pub coins_to_east: u32,
}

impl From<Play> for PlayDto {
    fn from(play: Play) -> Self {
        Self {
            player: play.player,
            action: play.action.into(),
            coins_to_west: play.coins_to_west,
            coins_to_east: play.coins_to_east,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, NifMap)]
pub struct CoinChangeDto {
    pub player: String,
    pub delta: i32,
}

impl From<CoinChange> for CoinChangeDto {
    fn from(change: CoinChange) -> Self {
        Self {
            player: change.player,
            delta: change.delta,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, NifMap)]
pub struct BattleDto {
    pub player: String,
    pub rival: String,
    pub player_strength: u32,
    pub rival_strength: u32,
    pub player_token: Option<i32>,
    pub rival_token: Option<i32>,
}

impl From<Battle> for BattleDto {
    fn from(b: Battle) -> Self {
        Self {
            player: b.player,
            rival: b.rival,
            player_strength: b.player_strength,
            rival_strength: b.rival_strength,
            player_token: b.player_token,
            rival_token: b.rival_token,
        }
    }
}

/// `{:turn, %{age, turn, kind, plays, coins}}` | `{:battles, %{age, battles}}`
#[derive(Debug, Clone, PartialEq, Eq, NifTaggedEnum)]
pub enum HistoryEventDto {
    Turn {
        age: u8,
        turn: u8,
        kind: TurnKindDto,
        plays: Vec<PlayDto>,
        coins: Vec<CoinChangeDto>,
    },
    Battles {
        age: u8,
        battles: Vec<BattleDto>,
    },
}

impl From<HistoryEvent> for HistoryEventDto {
    fn from(event: HistoryEvent) -> Self {
        match event {
            HistoryEvent::Turn {
                age,
                turn,
                kind,
                plays,
                coins,
            } => Self::Turn {
                age,
                turn,
                kind: kind.into(),
                plays: convert(plays),
                coins: convert(coins),
            },
            HistoryEvent::Battles { age, battles } => Self::Battles {
                age,
                battles: convert(battles),
            },
        }
    }
}
