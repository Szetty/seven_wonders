use super::game_helpers::names;
use crate::engine::rng::GameRng;
use crate::game::{
    Action, BuildOption, Game, Payment, Phase, PlayerView, SetupMode, SetupView, Side,
    WonderSelection,
};

fn push_options(out: &mut Vec<Action>, option: &BuildOption, make: impl Fn(Payment) -> Action) {
    match option {
        BuildOption::Unavailable { .. } => {}
        BuildOption::Free | BuildOption::Chain(_) | BuildOption::Coins(_) => {
            out.push(make(Payment::default()))
        }
        BuildOption::Trade(options) => {
            out.extend(options.iter().map(|option| make(option.payment.clone())))
        }
    }
}

/// Every action the view offers.
pub fn available_actions(view: &PlayerView) -> Vec<Action> {
    if let Some(setup) = &view.setup {
        return setup_actions(view, setup);
    }
    let mut actions = Vec::new();
    if let Some(pile) = &view.discard_pile {
        actions.extend(
            pile.iter()
                .map(|card| Action::BuildFromDiscard { card: card.clone() }),
        );
    }
    for card in &view.hand {
        push_options(&mut actions, &card.build, |payment| Action::Build {
            card: card.name.clone(),
            payment,
        });
        push_options(&mut actions, &card.wonder_stage, |payment| {
            Action::BuildWonderStage {
                card: card.name.clone(),
                payment,
            }
        });
        if card.free_build {
            actions.push(Action::BuildFree {
                card: card.name.clone(),
            });
        }
        actions.push(Action::Discard {
            card: card.name.clone(),
        });
    }
    actions
}

/// Claims of every board the viewer may take (free boards plus their own
/// claim for `ChooseBoard`; the drawn board for `ChooseSide`) on both sides,
/// plus `ConfirmWonder` once they hold a claim.
fn setup_actions(view: &PlayerView, setup: &SetupView) -> Vec<Action> {
    let mine = setup
        .seats
        .iter()
        .find(|seat| seat.player == view.me)
        .expect("the viewer has a seat");
    if mine.confirmed {
        return Vec::new();
    }
    let boards: Vec<String> = match setup.mode {
        SetupMode::ChooseBoard => setup
            .available
            .iter()
            .cloned()
            .chain(mine.wonder.clone())
            .collect(),
        SetupMode::ChooseSide => mine.wonder.iter().cloned().collect(),
    };
    let mut actions: Vec<Action> = boards
        .into_iter()
        .flat_map(|wonder| {
            [Side::A, Side::B].map(|side| Action::ClaimWonder {
                wonder: wonder.clone(),
                side,
            })
        })
        .collect();
    if mine.side.is_some() {
        actions.push(Action::ConfirmWonder);
    }
    actions
}

fn acting_players(game: &Game) -> Vec<String> {
    match game.phase() {
        Phase::Setup => game
            .view(&game.players()[0])
            .unwrap()
            .setup
            .expect("setup view during Phase::Setup")
            .seats
            .into_iter()
            .filter(|seat| !seat.confirmed)
            .map(|seat| seat.player)
            .collect(),
        Phase::ChoosingCards { .. } => game.players().to_vec(),
        Phase::ExtraTurn { player, .. } => vec![player.clone()],
        Phase::GameOver { .. } => Vec::new(),
    }
}

fn cards_on_table(view: &PlayerView) -> usize {
    view.players
        .iter()
        .map(|p| p.built.len() + usize::from(p.stages_built))
        .sum::<usize>()
        + view.discard_count
}

fn assert_invariants(game: &Game) {
    let players = game.players().len();
    let view = game.view(&game.players()[0]).unwrap();
    if let Some(setup) = &view.setup {
        let mut held: Vec<&String> = setup
            .seats
            .iter()
            .filter_map(|seat| seat.wonder.as_ref())
            .collect();
        let count = held.len();
        held.sort();
        held.dedup();
        assert_eq!(held.len(), count, "a board is held twice");
        // During setup no board, hand, discard or coin data is public yet.
        for player in game.players() {
            let seen = game.view(player).unwrap();
            assert!(seen.setup.is_some(), "{player} sees the setup view");
            assert!(seen.players.is_empty(), "setup view lists player boards");
            assert!(seen.hand.is_empty(), "setup view shows a hand");
            assert!(
                seen.discard_pile.is_none(),
                "setup view shows a discard pile"
            );
            assert_eq!(seen.discard_count, 0, "setup view counts discards");
            assert!(
                seen.my_pending.is_none(),
                "setup view shows a pending action"
            );
            assert!(seen.scores.is_none(), "setup view shows scores");
        }
        // The engine's debug output must not read the placeholder state.
        assert!(game.debug_json().contains("\"state\":null"));
        return;
    }
    let in_hands: usize = game
        .players()
        .iter()
        .map(|p| game.hand_names(p).len())
        .sum();
    assert_eq!(
        cards_on_table(&view) + in_hands,
        7 * players * usize::from(view.phase.age),
        "card conservation"
    );
    if let Phase::ChoosingCards { turn, .. } = game.phase() {
        for player in game.players() {
            assert_eq!(game.hand_names(player).len(), 8 - usize::from(*turn));
        }
    }
}

fn assert_view_agrees_with_submit(
    game: &Game,
    player: &str,
    view: &PlayerView,
    actions: &[Action],
) {
    for action in actions {
        assert_eq!(
            game.validate(player, action),
            Ok(()),
            "view offered {action:?} to {player}"
        );
    }
    for card in &view.hand {
        if let BuildOption::Unavailable { reason } = &card.build {
            let action = Action::Build {
                card: card.name.clone(),
                payment: Payment::default(),
            };
            assert_eq!(game.validate(player, &action), Err(*reason));
        }
        if let BuildOption::Unavailable { reason } = &card.wonder_stage {
            let action = Action::BuildWonderStage {
                card: card.name.clone(),
                payment: Payment::default(),
            };
            assert_eq!(game.validate(player, &action), Err(*reason));
        }
    }
}

/// Plays a full game choosing uniformly among the view's options.
/// `on_step` sees the game before every round of submissions.
/// Coins are `u32`: a negative balance would panic (debug overflow check).
pub fn play_random_game(players: usize, seed: u64, on_step: impl FnMut(&Game)) -> Game {
    play_random_game_with(players, WonderSelection::Random, seed, on_step).0
}

/// As `play_random_game` for any wonder selection; also returns the accepted
/// submissions in order (the action log Helios persists and replays).
pub fn play_random_game_with(
    players: usize,
    selection: WonderSelection,
    seed: u64,
    mut on_step: impl FnMut(&Game),
) -> (Game, Vec<(String, Action)>) {
    let mut game = Game::new(names(players), selection, seed).unwrap();
    let mut chooser = GameRng::new(seed.wrapping_mul(31).wrapping_add(17));
    let mut log = Vec::new();
    for _ in 0..1_000 {
        on_step(&game);
        let acting = acting_players(&game);
        if acting.is_empty() {
            return (game, log);
        }
        assert_invariants(&game);
        for player in acting {
            let view = game.view(&player).unwrap();
            let actions = available_actions(&view);
            assert!(!actions.is_empty(), "{player} has no legal action");
            assert_view_agrees_with_submit(&game, &player, &view, &actions);
            let choice = actions[chooser.below(actions.len())].clone();
            game.submit(&player, choice.clone()).unwrap();
            log.push((player, choice));
        }
    }
    panic!("game did not finish within 1000 rounds");
}

#[test]
fn random_games_finish_for_every_player_count() {
    for players in 3..=7 {
        for seed in 0..20 {
            let game = play_random_game(players, seed, |_| {});
            let Phase::GameOver { scores } = game.phase() else {
                panic!("game {players}/{seed} did not end");
            };
            assert_eq!(scores.len(), players);
            let mut ranked: Vec<String> = scores.iter().map(|s| s.player.clone()).collect();
            ranked.sort();
            let mut expected = names(players);
            expected.sort();
            assert_eq!(ranked, expected);
            assert!(scores.iter().any(|s| s.rank == 1));
            assert!(scores
                .iter()
                .all(|s| s.rank >= 1 && usize::from(s.rank) <= players));
            let view = game.view("p1").unwrap();
            assert_eq!(
                cards_on_table(&view),
                21 * players,
                "card conservation at the end"
            );
        }
    }
}

#[test]
fn identical_inputs_produce_identical_views() {
    let record = |seed: u64| -> Vec<PlayerView> {
        let mut views = Vec::new();
        play_random_game(5, seed, |game| {
            for player in game.players() {
                views.push(game.view(player).unwrap());
            }
        });
        views
    };
    assert_eq!(record(3), record(3));
    assert_ne!(record(3), record(4));
}

use crate::game::{HistoryEvent, PublicAction};
use std::collections::HashMap;

#[test]
fn history_agrees_with_the_final_table() {
    for players in [3, 5, 7] {
        for seed in 0..5 {
            let game = play_random_game(players, seed, |_| {});
            let mut coins: HashMap<String, i32> = HashMap::new();
            let mut built: HashMap<String, Vec<String>> = HashMap::new();
            let mut tokens: HashMap<String, Vec<i32>> = HashMap::new();
            for event in game.history() {
                match event {
                    HistoryEvent::Turn {
                        plays,
                        coins: changes,
                        ..
                    } => {
                        for change in changes {
                            *coins.entry(change.player.clone()).or_insert(3) += change.delta;
                        }
                        for play in plays {
                            if let PublicAction::Built { card }
                            | PublicAction::BuiltFree { card }
                            | PublicAction::BuiltFromDiscard { card } = &play.action
                            {
                                built
                                    .entry(play.player.clone())
                                    .or_default()
                                    .push(card.clone());
                            }
                        }
                    }
                    HistoryEvent::Battles { battles, .. } => {
                        for battle in battles {
                            if let Some(t) = battle.player_token {
                                tokens.entry(battle.player.clone()).or_default().push(t);
                            }
                            if let Some(t) = battle.rival_token {
                                tokens.entry(battle.rival.clone()).or_default().push(t);
                            }
                        }
                    }
                }
            }
            for player in game.players() {
                assert_eq!(
                    coins.get(player).copied().unwrap_or(3),
                    game.coins(player) as i32,
                    "coins {player} {players}/{seed}"
                );
                assert_eq!(
                    built.get(player).cloned().unwrap_or_default(),
                    game.built_names(player),
                    "built {player}"
                );
                let mut expected = game.tokens(player);
                let mut got = tokens.get(player).cloned().unwrap_or_default();
                expected.sort();
                got.sort();
                assert_eq!(got, expected, "tokens {player}");
            }
        }
    }
}

#[test]
fn identical_inputs_produce_identical_history() {
    let a = play_random_game(5, 3, |_| {});
    let b = play_random_game(5, 3, |_| {});
    assert_eq!(a.history(), b.history());
}

fn replay(players: usize, selection: WonderSelection, seed: u64, log: &[(String, Action)]) -> Game {
    let mut game = Game::new(names(players), selection, seed).unwrap();
    for (player, action) in log {
        game.submit(player, action.clone())
            .expect("a logged action replays");
    }
    game
}

fn final_views(game: &Game) -> Vec<PlayerView> {
    game.players()
        .iter()
        .map(|player| game.view(player).unwrap())
        .collect()
}

#[test]
fn setup_mode_games_finish_and_replay_identically() {
    for selection in [WonderSelection::ChooseBoard, WonderSelection::ChooseSide] {
        for players in 3..=7 {
            for seed in 0..5 {
                let (game, log) = play_random_game_with(players, selection.clone(), seed, |_| {});
                assert!(
                    matches!(game.phase(), Phase::GameOver { .. }),
                    "{selection:?} {players}/{seed} did not end"
                );
                assert!(log
                    .iter()
                    .any(|(_, action)| *action == Action::ConfirmWonder));
                assert!(
                    matches!(
                        game.history().first(),
                        Some(HistoryEvent::Turn {
                            age: 1,
                            turn: 1,
                            ..
                        })
                    ),
                    "setup must not add history"
                );
                let mut boards: Vec<String> = game
                    .players()
                    .iter()
                    .map(|player| game.wonder_of(player).0)
                    .collect();
                boards.sort();
                boards.dedup();
                assert_eq!(boards.len(), players, "a board was handed out twice");

                let replayed = replay(players, selection.clone(), seed, &log);
                assert_eq!(final_views(&replayed), final_views(&game));
                assert_eq!(replayed.history(), game.history());
            }
        }
    }
}

#[test]
fn a_choose_board_game_is_deterministic() {
    let record = |seed: u64| -> Vec<PlayerView> {
        let mut views = Vec::new();
        play_random_game_with(5, WonderSelection::ChooseBoard, seed, |game| {
            for player in game.players() {
                views.push(game.view(player).unwrap());
            }
        });
        views
    };
    assert_eq!(record(3), record(3));
    assert_ne!(record(3), record(4));
}
