use super::game_helpers::names;
use crate::engine::rng::GameRng;
use crate::game::{Action, BuildOption, Game, Payment, Phase, PlayerView, WonderSelection};

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
pub fn play_random_game(players: usize, seed: u64, mut on_step: impl FnMut(&Game)) -> Game {
    let mut game = Game::new(names(players), WonderSelection::Random, seed).unwrap();
    let mut chooser = GameRng::new(seed.wrapping_mul(31).wrapping_add(17));
    for _ in 0..1_000 {
        on_step(&game);
        let acting = acting_players(&game);
        if acting.is_empty() {
            return game;
        }
        assert_invariants(&game);
        for player in acting {
            let view = game.view(&player).unwrap();
            let actions = available_actions(&view);
            assert!(!actions.is_empty(), "{player} has no legal action");
            assert_view_agrees_with_submit(&game, &player, &view, &actions);
            let choice = actions[chooser.below(actions.len())].clone();
            game.submit(&player, choice).unwrap();
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
