//! Rustler boundary. Decodes Elixir terms into `crate::game` types, calls the
//! engine, encodes the result. Calls are serialized per game by the mutex
//! (and by the GameServer); each takes well under 1 ms, so no dirty scheduler.
pub(crate) mod dto;

use crate::game::{self, Game};
use dto::{ActionDto, ActionErrorDto, GameSettingsDto, PlayerViewDto, SetupErrorDto, WondersDto};
use rustler::{Atom, Encoder, Env, ResourceArc, Term};
use std::sync::Mutex;

mod atoms {
    rustler::atoms! { ok, error, lock_fail }
}

pub struct GameResource(pub Mutex<Game>);

#[rustler::resource_impl]
impl rustler::Resource for GameResource {}

#[rustler::nif]
fn game_settings() -> GameSettingsDto {
    game::settings().into()
}

#[rustler::nif]
fn new_game(
    players: Vec<String>,
    wonders: WondersDto,
    seed: u64,
) -> Result<ResourceArc<GameResource>, SetupErrorDto> {
    Game::new(players, wonders.into(), seed)
        .map(|game| ResourceArc::new(GameResource(Mutex::new(game))))
        .map_err(SetupErrorDto::from)
}

/// `:ok` | `{:error, reason}`
#[rustler::nif]
fn submit<'a>(
    env: Env<'a>,
    resource: ResourceArc<GameResource>,
    player: String,
    action: ActionDto,
) -> Term<'a> {
    let Ok(mut game) = resource.0.lock() else {
        return (atoms::error(), atoms::lock_fail()).encode(env);
    };
    match game.submit(&player, action.into()) {
        Ok(()) => atoms::ok().encode(env),
        Err(error) => (atoms::error(), ActionErrorDto::from(error)).encode(env),
    }
}

/// `{:ok, view}` | `{:error, reason}`
#[rustler::nif]
fn view<'a>(env: Env<'a>, resource: ResourceArc<GameResource>, player: String) -> Term<'a> {
    let Ok(game) = resource.0.lock() else {
        return (atoms::error(), atoms::lock_fail()).encode(env);
    };
    match game.view(&player) {
        Ok(view) => (atoms::ok(), PlayerViewDto::from(view)).encode(env),
        Err(error) => (atoms::error(), ActionErrorDto::from(error)).encode(env),
    }
}

#[rustler::nif]
fn debug_game(resource: ResourceArc<GameResource>) -> Result<String, Atom> {
    let game = resource.0.lock().map_err(|_| atoms::lock_fail())?;
    Ok(game.debug_json())
}
