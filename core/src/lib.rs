#[macro_use]
extern crate rustler;

use protobuf::Message;
use rustler::{resource::ResourceArc, Binary, Encoder, Env, Error, Term};

mod api;
mod common;
mod core;
mod domain;

#[cfg(test)]
mod tests {
    pub mod api;
    pub mod deck;
    pub mod game_effects;
    pub mod helpers;
    pub mod points;
    pub mod resources;
    pub mod trading;
}

mod atoms {
    rustler_atoms! {
        atom ok;
        atom error;
        atom __true__ = "true";
        atom __false__ = "false";
        atom invalid_players_number;
        atom invalid_players_and_wonder_side_length;
        atom invalid_wonder;
        atom bad_reference;
        atom lock_fail;
    }
}
use crate::atoms::*;

static VERSION: &str = env!("CARGO_PKG_VERSION");

rustler::rustler_export_nifs! {
    "Elixir.Core.Api",
    [
        ("ping", 1, ping),
        ("game_settings", 0, game_settings),
        ("start_game", 1, start_game),
        ("debug_game", 1, debug_game)
    ],
    Some(load)
}

pub struct GameStateWrapper(api::SafeGameState);

fn load(env: Env, _info: Term) -> bool {
    resource_struct_init!(GameStateWrapper, env);
    true
}

type GameStateReference = ResourceArc<GameStateWrapper>;

pub fn ping<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let data: Binary = args[0].decode()?;
    let ping_request = api::ping::PingRequest::parse_from_bytes(&data).unwrap();
    let ping_reply: api::ping::PingReply = api::ping(ping_request);
    Ok((ok(), ping_reply.write_to_bytes().unwrap()).encode(env))
}

pub fn game_settings<'a>(env: Env<'a>, _args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let reply: api::game_settings::Reply = api::game_settings();
    Ok((ok(), reply.write_to_bytes().unwrap()).encode(env))
}

pub fn start_game<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let data: Binary = args[0].decode()?;
    let request = api::start_game::Request::parse_from_bytes(&data).unwrap();
    let result = match api::start_game(request) {
        Ok(safe_game_state) => {
            (ok(), ResourceArc::new(GameStateWrapper(safe_game_state))).encode(env)
        }
        Err(error_type) => handle_error_type(env, error_type),
    };
    Ok(result)
}

pub fn debug_game<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let resource: GameStateReference = match args[0].decode() {
        Err(_) => return Ok((error(), bad_reference()).encode(env)),
        Ok(r) => r,
    };

    let game_state = match resource.0.try_lock() {
        Err(_) => return Ok((error(), lock_fail()).encode(env)),
        Ok(guard) => guard,
    };

    Ok((ok(), serde_json::to_string(&*game_state).unwrap()).encode(env))
}

fn handle_error_type(env: Env, error_type: api::ErrorType) -> Term {
    match error_type {
        api::ErrorType::InvalidPlayersNumber(number) => {
            (error(), (invalid_players_number(), number)).encode(env)
        }
        api::ErrorType::InvalidPlayersAndWonderSideLength(msg) => {
            (error(), (invalid_players_and_wonder_side_length(), msg)).encode(env)
        }
        api::ErrorType::InvalidWonder(wonder_name) => {
            (error(), (invalid_wonder(), wonder_name)).encode(env)
        }
    }
}
