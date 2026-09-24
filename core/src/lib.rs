//! Seven Wonders game engine (`seven_wonders_core`), loaded into Helios as a Rustler NIF.

pub mod api;
mod common;
pub mod domain;
pub mod engine;

// ---------------------------------------------------------------------------
// NIF layer: native-term DTOs + thin wrappers over `api`. No logic lives here.
// ---------------------------------------------------------------------------

use rustler::{Atom, NifMap, NifUntaggedEnum, ResourceArc};
use std::sync::Mutex;

mod atoms {
    rustler::atoms! {
        invalid_players_number,
        invalid_players_and_wonder_side_length,
        invalid_wonder,
        lock_fail,
        wonder_name,
        side_b,
    }
}

pub struct GameResource(Mutex<domain::GameState>);

#[rustler::resource_impl]
impl rustler::Resource for GameResource {}

#[derive(NifMap)]
struct GameSettings {
    version: String,
    wonders: Vec<String>,
}

impl From<api::GameSettings> for GameSettings {
    fn from(settings: api::GameSettings) -> Self {
        Self {
            version: settings.version,
            wonders: settings.wonders,
        }
    }
}

struct WonderSideChoice {
    wonder_name: String,
    side_b: bool,
}

// Manual `Decode`: any field failure (missing key, wrong type) returns
// `Error::BadArg`, so malformed Elixir input raises `ArgumentError`.
// The `NifMap` derive would rewrap those failures as `Error::RaiseTerm`,
// which the BEAM surfaces as `ErlangError` instead.
impl<'a> rustler::Decoder<'a> for WonderSideChoice {
    fn decode(term: rustler::Term<'a>) -> rustler::NifResult<Self> {
        fn decode_field<'a, T>(term: rustler::Term<'a>, key: rustler::Atom) -> rustler::NifResult<T>
        where
            T: rustler::Decoder<'a>,
        {
            let value = term.map_get(key).map_err(|_| rustler::Error::BadArg)?;
            T::decode(value).map_err(|_| rustler::Error::BadArg)
        }

        Ok(Self {
            wonder_name: decode_field(term, atoms::wonder_name())?,
            side_b: decode_field(term, atoms::side_b())?,
        })
    }
}

impl From<WonderSideChoice> for api::WonderSideChoice {
    fn from(choice: WonderSideChoice) -> Self {
        Self {
            wonder_name: choice.wonder_name,
            side_b: choice.side_b,
        }
    }
}

/// Second element of `{:error, {reason, detail}}`: an integer or a string.
#[derive(NifUntaggedEnum)]
enum ErrorDetail {
    Count(usize),
    Text(String),
}

fn nif_error(error_type: api::ErrorType) -> (Atom, ErrorDetail) {
    match error_type {
        api::ErrorType::InvalidPlayersNumber(count) => {
            (atoms::invalid_players_number(), ErrorDetail::Count(count))
        }
        api::ErrorType::InvalidPlayersAndWonderSideLength(message) => (
            atoms::invalid_players_and_wonder_side_length(),
            ErrorDetail::Text(message),
        ),
        api::ErrorType::InvalidWonder(wonder_name) => {
            (atoms::invalid_wonder(), ErrorDetail::Text(wonder_name))
        }
    }
}

#[rustler::nif]
fn game_settings() -> GameSettings {
    api::game_settings().into()
}

#[rustler::nif]
fn start_game(
    players: Vec<String>,
    wonder_sides: Vec<WonderSideChoice>,
) -> Result<ResourceArc<GameResource>, (Atom, ErrorDetail)> {
    let wonder_sides = wonder_sides.into_iter().map(Into::into).collect();
    api::start_game(players, wonder_sides)
        .map(|game_state| ResourceArc::new(GameResource(game_state)))
        .map_err(nif_error)
}

#[rustler::nif]
fn debug_game(game: ResourceArc<GameResource>) -> Result<String, Atom> {
    let game_state = game.0.try_lock().map_err(|_| atoms::lock_fail())?;
    Ok(serde_json::to_string(&*game_state).expect("GameState serializes to JSON"))
}

rustler::init!("Elixir.Helios.Core.Native");

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
