//! `seven_wonders_core`: the 7 Wonders base-game engine (`game`) with a
//! Rustler NIF boundary (`nif`) loaded by Helios as `Helios.Core.Native`.
mod domain;
mod engine;
pub mod game;
mod nif;

#[cfg(test)]
mod tests {
    pub mod data;
    pub mod deck;
    pub mod game_ages;
    pub mod game_effects;
    pub mod game_extra_turns;
    pub mod game_helpers;
    pub mod game_legality;
    pub mod game_olympia;
    pub mod game_payment;
    pub mod game_resolution;
    pub mod game_scoring;
    pub mod game_settings;
    pub mod game_setup;
    pub mod game_simulation;
    pub mod game_view;
    pub mod helpers;
    pub mod nif_dto;
    pub mod points;
    pub mod production;
    pub mod rng;
}

rustler::init!("Elixir.Helios.Core.Native");
