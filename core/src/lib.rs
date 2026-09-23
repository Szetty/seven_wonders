//! Seven Wonders game engine (`seven_wonders_core`), loaded into Helios as a Rustler NIF.

pub mod api;
mod common;
pub mod domain;
pub mod engine;

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
