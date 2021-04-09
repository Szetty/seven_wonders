use derive_more::Display;
use std::collections::{HashMap, HashSet};
use std::iter::FromIterator;
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

#[derive(Display, Default)]
pub struct Player(pub PName);

impl Player {
    pub fn name(&self) -> &PName {
        &self.0
    }
}

pub type PName = String;

#[derive(Default)]
pub struct Neighbours {
    data: HashMap<PName, (PName, PName)>,
}

impl Neighbours {
    pub fn new(player_names: Vec<PName>) -> Self {
        let mut data = HashMap::new();
        let count = player_names.len();
        for (idx, player_name) in player_names.iter().enumerate() {
            let west_neighbour = player_names[(count + idx - 1) % count].clone();
            let east_neighbour = player_names[(count + idx + 1) % count].clone();
            data.insert(player_name.clone(), (west_neighbour, east_neighbour));
        }
        Self { data }
    }
    pub fn get_neighbours(&self, player_name: &PName) -> &(PName, PName) {
        self.data.get(player_name).unwrap()
    }
    pub fn get_player_names_from_directions<'a>(
        &self,
        player_name: &PName,
        directions: PlayerDirections<'a>,
    ) -> HashSet<PName> {
        directions
            .iter()
            .map(|direction| self.get_player_name_from_direction(&player_name, direction))
            .collect()
    }
    fn get_player_name_from_direction(
        &self,
        player_name: &PName,
        direction: &PlayerDirection,
    ) -> PName {
        let (east, west) = self.data.get(player_name).unwrap();
        match direction {
            PlayerDirection::East => east.clone(),
            PlayerDirection::West => west.clone(),
            PlayerDirection::Own => player_name.clone(),
        }
    }
}

pub type PlayerDirections<'a> = &'a [PlayerDirection];
#[derive(EnumIter)]
pub enum PlayerDirection {
    East,
    West,
    Own,
}

#[test]
fn test_build_neighbours() {
    let neighbours = Neighbours::new(vec!["a".to_string(), "b".to_string(), "c".to_string()]);
    let data = neighbours.data;
    assert_eq!(*data.get("a").unwrap(), ("c".to_string(), "b".to_string()));
    assert_eq!(*data.get("b").unwrap(), ("a".to_string(), "c".to_string()));
    assert_eq!(*data.get("c").unwrap(), ("b".to_string(), "a".to_string()));
}

#[test]
fn test_get_player_names_from_directions() {
    let players = vec!["a".to_string(), "b".to_string(), "c".to_string()];
    let neighbours = Neighbours::new(players.clone());
    let directions: Vec<PlayerDirection> = PlayerDirection::iter().collect();
    let affected_player_names =
        neighbours.get_player_names_from_directions(&"b".to_string(), &directions);
    assert_eq!(affected_player_names, HashSet::from_iter(players.clone()));
    assert_eq!(
        neighbours.get_player_names_from_directions(&"b".to_string(), &[PlayerDirection::East]),
        HashSet::from_iter(vec!["a".to_string()].into_iter())
    );
    assert_eq!(
        neighbours.get_player_names_from_directions(&"b".to_string(), &[PlayerDirection::West]),
        HashSet::from_iter(vec!["c".to_string()].into_iter())
    );
    assert_eq!(
        neighbours.get_player_names_from_directions(&"b".to_string(), &[PlayerDirection::Own]),
        HashSet::from_iter(vec!["b".to_string()].into_iter())
    );
}
