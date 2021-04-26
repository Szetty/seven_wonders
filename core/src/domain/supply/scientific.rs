use super::super::point::Point;
use derive_more::Display;
use itertools::Itertools;
use serde::ser::{Serialize, SerializeStruct, Serializer};
use std::collections::HashSet;
use std::fmt;

pub type ScientificSymbols<'a> = &'a [ScientificSymbol];

#[derive(Display, Copy, Clone, Debug, PartialEq, serde::Serialize)]
pub enum ScientificSymbol {
    Tablet,
    Compass,
    Gears,
}
use ScientificSymbol::{Compass, Gears, Tablet};

type ScientificSymbolCount = u8;

#[derive(Default)]
pub struct ScientificSymbolsProduced {
    pub actions: Vec<Box<dyn Fn(&Self, &mut ScientificSymbolCounts) + Sync + Send>>,
    pub tablet: ScientificSymbolCount,
    pub compass: ScientificSymbolCount,
    pub gears: ScientificSymbolCount,
    pub any_symbols: Vec<ScientificSymbols<'static>>,
}

pub type ScientificSymbolCounts = [ScientificSymbolCount; 3];

impl ScientificSymbolsProduced {
    pub fn add_all_symbols(&mut self, symbols: ScientificSymbols) {
        for symbol in symbols {
            self.add_symbol(symbol);
        }
    }
    pub fn add_any_symbols(&mut self, symbols: ScientificSymbols<'static>) {
        self.any_symbols.push(symbols);
    }
    pub fn calculate_scientific_points(&self) -> Point {
        let mut symbol_counts = [self.tablet, self.gears, self.compass];
        for action in self.actions.iter() {
            action(self, &mut symbol_counts);
        }
        apply_any_symbols(&mut symbol_counts, &self.any_symbols);
        do_calculate_scientific_points(symbol_counts)
    }
    fn add_symbol(&mut self, symbol: &ScientificSymbol) {
        match symbol {
            Tablet => {
                self.tablet += 1;
            }
            Gears => {
                self.gears += 1;
            }
            Compass => {
                self.compass += 1;
            }
        }
    }
}

impl fmt::Debug for ScientificSymbolsProduced {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ScientificSymbolsProduced")
            .field("tablet", &self.tablet)
            .field("compass", &self.compass)
            .field("gears", &self.gears)
            .field("actions_size", &self.actions.len())
            .field("any_symbols", &self.any_symbols)
            .finish()
    }
}

impl PartialEq for ScientificSymbolsProduced {
    fn eq(&self, other: &Self) -> bool {
        self.actions.len() == other.actions.len()
            && self.any_symbols == other.any_symbols
            && self.tablet == other.tablet
            && self.compass == other.compass
            && self.gears == other.gears
    }
}

impl Serialize for ScientificSymbolsProduced {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut s = serializer.serialize_struct("ScientificSymbolsProduced", 6)?;
        s.serialize_field("type", "ScientificSymbolsProduced")?;
        s.serialize_field("tablet", &self.tablet)?;
        s.serialize_field("compass", &self.compass)?;
        s.serialize_field("gears", &self.gears)?;
        s.serialize_field("actions_size", &self.actions.len())?;
        s.serialize_field("any_symbols", &self.any_symbols)?;
        s.end()
    }
}

fn apply_any_symbols(
    symbol_counts: &mut ScientificSymbolCounts,
    any_symbols: &Vec<ScientificSymbols<'static>>,
) {
    if !any_symbols.is_empty() {
        let symbols_and_points = combination_of_symbols_and_points(symbol_counts, any_symbols);
        let ([tablet, gears, compass], _) = symbols_and_points
            .into_iter()
            .max_by(|(_, points1), (_, points2)| points1.cmp(points2))
            .unwrap();
        symbol_counts[0] = tablet;
        symbol_counts[1] = gears;
        symbol_counts[2] = compass;
    }
}

fn do_calculate_scientific_points(symbol_counts: ScientificSymbolCounts) -> Point {
    identical_symbols_points(symbol_counts) + set_of_different_symbols_points(symbol_counts)
}

fn identical_symbols_points(symbol_counts: ScientificSymbolCounts) -> Point {
    symbol_counts.iter().map(|c| (*c as Point).pow(2)).sum()
}

fn set_of_different_symbols_points(symbol_counts: ScientificSymbolCounts) -> Point {
    symbol_counts.iter().cloned().min().unwrap_or(0) as Point * 7
}

fn combination_of_symbols_and_points(
    symbol_counts: &ScientificSymbolCounts,
    any_symbols: &Vec<ScientificSymbols<'static>>,
) -> HashSet<(ScientificSymbolCounts, Point)> {
    let [tablet, gears, compass] = *symbol_counts;
    any_symbols
        .iter()
        .map(|symbols| symbols.iter())
        .multi_cartesian_product()
        .map(|symbol_product| {
            let final_symbols = symbol_product.iter().fold(
                [tablet, gears, compass],
                |[tablet, gears, compass], symbol| match symbol {
                    Tablet => [tablet + 1, gears, compass],
                    Gears => [tablet, gears + 1, compass],
                    Compass => [tablet, gears, compass + 1],
                },
            );
            (final_symbols, do_calculate_scientific_points(final_symbols))
        })
        .collect()
}

#[cfg(test)]
use maplit::hashset;

#[test]
fn test_identical_symbols_points() {
    assert_eq!(identical_symbols_points([0, 0, 0]), 0);
    assert_eq!(identical_symbols_points([0, 0, 1]), 1);
    assert_eq!(identical_symbols_points([0, 1, 0]), 1);
    assert_eq!(identical_symbols_points([1, 0, 0]), 1);
    assert_eq!(identical_symbols_points([1, 1, 0]), 2);
    assert_eq!(identical_symbols_points([1, 1, 1]), 3);
    assert_eq!(identical_symbols_points([2, 0, 0]), 4);
    assert_eq!(identical_symbols_points([3, 0, 0]), 9);
    assert_eq!(identical_symbols_points([4, 0, 0]), 16);
    assert_eq!(identical_symbols_points([5, 0, 0]), 25);
    assert_eq!(identical_symbols_points([1, 2, 3]), 14);
    assert_eq!(identical_symbols_points([5, 5, 4]), 66);
}

#[test]
fn test_set_of_different_symbols_points() {
    assert_eq!(set_of_different_symbols_points([0, 0, 0]), 0);
    assert_eq!(set_of_different_symbols_points([0, 0, 1]), 0);
    assert_eq!(set_of_different_symbols_points([0, 1, 0]), 0);
    assert_eq!(set_of_different_symbols_points([1, 0, 0]), 0);
    assert_eq!(set_of_different_symbols_points([1, 1, 1]), 7);
    assert_eq!(set_of_different_symbols_points([1, 2, 2]), 7);
    assert_eq!(set_of_different_symbols_points([2, 2, 2]), 14);
    assert_eq!(set_of_different_symbols_points([3, 3, 3]), 21);
    assert_eq!(set_of_different_symbols_points([4, 4, 4]), 28);
    assert_eq!(set_of_different_symbols_points([5, 5, 5]), 35);
}

#[test]
fn test_combination_of_symbols_and_points() {
    assert_eq!(
        combination_of_symbols_and_points(&[0, 0, 0], &vec![&[Tablet, Gears, Compass]]),
        hashset![([1, 0, 0], 1), ([0, 1, 0], 1), ([0, 0, 1], 1),]
    );
    assert_eq!(
        combination_of_symbols_and_points(
            &[0, 0, 0],
            &vec![&[Tablet, Gears, Compass], &[Tablet, Gears, Compass]]
        ),
        hashset![
            ([2, 0, 0], 4),
            ([1, 1, 0], 2),
            ([1, 0, 1], 2),
            ([1, 1, 0], 2),
            ([0, 1, 1], 2),
            ([0, 2, 0], 4),
            ([0, 0, 2], 4),
        ]
    );
    assert_eq!(
        combination_of_symbols_and_points(
            &[3, 2, 2],
            &vec![&[Tablet, Gears, Compass], &[Tablet, Gears, Compass]]
        ),
        hashset![
            ([5, 2, 2], 47),
            ([3, 3, 3], 48),
            ([4, 3, 2], 43),
            ([4, 2, 3], 43),
            ([3, 4, 2], 43),
            ([3, 2, 4], 43),
        ]
    );
    assert_eq!(
        combination_of_symbols_and_points(
            &[4, 3, 3],
            &vec![&[Tablet, Gears, Compass], &[Tablet, Gears, Compass]]
        ),
        hashset![
            ([6, 3, 3], 75),
            ([4, 4, 4], 76),
            ([5, 4, 3], 71),
            ([5, 3, 4], 71),
            ([4, 5, 3], 71),
            ([4, 3, 5], 71),
        ]
    );
}

#[test]
fn test_apply_any_symbols() {
    let mut symbol_counts = [0, 0, 0];
    apply_any_symbols(&mut symbol_counts, &vec![&[Tablet, Gears, Compass]]);
    let mut symbol_counts_vec = symbol_counts.to_vec();
    symbol_counts_vec.sort();
    assert_eq!(symbol_counts_vec, vec![0, 0, 1]);

    symbol_counts = [0, 0, 0];
    apply_any_symbols(
        &mut symbol_counts,
        &vec![&[Tablet, Gears, Compass], &[Tablet, Gears, Compass]],
    );
    symbol_counts_vec = symbol_counts.to_vec();
    symbol_counts_vec.sort();
    assert_eq!(symbol_counts_vec, vec![0, 0, 2]);

    symbol_counts = [3, 2, 2];
    apply_any_symbols(
        &mut symbol_counts,
        &vec![&[Tablet, Gears, Compass], &[Tablet, Gears, Compass]],
    );
    assert_eq!(symbol_counts, [3, 3, 3]);

    symbol_counts = [4, 3, 3];
    apply_any_symbols(
        &mut symbol_counts,
        &vec![&[Tablet, Gears, Compass], &[Tablet, Gears, Compass]],
    );
    assert_eq!(symbol_counts, [4, 4, 4]);

    symbol_counts = [3, 3, 3];
    apply_any_symbols(
        &mut symbol_counts,
        &vec![
            &[Tablet, Gears, Compass],
            &[Tablet, Gears, Compass],
            &[Tablet, Gears, Compass],
        ],
    );
    assert_eq!(symbol_counts, [4, 4, 4]);
}
