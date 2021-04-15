use std::collections::HashMap;
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

pub type Point = i16;
#[derive(PartialEq, Eq, Hash, Clone, Debug, EnumIter, serde::Serialize)]
pub enum PointCategory {
    MilitaryP,
    TreasuryP,
    WonderP,
    CivilianP,
    ScientificP,
    CommercialP,
    GuildP,
}
#[rustfmt::skip]
pub fn all_point_categories() -> impl Iterator<Item = PointCategory> { PointCategory::iter() }

#[derive(Debug)]
pub struct PointsMap {
    data: HashMap<PointCategory, Point>,
}

impl PointsMap {
    pub fn new() -> Self {
        let mut data: HashMap<PointCategory, Point> = Default::default();
        for category in all_point_categories() {
            data.insert(category, 0);
        }
        Self { data: data }
    }
    pub fn add(&mut self, key: &PointCategory, value: Point) {
        *(self.data.get_mut(&key).unwrap()) += value;
    }
    pub fn to_hash_map(&self) -> HashMap<PointCategory, Point> {
        self.data.clone()
    }
}
