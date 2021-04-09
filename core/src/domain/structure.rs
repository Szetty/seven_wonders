use super::effects::Effects;
use super::supply::{Coin, ResourceCosts};
use derive_more::Display;

#[derive(Display)]
#[display(fmt = "Structure({})", _0)]
pub struct Structure<'a, T>(
    pub Name<'a>,
    pub Category,
    pub Age,
    pub Effects<T>,
    pub Dependencies<'a>,
    pub Dependents<'a>,
    pub Cost<'a>,
    pub PlayerThresholds<'a>,
);

impl<'a, T> Structure<'a, T> {
    pub fn thresholds(&self) -> PlayerThresholds<'a> {
        self.7
    }
}

pub type Name<'a> = &'a str;

pub type Categories<'a> = &'a [Category];

#[derive(Display, PartialEq)]
pub enum Category {
    Civilian,
    Commercial,
    Guild,
    MG, // Manufactured Good
    Military,
    RM, // Raw Material
    Scientific,
}

#[derive(Display)]
pub enum Age {
    I,
    II,
    III,
}

pub type Dependencies<'a> = &'a [&'a str];
pub type Dependents<'a> = &'a [&'a str];
pub type Cost<'a> = (Coin, ResourceCosts<'a>);
pub type PlayerThresholds<'a> = &'a [u8];
