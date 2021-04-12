use super::effects::Effects;
use super::supply::{Coin, ResourceCosts};
use derive_more::Display;
use std::fmt;

#[derive(Display)]
#[display(fmt = "Structure({})", _0)]
pub struct Structure<'a, T>(
    pub SName<'a>,
    pub Category,
    pub Age,
    pub Effects<T>,
    pub Dependencies<'a>,
    pub Dependents<'a>,
    pub Cost<'a>,
    pub PlayerThresholds<'a>,
);

impl<'a, T> Structure<'a, T> {
    pub fn name(&self) -> SName {
        self.0
    }
    pub fn category(&self) -> Category {
        self.1.clone()
    }
    pub fn effects(&self) -> &Effects<T> {
        &self.3
    }
    pub fn dependencies(&self) -> Dependencies {
        self.4
    }
    pub fn thresholds(&self) -> PlayerThresholds<'a> {
        self.7
    }
}

impl<'a, T> fmt::Debug for Structure<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Structure").field("name", &self.0).finish()
    }
}

impl<'a, T> PartialEq for Structure<'a, T> {
    fn eq(&self, other: &Structure<'a, T>) -> bool {
        self.0 == other.0
    }
}

pub type SName<'a> = &'a str;

pub type Categories<'a> = &'a [Category];

#[derive(Display, PartialEq, Eq, Hash, Clone, Debug)]
pub enum Category {
    Civilian,
    Commercial,
    Guild,
    MG, // Manufactured Good
    Military,
    RM, // Raw Material
    Scientific,
}

#[derive(Display, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Age {
    None,
    I,
    II,
    III,
}

impl Age {
    pub fn get_current_and_future_ages(&self) -> Vec<Age> {
        match self {
            Self::None => vec![Self::I, Self::II, Self::III],
            Self::I => vec![Self::I, Self::II, Self::III],
            Self::II => vec![Self::II, Self::III],
            Self::III => vec![Self::III],
        }
    }
}

impl Default for Age {
    fn default() -> Self {
        Self::None
    }
}

pub type Dependencies<'a> = &'a [SName<'a>];
pub type Dependents<'a> = &'a [SName<'a>];
pub type Cost<'a> = (Coin, ResourceCosts<'a>);
pub type PlayerThresholds<'a> = &'a [u8];
