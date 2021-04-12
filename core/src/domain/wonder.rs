use super::effects::Effects;
use super::supply::ResourceCosts;
use derive_more::Display;
use std::fmt;

#[derive(Display)]
#[display(fmt = "Wonder({}, {}, {})", _0, _1, _2)]
pub struct Wonder<'a, T>(pub &'a str, pub WonderSide<'a, T>, pub WonderSide<'a, T>);

impl<'a, T> Wonder<'a, T> {
    pub fn get_wonder_side_a(&self) -> &WonderSide<'a, T> {
        &self.1
    }
    pub fn get_wonder_side_b(&self) -> &WonderSide<'a, T> {
        &self.2
    }
}

pub struct WonderSide<'a, T>(pub &'a str, pub Effects<T>, pub WonderStages<'a, T>);

impl<'a, T> WonderSide<'a, T> {
    pub fn initial_effects(&self) -> &Effects<T> {
        &self.1
    }
    // use indexing from 1
    pub fn wonder_stage_with_idx(&self, idx: usize) -> &WonderStage<'a, T> {
        &self.2[idx - 1]
    }
}

impl<'a, T> fmt::Debug for WonderSide<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("WonderSide").field("name", &self.0).finish()
    }
}

pub type WonderStages<'a, T> = Vec<WonderStage<'a, T>>;

pub struct WonderStage<'a, T>(pub ResourceCosts<'a>, pub Effects<T>);

impl<'a, T> WonderStage<'a, T> {
    pub fn effects(&self) -> &Effects<T> {
        &self.1
    }
}
