use super::effects::Effects;
use super::supply::ResourceCosts;
use derive_more::Display;

#[derive(Display)]
#[display(fmt = "Wonder({}, {}, {})", _0, _1, _2)]
pub struct Wonder<'a, T>(pub &'a str, pub WonderSide<'a, T>, pub WonderSide<'a, T>);

pub struct WonderSide<'a, T>(pub &'a str, pub Effects<T>, pub WonderStages<'a, T>);

pub type WonderStages<'a, T> = Vec<WonderStage<'a, T>>;

pub struct WonderStage<'a, T>(pub ResourceCosts<'a>, pub Effects<T>);
