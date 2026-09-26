use crate::domain::ResourceType;
use serde::Serialize;

/// Resources bought from each neighbour. Entries for the same resource are
/// summed; zero counts are ignored.
#[derive(Debug, Clone, Default, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub struct Payment {
    pub west: Vec<(ResourceType, u8)>,
    pub east: Vec<(ResourceType, u8)>,
}

impl Payment {
    pub fn is_empty(&self) -> bool {
        self.west
            .iter()
            .chain(&self.east)
            .all(|(_, count)| *count == 0)
    }
}

/// One way to pay for a card, with the coins going to each party.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct PaymentOption {
    pub payment: Payment,
    pub west_coins: u32,
    pub east_coins: u32,
    pub bank_coins: u32,
}
