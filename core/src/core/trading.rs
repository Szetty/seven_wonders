use crate::domain::{
    diff_resource_costs, Coin, GameState, PName, PlayerState, ResourceCost, ResourceCostOption,
    ResourceCostOptions, ResourceType,
};
use maplit::hashset;
use std::collections::HashSet;

pub type TradingOptions = HashSet<TradingOption>;
pub type TradingOption = Vec<(PName, ResourceType, Coin)>;

pub fn try_trading(
    game_state: &GameState,
    player: &PName,
    resource_cost_options: ResourceCostOptions,
) -> TradingOptions {
    let current = game_state.get_player_state(player);
    let (west, east) = game_state.get_neighbour_player_states(player);
    compute_trading_options((current, west, east), resource_cost_options)
}

type TradingSetup<'a> = (&'a PlayerState, &'a PlayerState, &'a PlayerState);

fn compute_trading_options(
    (current, west, east): TradingSetup,
    resource_cost_options: ResourceCostOptions,
) -> TradingOptions {
    let mut trading_options: TradingOptions = Default::default();
    for resource_cost_option in resource_cost_options {
        trading_options.extend(apply_trading_setup(
            (current, west, east),
            resource_cost_option.clone(),
        ));
        trading_options.extend(apply_trading_setup(
            (current, east, west),
            resource_cost_option.clone(),
        ));
    }
    trading_options
}

fn apply_trading_setup(
    (current, player_state1, player_state2): TradingSetup,
    resource_costs: ResourceCostOption,
) -> TradingOptions {
    let resource_cost_options1 = player_state1.cover_resource_costs(resource_costs.clone());
    if resource_cost_options1.len() == 0 {
        return hashset![build_trade_option(
            current,
            player_state1,
            resource_costs.clone()
        )];
    }
    let mut trading_options: TradingOptions = Default::default();
    for resource_cost_option in resource_cost_options1 {
        let mut trading_option = build_trade_option(
            current,
            player_state1,
            diff_resource_costs(resource_costs.clone(), resource_cost_option.clone()),
        );
        let resource_cost_options2 =
            player_state2.cover_resource_costs(resource_cost_option.clone());
        if resource_cost_options2.len() == 0 {
            trading_option.extend(build_trade_option(
                current,
                player_state2,
                resource_cost_option.clone(),
            ));
            trading_option.sort();
            trading_options.insert(trading_option);
        }
    }
    trading_options
}

fn build_trade_option(
    current: &PlayerState,
    player_to_trade: &PlayerState,
    resource_costs: Vec<ResourceCost>,
) -> TradingOption {
    let mut trading_option: TradingOption = Default::default();
    let player_name_to_trade = player_to_trade.player.name();
    for ResourceCost(resource_type, resource_count) in resource_costs {
        let trading_value = current.apply_trading(player_name_to_trade, &resource_type);
        for _ in 0..resource_count {
            trading_option.push((player_name_to_trade.clone(), resource_type, trading_value))
        }
    }
    trading_option
}
