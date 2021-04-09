use super::types::ResourceType::*;
use super::types::*;

pub fn assert_combination_of_three(
    f: fn(
        any_resources: Vec<Vec<ResourceType>>,
        resource_costs: Vec<ResourceCost>,
    ) -> Vec<Combination>,
) {
    assert_eq!(
        f(
            vec![vec![Ore], vec![Loom], vec![Stone]],
            vec![ResourceCost(Wood, 2), ResourceCost(Papyrus, 1),]
        ),
        vec![
            (
                vec![vec![Ore], vec![Loom], vec![Stone]],
                vec![Wood, Wood, Papyrus]
            ),
            (
                vec![vec![Ore], vec![Loom], vec![Stone]],
                vec![Wood, Papyrus, Wood]
            ),
            (
                vec![vec![Ore], vec![Loom], vec![Stone]],
                vec![Papyrus, Wood, Wood]
            ),
            (
                vec![vec![Ore], vec![Stone], vec![Loom]],
                vec![Wood, Wood, Papyrus]
            ),
            (
                vec![vec![Ore], vec![Stone], vec![Loom]],
                vec![Wood, Papyrus, Wood]
            ),
            (
                vec![vec![Ore], vec![Stone], vec![Loom]],
                vec![Papyrus, Wood, Wood]
            ),
            (
                vec![vec![Loom], vec![Ore], vec![Stone]],
                vec![Wood, Wood, Papyrus]
            ),
            (
                vec![vec![Loom], vec![Ore], vec![Stone]],
                vec![Wood, Papyrus, Wood]
            ),
            (
                vec![vec![Loom], vec![Ore], vec![Stone]],
                vec![Papyrus, Wood, Wood]
            ),
            (
                vec![vec![Loom], vec![Stone], vec![Ore]],
                vec![Wood, Wood, Papyrus]
            ),
            (
                vec![vec![Loom], vec![Stone], vec![Ore]],
                vec![Wood, Papyrus, Wood]
            ),
            (
                vec![vec![Loom], vec![Stone], vec![Ore]],
                vec![Papyrus, Wood, Wood]
            ),
            (
                vec![vec![Stone], vec![Ore], vec![Loom]],
                vec![Wood, Wood, Papyrus]
            ),
            (
                vec![vec![Stone], vec![Ore], vec![Loom]],
                vec![Wood, Papyrus, Wood]
            ),
            (
                vec![vec![Stone], vec![Ore], vec![Loom]],
                vec![Papyrus, Wood, Wood]
            ),
            (
                vec![vec![Stone], vec![Loom], vec![Ore]],
                vec![Wood, Wood, Papyrus]
            ),
            (
                vec![vec![Stone], vec![Loom], vec![Ore]],
                vec![Wood, Papyrus, Wood]
            ),
            (
                vec![vec![Stone], vec![Loom], vec![Ore]],
                vec![Papyrus, Wood, Wood]
            ),
        ]
    );
}
