use crate::domain::{
    all_resources_effect as allre, all_scientific_effect as allse, any_resources_effect as anyre,
    any_scientific_effect as anyse, build_free_from_discarded_cards_effect as bffdce,
    coin_effect as ce, construct_free_effect as cfe, copy_guild_effect as cge,
    dynamic_battle_lost_point_effect as dblpe, dynamic_coin_effect as dce,
    dynamic_point_effect as dpe, dynamic_wonder_coin_effect as dwce,
    dynamic_wonder_point_effect as dwpe, military_effect as me, play_last_card_effect as plce,
    point_effect as pe, trade_effect as te, Age::*, Category::*, Effect, PlayerDirection::*,
    PointCategory::*, ResourceCost as RCost, ResourceType::*, ScientificSymbol::*, Structure,
    Wonder, WonderSide, WonderStage,
};
use lazy_static::lazy_static;
use std::collections::HashMap;

lazy_static! {
    #[rustfmt::skip]
    pub static ref AGE_I_STRUCTURES: [Structure<'static, Effect>; 27] = [
        // Raw Materials (Brown)
        Structure("Lumber Yard", RM, I, vec![allre(&[Wood])],        &[], &[], (0, &[]), &[3, 4]),
        Structure("Stone Pit",   RM, I, vec![allre(&[Stone])],       &[], &[], (0, &[]), &[3, 5]),
        Structure("Clay Pool",   RM, I, vec![allre(&[Clay])],        &[], &[], (0, &[]), &[3, 5]),
        Structure("Ore Vein",    RM, I, vec![allre(&[Ore])],         &[], &[], (0, &[]), &[3, 4]),
        Structure("Tree Farm",   RM, I, vec![anyre(&[Wood, Clay])],  &[], &[], (1, &[]), &[6]),
        Structure("Excavation",  RM, I, vec![anyre(&[Stone, Clay])], &[], &[], (1, &[]), &[4]),
        Structure("Clay Pit",    RM, I, vec![anyre(&[Clay, Ore])],   &[], &[], (1, &[]), &[3]),
        Structure("Timber Yard", RM, I, vec![anyre(&[Stone, Wood])], &[], &[], (1, &[]), &[3]),
        Structure("Forest Cave", RM, I, vec![anyre(&[Wood, Ore])],   &[], &[], (1, &[]), &[5]),
        Structure("Mine",        RM, I, vec![anyre(&[Ore, Stone])],  &[], &[], (1, &[]), &[6]),
        // Manufactured Goods (Gray)
        Structure("Loom"      , MG, I, vec![allre(&[Loom])]   , &[], &[], (0, &[]), &[3, 6]),
        Structure("Glassworks", MG, I, vec![allre(&[Glass])]  , &[], &[], (0, &[]), &[3, 6]),
        Structure("Press"     , MG, I, vec![allre(&[Papyrus])], &[], &[], (0, &[]), &[3, 6]),
        // Civilian (Blue)
        Structure("Pawnshop", Civilian, I, vec![pe(CivilianP, 3)], &[], &[]          , (0, &[]),                &[4, 7]),
        Structure("Baths"   , Civilian, I, vec![allre(&[])], &[], &["Aqueduct"], (0, &[RCost(Stone, 1)]), &[3, 7]),
        Structure("Altar"   , Civilian, I, vec![pe(CivilianP, 2)], &[], &["Temple"]  , (0, &[]),                &[3, 5]),
        Structure("Theater" , Civilian, I, vec![pe(CivilianP, 2)], &[], &["Statue"]  , (0, &[]),                &[3, 6]),
        // Military (Red)
        Structure("Stockade"   , Military, I, vec![me(1)], &[], &[], (0, &[RCost(Wood, 1)]), &[3, 7]),
        Structure("Barracks"   , Military, I, vec![me(1)], &[], &[], (0, &[RCost(Ore, 1)]) , &[3, 5]),
        Structure("Guard tower", Military, I, vec![me(1)], &[], &[], (0, &[RCost(Clay, 1)]), &[3, 4]),
        // Scientific (Green)
        Structure("Apothecary" , Scientific, I, vec![allse(&[Compass])], &[], &["Stables", "Dispensary"]      , (0, &[RCost(Loom, 1)])   , &[3, 5]),
        Structure("Workshop"   , Scientific, I, vec![allse(&[Gears])]  , &[], &["Archery Range", "Laboratory"], (0, &[RCost(Glass, 1)])  , &[3, 7]),
        Structure("Scriptorium", Scientific, I, vec![allse(&[Tablet])] , &[], &["Courthouse", "Library"]      , (0, &[RCost(Papyrus, 1)]), &[3, 4]),
        // Commercial (Yellow)
        Structure("Tavern"           , Commercial, I, vec![ce(5)]                                     , &[], &[]             , (0, &[]), &[4, 5, 7]),
        Structure("East trading post", Commercial, I, vec![te(&[East], &[Wood, Stone, Ore, Clay])]    , &[], &["Forum"]      , (0, &[]), &[3, 7]),
        Structure("West trading post", Commercial, I, vec![te(&[West], &[Wood, Stone, Ore, Clay])]    , &[], &["Forum"]      , (0, &[]), &[3, 7]),
        Structure("Marketplace"      , Commercial, I, vec![te(&[East, West], &[Loom, Glass, Papyrus])], &[], &["Caravansery"], (0, &[]), &[3, 6]),
    ];
    #[rustfmt::skip]
    pub static ref AGE_II_STRUCTURES: [Structure<'static, Effect>; 23] = [
        // Raw Materials (Brown)
        Structure("Sawmill"  , RM, II, vec![allre(&[Wood, Wood])]  , &[], &[], (1, &[]), &[3, 4]),
        Structure("Quarry"   , RM, II, vec![allre(&[Stone, Stone])], &[], &[], (1, &[]), &[3, 4]),
        Structure("Brickyard", RM, II, vec![allre(&[Clay, Clay])]  , &[], &[], (1, &[]), &[3, 4]),
        Structure("Foundry"  , RM, II, vec![allre(&[Ore, Ore])]    , &[], &[], (1, &[]), &[3, 4]),
        // Manufactured Goods (Gray)
        Structure("Loom"      , MG, II, vec![allre(&[Loom])]   , &[], &[], (0, &[]), &[3, 5]),
        Structure("Glassworks", MG, II, vec![allre(&[Glass])]  , &[], &[], (0, &[]), &[3, 5]),
        Structure("Press"     , MG, II, vec![allre(&[Papyrus])], &[], &[], (0, &[]), &[3, 5]),
        // Civilian (Blue)
        Structure("Courthouse", Civilian, II, vec![pe(CivilianP, 4)], &["Scriptorium"], &["Senate"]  , (0, &[RCost(Clay, 2), RCost(Loom, 1)])                 , &[3, 5]),
        Structure("Aqueduct"  , Civilian, II, vec![pe(CivilianP, 5)], &["Baths"]      , &[]          , (0, &[RCost(Stone, 3)])                                , &[3, 7]),
        Structure("Temple"    , Civilian, II, vec![pe(CivilianP, 3)], &["Altar"]      , &["Pantheon"], (0, &[RCost(Wood, 1), RCost(Clay, 1), RCost(Glass, 1)]), &[3, 6]),
        Structure("Statue"    , Civilian, II, vec![pe(CivilianP, 4)], &["Theater"]    , &["Gardens"] , (0, &[RCost(Wood, 1), RCost(Ore, 2)])                  , &[3, 7]),
        // Military (Red)
        Structure("Walls"          , Military, II, vec![me(2)], &[]            , &["Fortifications"], (0, &[RCost(Stone, 3)])                              , &[3, 7]),
        Structure("Training Ground", Military, II, vec![me(2)], &[]            , &["Circus"]        , (0, &[RCost(Wood, 1), RCost(Ore, 2)])                , &[4, 6, 7]),
        Structure("Stables"        , Military, II, vec![me(2)], &["Apothecary"], &[]                , (0, &[RCost(Ore, 1), RCost(Clay, 1), RCost(Wood, 1)]), &[3, 5]),
        Structure("Archery Range"  , Military, II, vec![me(2)], &["Workshop"]  , &[]                , (0, &[RCost(Wood, 2), RCost(Ore, 1)])                , &[3, 6]),
        // Scientific (Green)
        Structure("Dispensary", Scientific, II, vec![allse(&[Compass])], &["Apothecary"] , &["Lodge", "Arena"]               , (0, &[RCost(Ore, 2), RCost(Glass, 1)])   , &[3, 4]),
        Structure("Laboratory", Scientific, II, vec![allse(&[Gears])]  , &["Workshop"]   , &["Siege Workshop", "Observatory"], (0, &[RCost(Clay, 2), RCost(Papyrus, 1)]), &[3, 5]),
        Structure("Library"   , Scientific, II, vec![allse(&[Tablet])] , &["Scriptorium"], &["Senate", "University"]         , (0, &[RCost(Stone, 2), RCost(Loom, 1)])  , &[3, 6]),
        Structure("School"    , Scientific, II, vec![allse(&[Tablet])] , &[]             , &["Academy", "Study"]             , (0, &[RCost(Wood, 1), RCost(Papyrus, 1)]), &[3, 7]),
        // Commercial (Yellow)
        Structure("Forum"      , Commercial, II, vec![anyre(&[Loom, Glass, Papyrus])]   , &["East trading post", "West trading post"], &["Haven"]     , (0, &[RCost(Clay, 2)]), &[3, 6, 7]),
        Structure("Caravansery", Commercial, II, vec![anyre(&[Wood, Stone, Ore, Clay])] , &["Marketplace"]                           , &["Lighthouse"], (0, &[RCost(Wood, 2)]), &[3, 5, 6]),
        Structure("Vineyard"   , Commercial, II, vec![dce(&[East, West, Own], &[RM], 1)], &[]                                        , &[]            , (0, &[])              , &[3, 6]),
        Structure("Bazar"      , Commercial, II, vec![dce(&[East, West, Own], &[MG], 2)], &[]                                        , &[]            , (0, &[])              , &[4, 7]),
    ];
    #[rustfmt::skip]
    pub static ref AGE_III_STRUCTURES: [Structure<'static, Effect>; 18] = [
        // Civilian (Blue)
        Structure("Pantheon" , Civilian, III, vec![pe(CivilianP, 7)], &["Temple"] , &[], (0, &[RCost(Clay, 2), RCost(Ore, 1), RCost(Papyrus, 1), RCost(Loom, 1), RCost(Glass, 1)])                                 , &[3, 6]),
        Structure("Gardens"  , Civilian, III, vec![pe(CivilianP, 5)], &["Statue"] , &[], (0, &[RCost(Clay, 2), RCost(Wood, 1)])                                                                                    , &[3, 4]),
        Structure("Town hall", Civilian, III, vec![pe(CivilianP, 6)], &[]         , &[], (0, &[RCost(Stone, 2), RCost(Ore, 1), RCost(Glass, 1)])                                                                   , &[3, 5, 6]),
        Structure("Palace"   , Civilian, III, vec![pe(CivilianP, 8)], &[]         , &[], (0, &[RCost(Wood, 1), RCost(Stone, 1), RCost(Ore, 1), RCost(Clay, 1), RCost(Loom, 1), RCost(Glass, 1), RCost(Papyrus, 1)]), &[3, 7]),
        Structure("Senate"   , Civilian, III, vec![pe(CivilianP, 6)], &["Library"], &[], (0, &[RCost(Wood, 2), RCost(Ore, 1), RCost(Stone, 1)])                                                                    , &[3, 5]),
        // Military (Red)
        Structure("Fortifications", Military, III, vec![me(3)], &["Walls"]          , &[], (0, &[RCost(Ore, 3), RCost(Stone, 1)])               , &[3, 7]),
        Structure("Circus"        , Military, III, vec![me(3)], &["Training Ground"], &[], (0, &[RCost(Stone, 3), RCost(Ore, 1)])               , &[4, 5, 6]),
        Structure("Arsenal"       , Military, III, vec![me(3)], &[]                 , &[], (0, &[RCost(Ore, 1), RCost(Wood, 2), RCost(Loom, 1)]), &[3, 4, 7]),
        Structure("Siege Workshop", Military, III, vec![me(3)], &["Laboratory"]     , &[], (0, &[RCost(Wood, 1), RCost(Clay, 3)])               , &[3, 5]),
        // Scientific (Green)
        Structure("Lodge"      , Scientific, III, vec![allse(&[Compass])], &["Dispensary"], &[], (0, &[RCost(Clay, 2), RCost(Loom, 1), RCost(Papyrus, 1)]) , &[3, 6]),
        Structure("Observatory", Scientific, III, vec![allse(&[Gears])]  , &["Laboratory"], &[], (0, &[RCost(Ore, 2), RCost(Glass, 1), RCost(Loom, 1)])    , &[3, 7]),
        Structure("University" , Scientific, III, vec![allse(&[Tablet])] , &["Library"]   , &[], (0, &[RCost(Wood, 2), RCost(Papyrus, 1), RCost(Glass, 1)]), &[3, 4]),
        Structure("Academy"    , Scientific, III, vec![allse(&[Compass])], &["School"]    , &[], (0, &[RCost(Stone, 3), RCost(Glass, 1)])                  , &[3, 7]),
        Structure("Study"      , Scientific, III, vec![allse(&[Gears])]  , &["School"]    , &[], (0, &[RCost(Wood, 1), RCost(Papyrus, 1), RCost(Loom, 1)]) , &[3, 5]),
        // Commercial (Yellow)
        Structure("Haven"              , Commercial, III, vec![dce(&[Own], &[RM], 1), dpe(CommercialP, &[Own], &[RM], 1)]                , &["Forum"]      , &[], (0, &[RCost(Ore, 1), RCost(Wood, 1), RCost(Loom, 1)]), &[3, 4]),
        Structure("Lighthouse"         , Commercial, III, vec![dce(&[Own], &[Commercial], 1), dpe(CommercialP, &[Own], &[Commercial], 1)], &["Caravansery"], &[], (0, &[RCost(Stone, 1), RCost(Glass, 1)])             , &[3, 6]),
        Structure("Chamber of commerce", Commercial, III, vec![dce(&[Own], &[MG], 2), dpe(CommercialP, &[Own], &[MG], 2)]                , &[]             , &[], (0, &[RCost(Clay, 2), RCost(Papyrus, 1)])            , &[4, 6]),
        Structure("Arena"              , Commercial, III, vec![dwce(&[Own], 3), dwpe(CommercialP, &[Own], 1)]                            , &["Dispensary"] , &[], (0, &[RCost(Stone, 2), RCost(Ore, 1)])               , &[3, 5, 7]),
    ];
    #[rustfmt::skip]
    pub static ref GUILD_STRUCTURES: [Structure<'static, Effect>; 10] = [
        Structure("Workers Guild"     , Guild, III, vec![dpe(GuildP, &[East, West], &[RM], 1)]        , &[], &[], (0, &[RCost(Ore, 2), RCost(Clay, 1), RCost(Stone, 1), RCost(Wood, 1)]), &[]),
        Structure("Craftsmens Guild"  , Guild, III, vec![dpe(GuildP, &[East, West], &[MG], 2)]        , &[], &[], (0, &[RCost(Ore, 2), RCost(Stone, 2)])                                , &[]),
        Structure("Traders Guild"     , Guild, III, vec![dpe(GuildP, &[East, West], &[Commercial], 1)], &[], &[], (0, &[RCost(Loom, 1), RCost(Papyrus, 1), RCost(Glass, 1)])            , &[]),
        Structure("Philosophers Guild", Guild, III, vec![dpe(GuildP, &[East, West], &[Scientific], 1)], &[], &[], (0, &[RCost(Clay, 3), RCost(Loom, 1), RCost(Papyrus, 1)])             , &[]),
        Structure("Spies Guild"       , Guild, III, vec![dpe(GuildP, &[East, West], &[Military], 1)]  , &[], &[], (0, &[RCost(Clay, 3), RCost(Glass, 1)])                               , &[]),
        Structure("Strategists Guild" , Guild, III, vec![dblpe(GuildP, &[East, West], 1)]             , &[], &[], (0, &[RCost(Ore, 2), RCost(Stone, 1), RCost(Loom, 1)])                , &[]),
        Structure("Shipowners Guild"  , Guild, III, vec![dpe(GuildP, &[Own], &[RM, MG, Guild], 1)]    , &[], &[], (0, &[RCost(Wood, 3), RCost(Papyrus, 1), RCost(Glass, 1)])            , &[]),
        Structure("Scientists Guild"  , Guild, III, vec![anyse(&[Compass, Gears, Tablet])]             , &[], &[], (0, &[RCost(Wood, 2), RCost(Ore, 2), RCost(Papyrus, 1)])              , &[]),
        Structure("Magistrates Guild" , Guild, III, vec![dpe(GuildP, &[East, West], &[Civilian], 2)]  , &[], &[], (0, &[RCost(Wood, 3), RCost(Stone, 1), RCost(Loom, 1)])               , &[]),
        Structure("Builders Guild"    , Guild, III, vec![dwpe(GuildP, &[East, West, Own], 1)]         , &[], &[], (0, &[RCost(Stone, 2), RCost(Clay, 2), RCost(Glass, 1)])              , &[]),
    ];
    pub static ref STRUCTURES_BY_NAME: HashMap<String, &'static Structure<'static, Effect>> = AGE_I_STRUCTURES.iter().chain(AGE_II_STRUCTURES.iter()).chain(AGE_III_STRUCTURES.iter()).chain(GUILD_STRUCTURES.iter()).map(|s| (s.0.to_string(), s)).collect();
    #[rustfmt::skip]
    pub static ref WONDERS: [Wonder<'static, Effect>; 7] = [
        Wonder(
            "Rhódos",
            WonderSide(
                "Rhódos - A",
                vec![allre(&[Ore])],
                vec![
                    WonderStage(&[RCost(Wood, 2)], vec![pe(WonderP, 3)]),
                    WonderStage(&[RCost(Clay, 3)], vec![me(2)]),
                    WonderStage(&[RCost(Ore, 4)], vec![pe(WonderP, 7)])
                ]
            ),
            WonderSide(
                "Rhódos - B",
                vec![allre(&[Ore])],
                vec![
                    WonderStage(&[RCost(Stone, 3)], vec![me(1), pe(WonderP, 3), ce(3)]),
                    WonderStage(&[RCost(Ore, 4)], vec![me(1), pe(WonderP, 4), ce(4)]),
                ]
            )
        ),
        Wonder(
            "Alexandria",
            WonderSide(
                "Alexandria - A",
                vec![allre(&[Glass])],
                vec![
                    WonderStage(&[RCost(Stone, 2)], vec![pe(WonderP, 3)]),
                    WonderStage(&[RCost(Ore, 2)],   vec![anyre(&[Wood, Stone, Ore, Clay])]),
                    WonderStage(&[RCost(Glass, 2)], vec![pe(WonderP, 7)]),
                ]
            ),
            WonderSide(
                "Alexandria - B",
                vec![allre(&[Glass])],
                vec![
                    WonderStage(&[RCost(Clay, 2)], vec![anyre(&[Wood, Stone, Ore, Clay])]),
                    WonderStage(&[RCost(Wood, 2)], vec![anyre(&[Loom, Glass, Papyrus])]),
                    WonderStage(&[RCost(Stone, 3)], vec![pe(WonderP, 7)]),
                ]
            )
        ),
        Wonder(
            "Éphesos",
            WonderSide(
                "Éphesos - A",
                vec![allre(&[Papyrus])],
                vec![
                    WonderStage(&[RCost(Stone, 2)], vec![pe(WonderP, 3)]),
                    WonderStage(&[RCost(Wood, 2)], vec![ce(9)]),
                    WonderStage(&[RCost(Papyrus, 2)], vec![pe(WonderP, 7)]),
                ]
            ),
            WonderSide(
                "Éphesos - B",
                vec![allre(&[Papyrus])],
                vec![
                    WonderStage(&[RCost(Stone, 2)], vec![pe(WonderP, 2), ce(4)]),
                    WonderStage(&[RCost(Wood, 2)], vec![pe(WonderP, 3), ce(4)]),
                    WonderStage(&[RCost(Papyrus, 1), RCost(Glass, 1), RCost(Loom, 1)], vec![pe(WonderP, 5), ce(4)]),
                ]
            )
        ),
        Wonder(
            "Babylon",
            WonderSide(
                "Babylon - A",
                vec![allre(&[Clay])],
                vec![
                    WonderStage(&[RCost(Clay, 2)], vec![pe(WonderP, 3)]),
                    WonderStage(&[RCost(Wood, 3)], vec![anyse(&[Compass, Gears, Tablet])]),
                    WonderStage(&[RCost(Clay, 4)], vec![pe(WonderP, 7)]),
                ]
            ),
            WonderSide(
                "Babylon - B",
                vec![allre(&[Clay])],
                vec![
                    WonderStage(&[RCost(Loom, 1), RCost(Clay, 1)], vec![pe(WonderP, 3)]),
                    WonderStage(&[RCost(Glass, 1), RCost(Wood, 2)], vec![plce()]),
                    WonderStage(&[RCost(Papyrus, 1), RCost(Clay, 3)], vec![anyse(&[Compass, Gears, Tablet])]),
                ]
            )
        ),
        Wonder(
            "Olympía",
            WonderSide(
                "Olympía - A",
                vec![allre(&[Wood])],
                vec![
                    WonderStage(&[RCost(Wood, 2)], vec![pe(WonderP, 3)]),
                    WonderStage(&[RCost(Stone, 2)], vec![cfe()]),
                    WonderStage(&[RCost(Ore, 2)], vec![pe(WonderP, 7)]),
                ]
            ),
            WonderSide(
                "Olympía - B",
                vec![allre(&[Wood])],
                vec![
                    WonderStage(&[RCost(Wood, 2)], vec![te(&[East, West], &[Wood, Stone, Ore, Clay])]),
                    WonderStage(&[RCost(Stone, 2)], vec![pe(WonderP, 5)]),
                    WonderStage(&[RCost(Loom, 1), RCost(Ore, 2)], vec![cge()]),
                ]
            )
        ),
        Wonder(
            "Halikarnassós",
            WonderSide(
                "Halikarnassós - A",
                vec![allre(&[Loom])],
                vec![
                    WonderStage(&[RCost(Clay, 2)], vec![pe(WonderP, 3)]),
                    WonderStage(&[RCost(Ore, 3)], vec![bffdce()]),
                    WonderStage(&[RCost(Loom, 2)], vec![pe(WonderP, 7)]),
                ]
            ),
            WonderSide(
                "Halikarnassós - B",
                vec![allre(&[Loom])],
                vec![
                    WonderStage(&[RCost(Ore, 2)], vec![pe(WonderP, 2), bffdce()]),
                    WonderStage(&[RCost(Clay, 3)], vec![pe(WonderP, 1), bffdce()]),
                    WonderStage(&[RCost(Papyrus, 1), RCost(Glass, 1), RCost(Loom, 1)], vec![bffdce()]),
                ]
            )
        ),
        Wonder(
            "Gizah",
            WonderSide(
                "Gizah - A",
                vec![allre(&[Stone])],
                vec![
                    WonderStage(&[RCost(Stone, 2)], vec![pe(WonderP, 3)]),
                    WonderStage(&[RCost(Wood, 3)], vec![pe(WonderP, 5)]),
                    WonderStage(&[RCost(Stone, 4)], vec![pe(WonderP, 7)]),
                ]
            ),
            WonderSide(
                "Gizah - B",
                vec![allre(&[Stone])],
                vec![
                    WonderStage(&[RCost(Wood, 2)], vec![pe(WonderP, 3)]),
                    WonderStage(&[RCost(Stone, 3)], vec![pe(WonderP, 5)]),
                    WonderStage(&[RCost(Clay, 3)], vec![pe(WonderP, 5)]),
                    WonderStage(&[RCost(Papyrus, 1), RCost(Stone, 4)], vec![pe(WonderP, 7)]),
                ]
            )
        )
    ];
    pub static ref WONDERS_BY_NAME: HashMap<String, &'static Wonder<'static, Effect>> = WONDERS.iter().map(|s| (s.0.to_string(), s)).collect();
    pub static ref WONDER_NAMES: Vec<String> = WONDERS.iter().map(|s| s.0.to_string()).collect();
}
