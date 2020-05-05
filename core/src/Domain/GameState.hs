module Domain.GameState where

import Domain.Wonder
import Domain.Structure
import Domain.Player
import Domain.Resource

import Data.Map (Map, fromList, lookup, empty)
import Data.Maybe (fromJust)
import Data.List.Index

import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Reader

type Target = Domain.Structure.Name
type Env = (GameState, Target)
type Effect = State Env ()
type Action = Reader GameState

data GameState = GameState {
    deck :: Deck
,   players :: Map Domain.Player.Name Player
,   playerStates :: Map Domain.Player.Name PlayerState
,   neighbours :: Map Domain.Player.Name (Domain.Player.Name, Domain.Player.Name)
,   cardsDismissed :: [Card]
,   currentAgeCards :: Map Domain.Player.Name [Card]
}

data PlayerState = PlayerState {
     coins ::                        Int
,    militarySymbols ::              Int
,    battleTokens ::                 [Int]
,    scientificSymbols ::            Map ScientificSymbol Int
,    builtStructures ::              [Domain.Structure.Name]
,    resourceActions ::              [Action ([ResourceType] -> [ResourceType])]
,    pointActions ::                 [Action (Int -> Int)]
,    tradeActions ::                 [Action (Domain.Player.Name -> Int)]
,    constructFreeAction ::          Action (Domain.Structure.Name -> Bool)
,    constructLastStructureAction :: Action Bool
,    copyGuildAction ::              Action Bool
}

type Point = Int
type Deck = ([Card], [Card], [Card])

newtype Card = Card {
    structure :: Structure Effect
} deriving (Show)

initGameState :: Deck -> [Player] -> GameState
initGameState deck players = GameState deck (toMapByName players) (initPlayerStates playerNames) (initNeighbours playerNames) [] empty
    where playerNames = fmap Domain.Player.name players
          toMapByName = fromList . fmap mapper
          mapper player@Player {name = name} = (name, player)

initPlayerStates :: [Domain.Player.Name] -> Map Domain.Player.Name PlayerState
initPlayerStates = fromList . fmap mapper
    where mapper name = (name, initialPlayerState)

initialPlayerState :: PlayerState
initialPlayerState =
    PlayerState {
        coins = 3,
        militarySymbols = 0,
        battleTokens = [],
        scientificSymbols = fromList [(Compass, 0), (Gears, 0), (Tablet, 0)],
        builtStructures = [],
        resourceActions = [],
        pointActions = [],
        tradeActions = [defaultTradeAction],
        constructFreeAction = defaultConstructFreeAction,
        constructLastStructureAction = defaultConstructLastStructureAction,
        copyGuildAction = defaultCopyGuildAction
    }
    where
        defaultTradeAction = (return $ const 2) :: Action (Domain.Player.Name -> Int)
        defaultConstructFreeAction = (return $ const False) :: Action (Domain.Structure.Name -> Bool)
        defaultConstructLastStructureAction = return False :: Action Bool
        defaultCopyGuildAction = return False :: Action Bool

initNeighbours :: [Domain.Player.Name] -> Map Domain.Player.Name (Domain.Player.Name, Domain.Player.Name)
initNeighbours playerNames = fromList $ mapper <$> indexed playerNames
    where mapper (idx, name) = (name, (westNeighbour idx, eastNeighbour idx))
          playerNo = length playerNames
          westNeighbour idx = playerNames !! ((idx - 1) `mod` playerNo)
          eastNeighbour idx = playerNames !! ((idx + 1) `mod` playerNo)

lookupNeighbours :: GameState -> Domain.Player.Name -> (Domain.Player.Name, Domain.Player.Name)
lookupNeighbours GameState{neighbours = neighbours} playerName =
    fromJust $ Data.Map.lookup playerName neighbours

anyResourceEffect :: [ResourceType] -> Effect
anyResourceEffect _ = undefined

allResourceEffect :: [ResourceType] -> Effect
allResourceEffect _ = undefined

pointEffect :: Point -> Effect
pointEffect _ = undefined

militaryEffect :: Int -> Effect
militaryEffect _ = undefined

scientificEffect :: [ScientificSymbol] -> Effect
scientificEffect _ = undefined

coinEffect :: Int -> Effect
coinEffect _ = undefined

tradeEffect :: [EffectDirection] -> [ResourceType] -> Effect
tradeEffect _ _ = undefined

dynamicCoinEffect :: [EffectDirection] -> [Category] -> Int -> Effect
dynamicCoinEffect _ _ _ = undefined

dynamicPointEffect :: [EffectDirection] -> [Category] -> Int -> Effect
dynamicPointEffect _ _ _ = undefined

dynamicWonderCoinEffect :: [EffectDirection] -> Int -> Effect
dynamicWonderCoinEffect _ _ = undefined

dynamicWonderPointEffect :: [EffectDirection] -> Int -> Effect
dynamicWonderPointEffect _ _ = undefined

dynamicBattleLostPointEffect :: [EffectDirection] -> Int -> Effect
dynamicBattleLostPointEffect _ _ = undefined

buildFreeFromDiscardedEffect :: Effect
buildFreeFromDiscardedEffect = undefined

constructFreeEffect :: Effect
constructFreeEffect = undefined

constructLastStructureEffect :: Effect
constructLastStructureEffect = undefined

copyGuildEffect :: Effect
copyGuildEffect = undefined

structures :: [Structure Effect]
structures =
    [
    -- AGE I Raw Materials (Brown)
        Structure "Lumber Yard" RM I [allResourceEffect [Wood]]        [] [] (0, []) [3, 4]
    ,   Structure "Stone Pit"   RM I [allResourceEffect [Stone]]       [] [] (0, []) [3, 5]
    ,   Structure "Clay Pool"   RM I [allResourceEffect [Clay]]        [] [] (0, []) [3, 5]
    ,   Structure "Ore Vein"    RM I [allResourceEffect [Ore]]         [] [] (0, []) [3, 4]
    ,   Structure "Tree Farm"   RM I [anyResourceEffect [Wood, Clay]]  [] [] (1, []) [6]
    ,   Structure "Excavation"  RM I [anyResourceEffect [Stone, Clay]] [] [] (1, []) [4]
    ,   Structure "Clay Pit"    RM I [anyResourceEffect [Clay, Ore]]   [] [] (1, []) [3]
    ,   Structure "Timber Yard" RM I [anyResourceEffect [Stone, Wood]] [] [] (1, []) [3]
    ,   Structure "Forest Cave" RM I [anyResourceEffect [Wood, Ore]]   [] [] (1, []) [5]
    ,   Structure "Mine"        RM I [anyResourceEffect [Ore, Stone]]  [] [] (1, []) [6]
    -- AGE I Manufactured Goods (Gray)
    ,   Structure "Loom"       MG I [allResourceEffect [Loom]]    [] [] (0, []) [3, 6]
    ,   Structure "Glassworks" MG I [allResourceEffect [Glass]]   [] [] (0, []) [3, 6]
    ,   Structure "Press"      MG I [allResourceEffect [Papyrus]] [] [] (0, []) [3, 6]
    -- AGE I Civilian (Blue)
    ,   Structure "Pawnshop" Civilian I [pointEffect 3] [] []           (0, [])           [4, 7]
    ,   Structure "Baths"    Civilian I [pointEffect 3] [] ["Aqueduct"] (0, [StoneC 1])   [3, 7]
    ,   Structure "Altar"    Civilian I [pointEffect 2] [] ["Temple"]   (0, [])           [3, 5]
    ,   Structure "Theater"  Civilian I [pointEffect 2] [] ["Statue"]   (0, [])           [3, 6]
    -- AGE I Military (Red)
    ,   Structure "Stockade"    Military I [militaryEffect 1] [] [] (0, [WoodC 1]) [3, 7]
    ,   Structure "Barracks"    Military I [militaryEffect 1] [] [] (0, [OreC 1])  [3, 5]
    ,   Structure "Guard tower" Military I [militaryEffect 1] [] [] (0, [ClayC 1]) [3, 4]
    -- AGE I Scientific (Green)
    ,   Structure "Apothecary"  Scientific I [scientificEffect [Compass]] [] ["Stables", "Dispensary"]       (0, [LoomC 1])    [3, 5]
    ,   Structure "Workshop"    Scientific I [scientificEffect [Gears]]   [] ["Archery Range", "Laboratory"] (0, [GlassC 1])   [3, 7]
    ,   Structure "Scriptorium" Scientific I [scientificEffect [Tablet]]  [] ["Courthouse", "Library"]       (0, [PapyrusC 1]) [3, 4]
    -- AGE I Commercial (Yellow)
    ,   Structure "Tavern"            Commercial I [coinEffect 5]                                    [] []              (0, []) [4, 5, 7]
    ,   Structure "East trading post" Commercial I [tradeEffect [East] [Wood, Stone, Ore, Clay]]     [] ["Forum"]       (0, []) [3, 7]
    ,   Structure "West trading post" Commercial I [tradeEffect [West] [Wood, Stone, Ore, Clay]]     [] ["Forum"]       (0, []) [3, 7]
    ,   Structure "Marketplace"       Commercial I [tradeEffect [East, West] [Loom, Glass, Papyrus]] [] ["Caravansery"] (0, []) [3, 6]

    -- AGE II Raw Materials (Brown)
    ,   Structure "Sawmill"   RM II [allResourceEffect [Wood, Wood]]   [] [] (1, []) [3, 4]
    ,   Structure "Quarry"    RM II [allResourceEffect [Stone, Stone]] [] [] (1, []) [3, 4]
    ,   Structure "Brickyard" RM II [allResourceEffect [Clay, Clay]]   [] [] (1, []) [3, 4]
    ,   Structure "Foundry"   RM II [allResourceEffect [Ore, Ore]]     [] [] (1, []) [3, 4]
    -- AGE II Manufactured Goods (Gray)
    ,   Structure "Loom"       MG II [allResourceEffect [Loom]]    [] [] (0, []) [3, 5]
    ,   Structure "Glassworks" MG II [allResourceEffect [Glass]]   [] [] (0, []) [3, 5]
    ,   Structure "Press"      MG II [allResourceEffect [Papyrus]] [] [] (0, []) [3, 5]
    -- AGE II Civilian (Blue)
    ,   Structure "Courthouse" Civilian II [pointEffect 4] ["Scriptorium"] ["Senate"]   (0, [ClayC 2, LoomC 1])           [3, 5]
    ,   Structure "Aqueduct"   Civilian II [pointEffect 5] ["Baths"]       []           (0, [StoneC 3])                   [3, 7]
    ,   Structure "Temple"     Civilian II [pointEffect 3] ["Altar"]       ["Pantheon"] (0, [WoodC 1, ClayC 1, GlassC 1]) [3, 6]
    ,   Structure "Statue"     Civilian II [pointEffect 4] ["Theater"]     ["Gardens"]  (0, [WoodC 1, OreC 2])            [3, 7]
    -- AGE II Military (Red)
    ,   Structure "Walls"           Military II [militaryEffect 2] []             ["Fortifications"] (0, [StoneC 3])                 [3, 7]
    ,   Structure "Training Ground" Military II [militaryEffect 2] []             ["Circus"]         (0, [WoodC 1, OreC 2])          [4, 6, 7]
    ,   Structure "Stables"         Military II [militaryEffect 2] ["Apothecary"] []                 (0, [OreC 1, ClayC 1, WoodC 1]) [3, 5]
    ,   Structure "Archery Range"   Military II [militaryEffect 2] ["Workshop"]   []                 (0, [WoodC 2, OreC 1])          [3, 6]
    -- AGE II Scientific (Green) 
    ,   Structure "Dispensary" Scientific II [scientificEffect [Compass]] ["Apothecary"]  ["Lodge", "Arena"]                (0, [OreC 2, GlassC 1])    [3, 4]
    ,   Structure "Laboratory" Scientific II [scientificEffect [Gears]]   ["Workshop"]    ["Siege Workshop", "Observatory"] (0, [ClayC 2, PapyrusC 1]) [3, 5]
    ,   Structure "Library"    Scientific II [scientificEffect [Tablet]]  ["Scriptorium"] ["Senate", "University"]          (0, [StoneC 2, LoomC 1])   [3, 6]
    ,   Structure "School"     Scientific II [scientificEffect [Tablet]]  []              ["Academy", "Study"]              (0, [WoodC 1, PapyrusC 1]) [3, 7]
    -- AGE II Commercial (Yellow)
    ,   Structure "Forum"       Commercial II [anyResourceEffect [Loom, Glass, Papyrus]]    ["East trading post", "West trading post"] ["Haven"]      (0, [ClayC 2]) [3, 6, 7]
    ,   Structure "Caravansery" Commercial II [anyResourceEffect [Wood, Stone, Ore, Clay]]  ["Marketplace"]                            ["Lighthouse"] (0, [WoodC 2]) [3, 5, 6]
    ,   Structure "Vineyard"    Commercial II [dynamicCoinEffect [East, West, Self] [RM] 1] []                                         []             (0, [])        [3, 6]
    ,   Structure "Bazar"       Commercial II [dynamicCoinEffect [East, West, Self] [MG] 2] []                                         []             (0, [])        [4, 7]

    -- AGE III Civilian (Blue)
    ,   Structure "Pantheon"  Civilian III [pointEffect 7] ["Temple"]  [] (0, [ClayC 2, OreC 1, PapyrusC 1, LoomC 1, GlassC 1])                    [3, 6]
    ,   Structure "Gardens"   Civilian III [pointEffect 5] ["Statue"]  [] (0, [ClayC 2, WoodC 1])                                                  [3, 4]
    ,   Structure "Town hall" Civilian III [pointEffect 6] []          [] (0, [StoneC 2, OreC 1, GlassC 1])                                        [3, 5, 6]
    ,   Structure "Palace"    Civilian III [pointEffect 8] []          [] (0, [WoodC 1, StoneC 1, OreC 1, ClayC 1, LoomC 1, GlassC 1, PapyrusC 1]) [3, 7]
    ,   Structure "Senate"    Civilian III [pointEffect 6] ["Library"] [] (0, [WoodC 2, OreC 1, StoneC 1])                                         [3, 5]
    -- AGE III Military (Red)
    ,   Structure "Fortifications" Military III [militaryEffect 3] ["Walls"]           [] (0, [OreC 3, StoneC 1])         [3, 7]
    ,   Structure "Circus"         Military III [militaryEffect 3] ["Training Ground"] [] (0, [StoneC 3, OreC 1])         [4, 5, 6]
    ,   Structure "Arsenal"        Military III [militaryEffect 3] []                  [] (0, [OreC 1, WoodC 2, LoomC 1]) [3, 4, 7]
    ,   Structure "Siege Workshop" Military III [militaryEffect 3] ["Laboratory"]      [] (0, [WoodC 1, ClayC 3])         [3, 5]
    -- -- AGE III Scientific (Green)
    ,   Structure "Lodge"       Scientific III [scientificEffect [Compass]] ["Dispensary"] [] (0, [ClayC 2, LoomC 1, PapyrusC 1])  [3, 6]
    ,   Structure "Observatory" Scientific III [scientificEffect [Gears]]   ["Laboratory"] [] (0, [OreC 2, GlassC 1, LoomC 1])     [3, 7]
    ,   Structure "University"  Scientific III [scientificEffect [Tablet]]  ["Library"]    [] (0, [WoodC 2, PapyrusC 1, GlassC 1]) [3, 4]
    ,   Structure "Academy"     Scientific III [scientificEffect [Compass]] ["School"]     [] (0, [StoneC 3, GlassC 1])            [3, 7]
    ,   Structure "Study"       Scientific III [scientificEffect [Gears]]   ["School"]     [] (0, [WoodC 1, PapyrusC 1, LoomC 1])  [3, 5]
    -- AGE III Commercial (Yellow)
    ,   Structure "Haven"               Commercial III [dynamicCoinEffect [Self] [RM] 1, dynamicPointEffect [Self] [RM] 1]                 ["Forum"]       [] (0, [OreC 1, WoodC 1, LoomC 1]) [3, 4]
    ,   Structure "Lighthouse"          Commercial III [dynamicCoinEffect [Self] [Commercial] 1, dynamicPointEffect [Self] [Commercial] 1] ["Caravansery"] [] (0, [StoneC 1, GlassC 1])       [3, 6]
    ,   Structure "Chamber of commerce" Commercial III [dynamicCoinEffect [Self] [MG] 2, dynamicPointEffect [Self] [MG] 2]                 []              [] (0, [ClayC 2, PapyrusC 1])      [4, 6]
    ,   Structure "Arena"               Commercial III [dynamicWonderCoinEffect [Self] 3, dynamicWonderPointEffect [Self] 1]               ["Dispensary"]  [] (0, [StoneC 2, OreC 1])         [3, 5, 7]
    -- -- AGE III Guilds (Purple)
    ,   Structure "Workers Guild"      Guild III [dynamicPointEffect [East, West] [RM] 1]         [] [] (0, [OreC 2, ClayC 1, StoneC 1, WoodC 1]) []
    ,   Structure "Craftsmens Guild"   Guild III [dynamicPointEffect [East, West] [MG] 2]         [] [] (0, [OreC 2, StoneC 2])                   []
    ,   Structure "Traders Guild"      Guild III [dynamicPointEffect [East, West] [Commercial] 1] [] [] (0, [LoomC 1, PapyrusC 1, GlassC 1])      []
    ,   Structure "Philosophers Guild" Guild III [dynamicPointEffect [East, West] [Scientific] 1] [] [] (0, [ClayC 3, LoomC 1, PapyrusC 1])       []
    ,   Structure "Spies Guild"        Guild III [dynamicPointEffect [East, West] [Military] 1]   [] [] (0, [ClayC 3, GlassC 1])                  []
    ,   Structure "Strategists Guild"  Guild III [dynamicBattleLostPointEffect [East, West] 1]    [] [] (0, [OreC 2, StoneC 1, LoomC 1])          []
    ,   Structure "Shipowners Guild"   Guild III [dynamicPointEffect [Self] [RM, MG, Guild] 1]    [] [] (0, [WoodC 3, PapyrusC 1, GlassC 1])      []
    ,   Structure "Scientists Guild"   Guild III [scientificEffect [Compass, Gears, Tablet]]      [] [] (0, [WoodC 2, OreC 2, PapyrusC 1])        []
    ,   Structure "Magistrates Guild"  Guild III [dynamicPointEffect [East, West] [Civilian] 2]   [] [] (0, [WoodC 3, StoneC 1, LoomC 1])         []
    ,   Structure "Builders Guild"     Guild III [dynamicWonderPointEffect [East, West, Self] 1]  [] [] (0, [StoneC 2, ClayC 2, GlassC 1])        []
    ]

wonders :: [Wonder Effect]
wonders =
    [
        Wonder "Rhódos"        (WonderSide 
                                [allResourceEffect [Ore]]
                                [
                                    WonderStage [WoodC 2] [pointEffect 3], 
                                    WonderStage [ClayC 3] [militaryEffect 2], 
                                    WonderStage [OreC 4] [pointEffect 7]
                                ]
                               )
                               (WonderSide 
                                [allResourceEffect [Ore]]
                                [
                                    WonderStage [StoneC 3] [militaryEffect 1, pointEffect 3, coinEffect 3], 
                                    WonderStage [OreC 4] [militaryEffect 1, pointEffect 4, coinEffect 4]
                                ]
                               )
    ,   Wonder "Alexandria"    (WonderSide 
                                [allResourceEffect [Glass]]   
                                [
                                    WonderStage [StoneC 2] [pointEffect 3], 
                                    WonderStage [OreC 2] [anyResourceEffect [Wood, Stone, Ore, Clay]], 
                                    WonderStage [GlassC 2] [pointEffect 7]
                                ]
                               ) 
                               (WonderSide 
                                [allResourceEffect [Glass]]   
                                [
                                    WonderStage [ClayC 2] [anyResourceEffect [Wood, Stone, Ore, Clay]], 
                                    WonderStage [WoodC 2] [anyResourceEffect [Loom, Glass, Papyrus]], 
                                    WonderStage [StoneC 3] [pointEffect 7]
                                ]
                               )
    ,   Wonder "Éphesos"       (WonderSide 
                                [allResourceEffect [Papyrus]] 
                                [
                                    WonderStage [StoneC 2] [pointEffect 3], 
                                    WonderStage [WoodC 2] [coinEffect 9], 
                                    WonderStage [PapyrusC 2] [pointEffect 7]
                                ]
                               ) 
                               (WonderSide 
                                [allResourceEffect [Papyrus]] 
                                [
                                    WonderStage [StoneC 2] [pointEffect 2, coinEffect 4], 
                                    WonderStage [WoodC 2] [pointEffect 3, coinEffect 4], 
                                    WonderStage [PapyrusC 1, GlassC 1, LoomC 1] [pointEffect 5, coinEffect 4]
                                ]
                               )
    ,   Wonder "Babylon"       (WonderSide 
                                [allResourceEffect [Clay]]   
                                [
                                    WonderStage [ClayC 2] [pointEffect 3], 
                                    WonderStage [WoodC 3] [scientificEffect [Compass, Gears, Tablet]], 
                                    WonderStage [ClayC 4] [pointEffect 7]
                                ]
                               ) 
                               (WonderSide 
                                [allResourceEffect [Clay]]    
                                [
                                    WonderStage [LoomC 1, ClayC 1] [pointEffect 3], 
                                    WonderStage [GlassC 1, WoodC 2] [constructLastStructureEffect], 
                                    WonderStage [PapyrusC 1, ClayC 3] [scientificEffect [Compass, Gears, Tablet]]
                                ]
                               )
    ,   Wonder "Olympía"       (WonderSide 
                                [allResourceEffect [Wood]]    
                                [
                                    WonderStage [WoodC 2] [pointEffect 3], 
                                    WonderStage [StoneC 2] [constructFreeEffect], 
                                    WonderStage [OreC 2] [pointEffect 7]
                                ]
                               ) 
                               (WonderSide 
                                [allResourceEffect [Wood]]    
                                [
                                    WonderStage [WoodC 2] [tradeEffect [East, West] [Wood, Stone, Ore, Clay]], 
                                    WonderStage [StoneC 2] [pointEffect 5], 
                                    WonderStage [LoomC 1, OreC 2] [copyGuildEffect]
                                ]
                               )
    ,   Wonder "Halikarnassós" (WonderSide 
                                [allResourceEffect [Loom]]    
                                [
                                    WonderStage [ClayC 2] [pointEffect 3], 
                                    WonderStage [OreC 3] [buildFreeFromDiscardedEffect], 
                                    WonderStage [LoomC 2] [pointEffect 7]
                                ]
                               ) 
                               (WonderSide 
                                [allResourceEffect [Loom]]    
                                [
                                    WonderStage [OreC 2] [pointEffect 2, buildFreeFromDiscardedEffect], 
                                    WonderStage [ClayC 3] [pointEffect 1, buildFreeFromDiscardedEffect], 
                                    WonderStage [PapyrusC 1, GlassC 1, LoomC 1] [buildFreeFromDiscardedEffect]
                                ]
                               )
    ,   Wonder "Gizah"         (WonderSide 
                                [allResourceEffect [Stone]]   
                                [
                                    WonderStage [StoneC 2] [pointEffect 3], 
                                    WonderStage [WoodC 3] [pointEffect 5], 
                                    WonderStage [StoneC 4] [pointEffect 7]
                                ]
                               ) 
                               (WonderSide 
                                [allResourceEffect [Stone]]   
                                [
                                    WonderStage [WoodC 2] [pointEffect 3], 
                                    WonderStage [StoneC 3] [pointEffect 5], 
                                    WonderStage [ClayC 3] [pointEffect 5], 
                                    WonderStage [PapyrusC 1, StoneC 4] [pointEffect 7]
                                ]
                               )
    ]
