module Domain.GameState where

import Domain.Wonder
import Domain.Structure
import Domain.Player
import Domain.Resource

import Data.Set (Set, size)
import Data.Map (Map, fromList, lookup, empty, adjust)
import Data.Maybe (fromJust)
import Data.Function (on)
import Data.List (maximumBy, elem, length)
import Data.List.Index

import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Reader (Reader, runReader)

type Target = Domain.Player.Name
type Env = (GameState, Target)
type Effect = State Env (Maybe String)
type Action = Reader GameState

data GameState = GameState {
    deck :: Deck
,   players :: Map Domain.Player.Name Player
,   playerStates :: Map Domain.Player.Name PlayerState
,   neighbours :: Map Domain.Player.Name (Domain.Player.Name, Domain.Player.Name)
,   cardsDismissed :: [Card]
,   currentAgeCards :: Map Domain.Player.Name [Card]
} deriving (Show)

data PlayerState = PlayerState {
    playerName ::                   Domain.Player.Name
,   coins ::                        Int
,   militarySymbols ::              Int
,   battleTokens ::                 [Int]
,   scientificSymbols ::            Map ScientificSymbol Int
,   builtStructures ::              Map Category (Set Domain.Structure.Name)
,   resourcesProduced ::            [ResourceProduced]
,   pointActions ::                 [Action (Map PointCategory Int -> Map PointCategory Int)]
,   tradeActions ::                 [Action (Domain.Player.Name -> ResourceType -> Int)]
,   constructFreeAction ::          Bool
,   constructLastStructureAction :: Bool
,   copyGuildAction ::              Bool
,   scientificActions ::            [Action (Map ScientificSymbol Int -> Map ScientificSymbol Int)]
,   wonderStagesBuilt ::            Int
,   wonder ::                       WonderSide Effect
}

instance Show PlayerState where
    show PlayerState { playerName = playerName
                     , coins = coins
                     , militarySymbols = militarySymbols
                     , battleTokens = battleTokens
                     , scientificSymbols = scientificSymbols
                     , builtStructures = builtStructures
                     , resourcesProduced = resourcesProduced
                     , pointActions = pointActions
                     , tradeActions = tradeActions
                     , constructFreeAction = constructFreeAction
                     , constructLastStructureAction = constructLastStructureAction
                     , copyGuildAction = copyGuildAction
                     , scientificActions = scientificActions
                     , wonderStagesBuilt = wonderStagesBuilt
                     , wonder = wonder
                     } = 
        "PlayerState {playerName = " ++ show playerName
        ++ ", coins = " ++ show coins
        ++ ", militarySymbols = " ++ show militarySymbols
        ++ ", battleTokens = " ++ show battleTokens
        ++ ", scientificSymbols = " ++ show scientificSymbols
        ++ ", builtStructures = " ++ show builtStructures
        ++ ", resourcesProduced = " ++ show resourcesProduced
        ++ ", pointActions = " ++ show (length pointActions)
        ++ ", tradeActions = " ++ show (length tradeActions)
        ++ ", constructFreeAction = " ++ show constructFreeAction
        ++ ", constructLastStructureAction = " ++ show constructLastStructureAction
        ++ ", copyGuildAction = " ++ show copyGuildAction
        ++ ", scientificActions = " ++ show (length scientificActions)
        ++ ", wonderStagesBuilt = " ++ show wonderStagesBuilt
        ++ ", wonder = " ++ show wonder
        ++ "}"

type Point = Int
type Deck = ([Card], [Card], [Card])

data EffectDirection = East | West | Self deriving (Enum, Show)
newtype Card = Card {
    structure :: Structure Effect
} deriving (Show)

data PointCategory = MilitaryP | TreasuryP | WonderP | CivilianP | ScientificP | CommercialP | GuildsP deriving (Enum, Show, Ord, Eq)

initGameState :: Deck -> [(Player, WonderSide Effect)] -> GameState
initGameState deck players = GameState deck (toMapByName players) (initPlayerStates playerNamesAndWonders) (initNeighbours playerNames) [] empty
    where playerNamesAndWonders = fmap (\(Player {name = name}, wonder) -> (name, wonder)) players
          playerNames = fmap (\(Player {name = name}, _) -> name) players
          toMapByName = fromList . fmap mapper
          mapper (player@Player {name = name}, _) = (name, player)

initPlayerStates :: [(Domain.Player.Name, WonderSide Effect)] -> Map Domain.Player.Name PlayerState
initPlayerStates = fromList . fmap mapper
    where mapper (name, wonder) = (name, initialPlayerState name wonder)

initialPlayerState :: Domain.Player.Name -> WonderSide Effect -> PlayerState
initialPlayerState playerName wonder =
    PlayerState {
        playerName = playerName,
        coins = 3,
        militarySymbols = 0,
        battleTokens = [],
        scientificSymbols = fromList [(Compass, 0), (Gears, 0), (Tablet, 0)],
        builtStructures = fromList [],
        resourcesProduced = [],
        pointActions = [],
        tradeActions = [defaultTradeAction],
        constructFreeAction = False,
        constructLastStructureAction = False,
        copyGuildAction = False,
        scientificActions = [],
        wonderStagesBuilt = 0,
        wonder = wonder
    }

defaultTradeAction = (return $ const $ const 2) :: Action (Domain.Player.Name -> ResourceType -> Int)

initNeighbours :: [Domain.Player.Name] -> Map Domain.Player.Name (Domain.Player.Name, Domain.Player.Name)
initNeighbours playerNames = fromList $ mapper <$> indexed playerNames
    where mapper (idx, name) = (name, (westNeighbour idx, eastNeighbour idx))
          playerNo = length playerNames
          westNeighbour idx = playerNames !! ((idx - 1) `mod` playerNo)
          eastNeighbour idx = playerNames !! ((idx + 1) `mod` playerNo)

lookupNeighbours :: GameState -> Domain.Player.Name -> (Domain.Player.Name, Domain.Player.Name)
lookupNeighbours GameState{neighbours = neighbours} playerName =
    fromJust $ Data.Map.lookup playerName neighbours

lookupPlayerState :: GameState -> Domain.Player.Name -> PlayerState
lookupPlayerState GameState{playerStates = playerStates} playerName =
    fromJust $ Data.Map.lookup playerName playerStates

toWonderSide :: Wonder Effect -> Int -> WonderSide Effect
toWonderSide Wonder{sideA = sideA, sideB = sideB} side =
    case side of
        0 -> sideA
        1 -> sideB

anyResourceEffect :: [ResourceType] -> Effect
anyResourceEffect resources = 
    applyEffect (\playerState@PlayerState{resourcesProduced = resourcesProduced} ->
        playerState{resourcesProduced = Any resources : resourcesProduced}
    )


allResourceEffect :: [ResourceType] -> Effect
allResourceEffect resources =
    applyEffect (\playerState@PlayerState{resourcesProduced = resourcesProduced} ->
        playerState{resourcesProduced = foldl addResource resourcesProduced resources}
    )
    where 
        addResource [] resource = [Single resource 1]
        addResource (Single resourceType count : resourcesProduced) resource =
            if resourceType == resource then
                Single resourceType (count + 1) : resourcesProduced
            else
                addResource resourcesProduced resource

pointEffect :: PointCategory -> Point -> Effect
pointEffect pointCategory pointUnit =
    applyEffect (\playerState@PlayerState{pointActions = pointActions} ->
        playerState{pointActions = pointAction : pointActions}
    )
    where 
        pointAction :: Action (Map PointCategory Int -> Map PointCategory Int)
        pointAction = return $ Data.Map.adjust (pointUnit+) pointCategory


dynamicPointEffect :: PointCategory -> [EffectDirection] -> [Category] -> Int -> Effect
dynamicPointEffect pointCategory effectDirections structureCategories pointUnit =
    applyEffectWithDirections effectDirections (dynamicPointPlayerStateTransformer pointCategory pointUnit (countStructures structureCategories))
        

dynamicWonderPointEffect :: PointCategory -> [EffectDirection] -> Int -> Effect
dynamicWonderPointEffect pointCategory effectDirections pointUnit =
    applyEffectWithDirections effectDirections (dynamicPointPlayerStateTransformer pointCategory pointUnit countWonderStages)

dynamicBattleLostPointEffect :: PointCategory -> [EffectDirection] -> Int -> Effect
dynamicBattleLostPointEffect pointCategory effectDirections pointUnit =
    applyEffectWithDirections effectDirections (dynamicPointPlayerStateTransformer pointCategory pointUnit dynamicPointMapper)
    where
        dynamicPointMapper PlayerState{battleTokens = battleTokens} = length $ filter (<0) battleTokens

dynamicPointPlayerStateTransformer :: PointCategory -> Int -> (PlayerState -> Int) -> [PlayerState] -> PlayerState -> PlayerState
dynamicPointPlayerStateTransformer pointCategory pointUnit dynamicPointMapper playerStates playerState@PlayerState{pointActions = pointActions} =
    playerState{pointActions = pointAction : pointActions}
    where
        pointAction :: Action (Map PointCategory Int -> Map PointCategory Int)
        pointAction = return $ adjust (points+) pointCategory
        points = pointUnit * sum (dynamicPointMapper <$> playerStates)

militaryEffect :: Int -> Effect
militaryEffect militarySymbolsDelta =
    applyEffect (\playerState@PlayerState{militarySymbols = militarySymbols} ->
        playerState{militarySymbols = militarySymbols + militarySymbolsDelta}
    )

anyScientificEffect :: [ScientificSymbol] -> Effect
anyScientificEffect scientificSymbols =
    applyEffect (\playerState@PlayerState{scientificActions = scientificActions} ->
        playerState{scientificActions = newScientificAction : scientificActions}
    )
    where
        newScientificAction :: Action (Map ScientificSymbol Int -> Map ScientificSymbol Int)
        newScientificAction =
            return (\scientificSymbolsMap ->
                adjust (1+) (bestSymbol scientificSymbolsMap scientificSymbols) scientificSymbolsMap
            )
        bestSymbol scientificSymbolsMap = maximumBy (compare `on` pointsWith scientificSymbolsMap)
        pointsWith scientificSymbolsMap scientificSymbol = calculateScientificPoints (adjust (+1) scientificSymbol scientificSymbolsMap)
        

allScientificEffect :: [ScientificSymbol] -> Effect
allScientificEffect newScientificSymbols =
    applyEffect (\playerState@PlayerState{scientificSymbols = scientificSymbols} ->
        playerState{scientificSymbols = foldl (flip (adjust (1+))) scientificSymbols newScientificSymbols}
    )

coinEffect :: Int -> Effect
coinEffect newCoins =
    applyEffect (\playerState@PlayerState{coins = coins} ->
        playerState{coins = coins + newCoins}
    )

dynamicCoinEffect :: [EffectDirection] -> [Category] -> Int -> Effect
dynamicCoinEffect effectDirections structureCategories coinUnit =
    applyEffectWithDirections effectDirections (dynamicCoinPlayerStateTransformer coinUnit (countStructures structureCategories))

dynamicWonderCoinEffect :: [EffectDirection] -> Int -> Effect
dynamicWonderCoinEffect effectDirections coinUnit =
    applyEffectWithDirections effectDirections (dynamicCoinPlayerStateTransformer coinUnit countWonderStages)

dynamicCoinPlayerStateTransformer :: Int -> (PlayerState -> Int) -> [PlayerState] -> PlayerState -> PlayerState
dynamicCoinPlayerStateTransformer coinUnit dynamicCoinMapper playerStates playerState@PlayerState{coins = coins} =
    playerState{coins = coins + coinUnit * sum (dynamicCoinMapper <$> playerStates)}

tradeEffect :: [EffectDirection] -> [ResourceType] -> Effect
tradeEffect effectDirections resourceTypes =
    applyEffectWithDirections effectDirections playerStateTransformer
    where 
        playerStateTransformer playerStates playerState@PlayerState{tradeActions = tradeActions} =
            playerState{tradeActions = tradeAction : tradeActions}
            where
                playerNames = playerName <$> playerStates
                tradeAction :: Action (Domain.Player.Name -> ResourceType -> Int)
                tradeAction = 
                    return (\playerName resourceType ->
                        if (playerName `elem` playerNames) && (resourceType `elem` resourceTypes) then
                            1
                        else
                            2
                    )
    

buildFreeFromDiscardedEffect :: Effect
buildFreeFromDiscardedEffect =
    return $ Just "buildFreeFromDiscarded"

constructFreeEffect :: Effect
constructFreeEffect = 
    applyEffect (\playerState ->
        playerState{constructFreeAction = True}
    )

constructLastStructureEffect :: Effect
constructLastStructureEffect =
    applyEffect (\playerState ->
        playerState{constructLastStructureAction = True}
    )

copyGuildEffect :: Effect
copyGuildEffect =
    applyEffect (\playerState ->
        playerState{copyGuildAction = True}
    )

applyEffectWithDirections :: [EffectDirection] -> ([PlayerState] -> PlayerState -> PlayerState) -> Effect
applyEffectWithDirections directions playerStateTransformer = do
    (gameState, playerName) <- get
    let playerNeighbours = lookupNeighbours gameState playerName
    let playerStatesFromDirections = lookupPlayerState gameState . playerNameFromDirection playerName playerNeighbours <$> directions
    applyEffect (playerStateTransformer playerStatesFromDirections)
    where
        playerNameFromDirection playerName (eastNeighbour, westNeighbour) direction =
            case direction of
                East -> eastNeighbour
                West -> westNeighbour
                Self -> playerName

applyEffect :: (PlayerState -> PlayerState) -> Effect
applyEffect playerStateTransformer = do
    (gameState@GameState{playerStates = playerStates}, playerName) <- get
    let newPlayerState = playerStateTransformer $ lookupPlayerState gameState playerName
    let newPlayerStates = Data.Map.adjust (const newPlayerState) playerName playerStates
    put (gameState {playerStates = newPlayerStates}, playerName)
    return Nothing

extractActions :: GameState -> [Action a] -> [a]
extractActions gameState = (flip runReader gameState <$>)

combineFunctionActions :: GameState -> [Action (a -> a)] -> (a -> a)
combineFunctionActions gameState actions = foldl (.) id $ flip runReader gameState <$> actions

countStructures :: [Category] -> PlayerState -> Int
countStructures structureCategories PlayerState{builtStructures = builtStructures} =
    sum $ categoryCount <$> structureCategories
    where
        categoryCount :: Category -> Int
        categoryCount category =
            case Data.Map.lookup category builtStructures of
                Just structures ->
                    size structures
                Nothing ->
                    0

countWonderStages :: PlayerState -> Int
countWonderStages PlayerState{wonderStagesBuilt = wonderStagesBuilt} = wonderStagesBuilt

getStructureByName :: String -> Structure Effect
getStructureByName =
    fromJust . (`Data.Map.lookup` structuresByName)

structuresByName :: Map String (Structure Effect)
structuresByName =
    fromList $ (\structure@(Structure name _ _ _ _ _ _ _) -> (name, structure)) <$> structures

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
    ,   Structure "Pawnshop" Civilian I [pointEffect CivilianP 3] [] []           (0, [])             [4, 7]
    ,   Structure "Baths"    Civilian I [pointEffect CivilianP 3] [] ["Aqueduct"] (0, [Cost Stone 1]) [3, 7]
    ,   Structure "Altar"    Civilian I [pointEffect CivilianP 2] [] ["Temple"]   (0, [])             [3, 5]
    ,   Structure "Theater"  Civilian I [pointEffect CivilianP 2] [] ["Statue"]   (0, [])             [3, 6]
    -- AGE I Military (Red)
    ,   Structure "Stockade"    Military I [militaryEffect 1] [] [] (0, [Cost Wood 1]) [3, 7]
    ,   Structure "Barracks"    Military I [militaryEffect 1] [] [] (0, [Cost Ore 1])  [3, 5]
    ,   Structure "Guard tower" Military I [militaryEffect 1] [] [] (0, [Cost Clay 1]) [3, 4]
    -- AGE I Scientific (Green)
    ,   Structure "Apothecary"  Scientific I [allScientificEffect [Compass]] [] ["Stables", "Dispensary"]       (0, [Cost Loom 1])    [3, 5]
    ,   Structure "Workshop"    Scientific I [allScientificEffect [Gears]]   [] ["Archery Range", "Laboratory"] (0, [Cost Glass 1])   [3, 7]
    ,   Structure "Scriptorium" Scientific I [allScientificEffect [Tablet]]  [] ["Courthouse", "Library"]       (0, [Cost Papyrus 1]) [3, 4]
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
    ,   Structure "Courthouse" Civilian II [pointEffect CivilianP 4] ["Scriptorium"] ["Senate"]   (0, [Cost Clay 2, Cost Loom 1])               [3, 5]
    ,   Structure "Aqueduct"   Civilian II [pointEffect CivilianP 5] ["Baths"]       []           (0, [Cost Stone 3])                           [3, 7]
    ,   Structure "Temple"     Civilian II [pointEffect CivilianP 3] ["Altar"]       ["Pantheon"] (0, [Cost Wood 1, Cost Clay 1, Cost Glass 1]) [3, 6]
    ,   Structure "Statue"     Civilian II [pointEffect CivilianP 4] ["Theater"]     ["Gardens"]  (0, [Cost Wood 1, Cost Ore 2])                [3, 7]
    -- AGE II Military (Red)
    ,   Structure "Walls"           Military II [militaryEffect 2] []             ["Fortifications"] (0, [Cost Stone 3])                         [3, 7]
    ,   Structure "Training Ground" Military II [militaryEffect 2] []             ["Circus"]         (0, [Cost Wood 1, Cost Ore 2])              [4, 6, 7]
    ,   Structure "Stables"         Military II [militaryEffect 2] ["Apothecary"] []                 (0, [Cost Ore 1, Cost Clay 1, Cost Wood 1]) [3, 5]
    ,   Structure "Archery Range"   Military II [militaryEffect 2] ["Workshop"]   []                 (0, [Cost Wood 2, Cost Ore 1])              [3, 6]
    -- AGE II Scientific (Green) 
    ,   Structure "Dispensary" Scientific II [allScientificEffect [Compass]] ["Apothecary"]  ["Lodge", "Arena"]                (0, [Cost Ore 2, Cost Glass 1])    [3, 4]
    ,   Structure "Laboratory" Scientific II [allScientificEffect [Gears]]   ["Workshop"]    ["Siege Workshop", "Observatory"] (0, [Cost Clay 2, Cost Papyrus 1]) [3, 5]
    ,   Structure "Library"    Scientific II [allScientificEffect [Tablet]]  ["Scriptorium"] ["Senate", "University"]          (0, [Cost Stone 2, Cost Loom 1])   [3, 6]
    ,   Structure "School"     Scientific II [allScientificEffect [Tablet]]  []              ["Academy", "Study"]              (0, [Cost Wood 1, Cost Papyrus 1]) [3, 7]
    -- AGE II Commercial (Yellow)
    ,   Structure "Forum"       Commercial II [anyResourceEffect [Loom, Glass, Papyrus]]    ["East trading post", "West trading post"] ["Haven"]      (0, [Cost Clay 2]) [3, 6, 7]
    ,   Structure "Caravansery" Commercial II [anyResourceEffect [Wood, Stone, Ore, Clay]]  ["Marketplace"]                            ["Lighthouse"] (0, [Cost Wood 2]) [3, 5, 6]
    ,   Structure "Vineyard"    Commercial II [dynamicCoinEffect [East, West, Self] [RM] 1] []                                         []             (0, [])            [3, 6]
    ,   Structure "Bazar"       Commercial II [dynamicCoinEffect [East, West, Self] [MG] 2] []                                         []             (0, [])            [4, 7]

    -- AGE III Civilian (Blue)
    ,   Structure "Pantheon"  Civilian III [pointEffect CivilianP 7] ["Temple"]  [] (0, [Cost Clay 2, Cost Ore 1, Cost Papyrus 1, Cost Loom 1, Cost Glass 1])                            [3, 6]
    ,   Structure "Gardens"   Civilian III [pointEffect CivilianP 5] ["Statue"]  [] (0, [Cost Clay 2, Cost Wood 1])                                                                      [3, 4]
    ,   Structure "Town hall" Civilian III [pointEffect CivilianP 6] []          [] (0, [Cost Stone 2, Cost Ore 1, Cost Glass 1])                                                        [3, 5, 6]
    ,   Structure "Palace"    Civilian III [pointEffect CivilianP 8] []          [] (0, [Cost Wood 1, Cost Stone 1, Cost Ore 1, Cost Clay 1, Cost Loom 1, Cost Glass 1, Cost Papyrus 1]) [3, 7]
    ,   Structure "Senate"    Civilian III [pointEffect CivilianP 6] ["Library"] [] (0, [Cost Wood 2, Cost Ore 1, Cost Stone 1])                                                         [3, 5]
    -- AGE III Military (Red)
    ,   Structure "Fortifications" Military III [militaryEffect 3] ["Walls"]           [] (0, [Cost Ore 3, Cost Stone 1])         [3, 7]
    ,   Structure "Circus"         Military III [militaryEffect 3] ["Training Ground"] [] (0, [Cost Stone 3, Cost Ore 1])         [4, 5, 6]
    ,   Structure "Arsenal"        Military III [militaryEffect 3] []                  [] (0, [Cost Ore 1, Cost Wood 2, Cost Loom 1]) [3, 4, 7]
    ,   Structure "Siege Workshop" Military III [militaryEffect 3] ["Laboratory"]      [] (0, [Cost Wood 1, Cost Clay 3])         [3, 5]
    -- -- AGE III Scientific (Green)
    ,   Structure "Lodge"       Scientific III [allScientificEffect [Compass]] ["Dispensary"] [] (0, [Cost Clay 2, Cost Loom 1, Cost Papyrus 1])  [3, 6]
    ,   Structure "Observatory" Scientific III [allScientificEffect [Gears]]   ["Laboratory"] [] (0, [Cost Ore 2, Cost Glass 1, Cost Loom 1])     [3, 7]
    ,   Structure "University"  Scientific III [allScientificEffect [Tablet]]  ["Library"]    [] (0, [Cost Wood 2, Cost Papyrus 1, Cost Glass 1]) [3, 4]
    ,   Structure "Academy"     Scientific III [allScientificEffect [Compass]] ["School"]     [] (0, [Cost Stone 3, Cost Glass 1])            [3, 7]
    ,   Structure "Study"       Scientific III [allScientificEffect [Gears]]   ["School"]     [] (0, [Cost Wood 1, Cost Papyrus 1, Cost Loom 1])  [3, 5]
    -- AGE III Commercial (Yellow)
    ,   Structure "Haven"               Commercial III [dynamicCoinEffect [Self] [RM] 1, dynamicPointEffect CommercialP [Self] [RM] 1]                 ["Forum"]       [] (0, [Cost Ore 1, Cost Wood 1, Cost Loom 1]) [3, 4]
    ,   Structure "Lighthouse"          Commercial III [dynamicCoinEffect [Self] [Commercial] 1, dynamicPointEffect CommercialP [Self] [Commercial] 1] ["Caravansery"] [] (0, [Cost Stone 1, Cost Glass 1])           [3, 6]
    ,   Structure "Chamber of commerce" Commercial III [dynamicCoinEffect [Self] [MG] 2, dynamicPointEffect CommercialP [Self] [MG] 2]                 []              [] (0, [Cost Clay 2, Cost Papyrus 1])          [4, 6]
    ,   Structure "Arena"               Commercial III [dynamicWonderCoinEffect [Self] 3, dynamicWonderPointEffect CommercialP [Self] 1]               ["Dispensary"]  [] (0, [Cost Stone 2, Cost Ore 1])             [3, 5, 7]
    -- -- AGE III Guilds (Purple)
    ,   Structure "Workers Guild"      Guild III [dynamicPointEffect GuildsP [East, West] [RM] 1]         [] [] (0, [Cost Ore 2, Cost Clay 1, Cost Stone 1, Cost Wood 1]) []
    ,   Structure "Craftsmens Guild"   Guild III [dynamicPointEffect GuildsP [East, West] [MG] 2]         [] [] (0, [Cost Ore 2, Cost Stone 2])                           []
    ,   Structure "Traders Guild"      Guild III [dynamicPointEffect GuildsP [East, West] [Commercial] 1] [] [] (0, [Cost Loom 1, Cost Papyrus 1, Cost Glass 1])          []
    ,   Structure "Philosophers Guild" Guild III [dynamicPointEffect GuildsP [East, West] [Scientific] 1] [] [] (0, [Cost Clay 3, Cost Loom 1, Cost Papyrus 1])           []
    ,   Structure "Spies Guild"        Guild III [dynamicPointEffect GuildsP [East, West] [Military] 1]   [] [] (0, [Cost Clay 3, Cost Glass 1])                          []
    ,   Structure "Strategists Guild"  Guild III [dynamicBattleLostPointEffect GuildsP [East, West] 1]    [] [] (0, [Cost Ore 2, Cost Stone 1, Cost Loom 1])              []
    ,   Structure "Shipowners Guild"   Guild III [dynamicPointEffect GuildsP [Self] [RM, MG, Guild] 1]    [] [] (0, [Cost Wood 3, Cost Papyrus 1, Cost Glass 1])          []
    ,   Structure "Scientists Guild"   Guild III [anyScientificEffect [Compass, Gears, Tablet]]             [] [] (0, [Cost Wood 2, Cost Ore 2, Cost Papyrus 1])            []
    ,   Structure "Magistrates Guild"  Guild III [dynamicPointEffect GuildsP [East, West] [Civilian] 2]   [] [] (0, [Cost Wood 3, Cost Stone 1, Cost Loom 1])             []
    ,   Structure "Builders Guild"     Guild III [dynamicWonderPointEffect GuildsP [East, West, Self] 1]  [] [] (0, [Cost Stone 2, Cost Clay 2, Cost Glass 1])            []
    ]

getWonderByName :: String -> Wonder Effect
getWonderByName =
    fromJust . (`Data.Map.lookup` wondersByName)

wondersByName :: Map String (Wonder Effect)
wondersByName =
    fromList $ (\wonder@(Wonder name _ _) -> (name, wonder)) <$> wonders

wonders :: [Wonder Effect]
wonders =
    [
        Wonder "Rhódos"        (WonderSide
                                "Rhódos - A"
                                [allResourceEffect [Ore]]
                                [
                                    WonderStage [Cost Wood 2] [pointEffect WonderP 3],
                                    WonderStage [Cost Clay 3] [militaryEffect 2],
                                    WonderStage [Cost Ore 4] [pointEffect WonderP 7]
                                ]
                               )
                               (WonderSide
                                "Rhódos - B"
                                [allResourceEffect [Ore]]
                                [
                                    WonderStage [Cost Stone 3] [militaryEffect 1, pointEffect WonderP 3, coinEffect 3],
                                    WonderStage [Cost Ore 4] [militaryEffect 1, pointEffect WonderP 4, coinEffect 4]
                                ]
                               )
    ,   Wonder "Alexandria"    (WonderSide
                                "Alexandria - A"
                                [allResourceEffect [Glass]]   
                                [
                                    WonderStage [Cost Stone 2] [pointEffect WonderP 3],
                                    WonderStage [Cost Ore 2] [anyResourceEffect [Wood, Stone, Ore, Clay]],
                                    WonderStage [Cost Glass 2] [pointEffect WonderP 7]
                                ]
                               ) 
                               (WonderSide
                                "Alexandria - B"
                                [allResourceEffect [Glass]]   
                                [
                                    WonderStage [Cost Clay 2] [anyResourceEffect [Wood, Stone, Ore, Clay]],
                                    WonderStage [Cost Wood 2] [anyResourceEffect [Loom, Glass, Papyrus]],
                                    WonderStage [Cost Stone 3] [pointEffect WonderP 7]
                                ]
                               )
    ,   Wonder "Éphesos"       (WonderSide
                                "Éphesos - A"
                                [allResourceEffect [Papyrus]] 
                                [
                                    WonderStage [Cost Stone 2] [pointEffect WonderP 3],
                                    WonderStage [Cost Wood 2] [coinEffect 9],
                                    WonderStage [Cost Papyrus 2] [pointEffect WonderP 7]
                                ]
                               ) 
                               (WonderSide
                                "Éphesos - B"
                                [allResourceEffect [Papyrus]] 
                                [
                                    WonderStage [Cost Stone 2] [pointEffect WonderP 2, coinEffect 4],
                                    WonderStage [Cost Wood 2] [pointEffect WonderP 3, coinEffect 4],
                                    WonderStage [Cost Papyrus 1, Cost Glass 1, Cost Loom 1] [pointEffect WonderP 5, coinEffect 4]
                                ]
                               )
    ,   Wonder "Babylon"       (WonderSide
                                "Babylon - A"
                                [allResourceEffect [Clay]]   
                                [
                                    WonderStage [Cost Clay 2] [pointEffect WonderP 3],
                                    WonderStage [Cost Wood 3] [anyScientificEffect [Compass, Gears, Tablet]],
                                    WonderStage [Cost Clay 4] [pointEffect WonderP 7]
                                ]
                               ) 
                               (WonderSide
                                "Babylon - B"
                                [allResourceEffect [Clay]]    
                                [
                                    WonderStage [Cost Loom 1, Cost Clay 1] [pointEffect WonderP 3],
                                    WonderStage [Cost Glass 1, Cost Wood 2] [constructLastStructureEffect],
                                    WonderStage [Cost Papyrus 1, Cost Clay 3] [anyScientificEffect [Compass, Gears, Tablet]]
                                ]
                               )
    ,   Wonder "Olympía"       (WonderSide
                                "Olympia - A"
                                [allResourceEffect [Wood]]    
                                [
                                    WonderStage [Cost Wood 2] [pointEffect WonderP 3],
                                    WonderStage [Cost Stone 2] [constructFreeEffect],
                                    WonderStage [Cost Ore 2] [pointEffect WonderP 7]
                                ]
                               ) 
                               (WonderSide
                                "Olympia - B"
                                [allResourceEffect [Wood]]    
                                [
                                    WonderStage [Cost Wood 2] [tradeEffect [East, West] [Wood, Stone, Ore, Clay]],
                                    WonderStage [Cost Stone 2] [pointEffect WonderP 5],
                                    WonderStage [Cost Loom 1, Cost Ore 2] [copyGuildEffect]
                                ]
                               )
    ,   Wonder "Halikarnassós" (WonderSide
                                "Halikarnassós - A"
                                [allResourceEffect [Loom]]    
                                [
                                    WonderStage [Cost Clay 2] [pointEffect WonderP 3],
                                    WonderStage [Cost Ore 3] [buildFreeFromDiscardedEffect],
                                    WonderStage [Cost Loom 2] [pointEffect WonderP 7]
                                ]
                               ) 
                               (WonderSide
                                "Halikarnassós - B"
                                [allResourceEffect [Loom]]    
                                [
                                    WonderStage [Cost Ore 2] [pointEffect WonderP 2, buildFreeFromDiscardedEffect],
                                    WonderStage [Cost Clay 3] [pointEffect WonderP 1, buildFreeFromDiscardedEffect],
                                    WonderStage [Cost Papyrus 1, Cost Glass 1, Cost Loom 1] [buildFreeFromDiscardedEffect]
                                ]
                               )
    ,   Wonder "Gizah"         (WonderSide
                                "Gizah - A"
                                [allResourceEffect [Stone]]   
                                [
                                    WonderStage [Cost Stone 2] [pointEffect WonderP 3],
                                    WonderStage [Cost Wood 3] [pointEffect WonderP 5],
                                    WonderStage [Cost Stone 4] [pointEffect WonderP 7]
                                ]
                               ) 
                               (WonderSide
                                "Gizah - B"
                                [allResourceEffect [Stone]]   
                                [
                                    WonderStage [Cost Wood 2] [pointEffect WonderP 3],
                                    WonderStage [Cost Stone 3] [pointEffect WonderP 5],
                                    WonderStage [Cost Clay 3] [pointEffect WonderP 5],
                                    WonderStage [Cost Papyrus 1, Cost Stone 4] [pointEffect WonderP 7]
                                ]
                               )
    ]
