module Core.GameInit where

import Domain.GameState
import Domain.Structure
import Domain.Player
import Domain.Wonder
import System.Random.Shuffle
import System.Random (randomRIO)
import Data.Map(Map, fromList)

initWithWonders :: [Player] -> [(String, Int)] -> IO GameState
initWithWonders players wondersWithSides =
    doInit $ mapper <$> zip players wondersWithSides
    where
        mapper (player, (wonderName, side)) = (player, toWonderSide (getWonderByName wonderName) side)

initWithRandomWonders :: [Player] -> IO GameState
initWithRandomWonders players = do
    let playersNo = length players
    wonders <- take playersNo <$> shuffleM wonders
    sides <- sequenceA $ replicate playersNo $ randomRIO (0,1)
    doInit $ (\(player, wonder, side) -> (player, toWonderSide wonder side)) <$> zip3 players wonders sides

doInit :: [(Player, WonderSide Effect)] -> IO GameState
doInit playersWithWonders = do
    let playersNo = length playersWithWonders
    deck <- generateDeck playersNo
    return $ initGameState deck playersWithWonders

generateDeck :: Int -> IO ([Card], [Card], [Card])
generateDeck playersNo =
    generateAgesDeck $ foldl ageSplit ([], [], []) structures
    where 
        ageSplit :: ([Structure Effect], [Structure Effect], [Structure Effect]) -> Structure Effect -> ([Structure Effect], [Structure Effect], [Structure Effect])
        ageSplit (age1, age2, age3) structure@Structure {age = I} = (structure : age1, age2, age3)
        ageSplit (age1, age2, age3) structure@Structure {age = II} = (age1, structure : age2, age3)
        ageSplit (age1, age2, age3) structure@Structure {age = III} = (age1, age2, structure : age3)
        
        generateAgesDeck :: ([Structure Effect], [Structure Effect], [Structure Effect]) -> IO ([Card], [Card], [Card])
        generateAgesDeck (age1, age2, age3) = do 
            age1Cards <- shuffleM $ generateAgeDeck age1
            age2Cards <- shuffleM $ generateAgeDeck age2
            age3Cards <- addGuilds age3 (generateAgeDeck age3) >>= shuffleM
            return (age1Cards, age2Cards, age3Cards)
        
        generateAgeDeck :: [Structure Effect] -> [Card]
        generateAgeDeck = foldl addCardCopies []
        
        addCardCopies :: [Card] -> Structure Effect -> [Card]
        addCardCopies cards structure@Structure{playerThresholds = playerThresholds} =
            cards ++ (fmap (\_ -> Card structure) . filter (<=playersNo)) playerThresholds
        
        addGuilds :: [Structure Effect] -> [Card] -> IO [Card]
        addGuilds structures cards = do
            shuffledGuildCards <- shuffleM $ Card <$> filter isGuildStructure structures
            return $ take (playersNo + 2) shuffledGuildCards ++ cards
