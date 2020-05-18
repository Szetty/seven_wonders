{-# language FlexibleContexts      #-}
{-# language PartialTypeSignatures #-}
{-# language OverloadedStrings     #-}
{-# language TypeApplications      #-}
{-# language DataKinds             #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Main where

import Network.Wai.Handler.Warp (defaultSettings, setBeforeMainLoop, setPort, setHost, Port)
import Mu.GRpc.Server
import Mu.Server
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import System.IO

import Schema

main :: IO ()
main = do
    port <- fromMaybe "8000" <$> lookupEnv "PORT"
    let warpPort = read port :: Port
    let settings = setBeforeMainLoop (logStart port) $ setPort warpPort $ setHost "127.0.0.1" defaultSettings
    runGRpcAppSettings msgProtoBuf settings id server

logStart :: String -> IO ()
logStart port = do
    putStrLn $ "Started core on port " <> port
    hFlush stdout

server :: MonadServer m => SingleServerT Service m _
server = singleService (method @"Ping" ping)

ping :: (MonadServer m) => PingRequest -> m PingReply
ping (PingRequest name) = pure $ PingReply ("Pong: Core -> " <> name)