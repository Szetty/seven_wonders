{-# language DataKinds             #-}
{-# language DeriveAnyClass        #-}
{-# language DeriveGeneric         #-}
{-# language DuplicateRecordFields #-}
{-# language FlexibleContexts      #-}
{-# language FlexibleInstances     #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds             #-}
{-# language TemplateHaskell       #-}
{-# language TypeFamilies          #-}
{-# language TypeOperators         #-}

module Schema where

import Data.Text as T
import GHC.Generics

import Mu.Quasi.GRpc
import Mu.Schema

grpc "PingSchema" id "../proto/ping.proto"

-- A. Map to Haskell types
newtype PingRequest
  = PingRequest { name :: T.Text }
  deriving (Eq, Show, Generic
           , ToSchema   PingSchema "PingRequest"
           , FromSchema PingSchema "PingRequest")

newtype PingReply
  = PingReply { message :: T.Text }
  deriving (Eq, Show, Generic
           , ToSchema   PingSchema "PingReply"
           , FromSchema PingSchema "PingReply")