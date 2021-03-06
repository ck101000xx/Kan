{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Kan.Api.Actions.Types
 ( DeckId(..)
 , ShipId(..)
 , MissionId(..)
 , Basic(..)
 , Deck(..)
 , MissionState(..)
 , Ship(..)
 , ChargeKind(..)
 , MissionStart(..)
 ) where

import Control.Applicative
import Data.Aeson
import Data.Ratio
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Kan.Api.Actions.TH

newtype DeckId = DeckId Int deriving (Show, Eq, ToJSON, FromJSON)

newtype ShipId = ShipId Int deriving (Show, Eq, ToJSON, FromJSON)

newtype MissionId = MissionId Int deriving (Show, Eq, ToJSON, FromJSON)

data Basic = Basic
  { memberId :: Int
  -- TODO 
  } deriving (Show)

instance FromJSON Basic where
  parseJSON (Object v) = Basic <$>
    (read <$> v .: "api_member_id")

data Deck = Deck
  { deckId :: DeckId
  , ship :: [Maybe ShipId]
  , mission :: Maybe (MissionState, MissionId, UTCTime, Int)
  } deriving (Show)

data MissionState = Running | Complete deriving (Show)

data Ship = Ship
  { id :: ShipId
  }

deriveApiData ''Ship

parseTime :: Integer -> UTCTime
parseTime = posixSecondsToUTCTime . fromRational . (% 1000)

instance FromJSON Deck where
  parseJSON (Object v) =
    let
      parseMaybeShipId :: Int -> Maybe (ShipId)
      parseMaybeShipId -1 = Nothing
      parseMaybeShipId i  = Just (ShipId i)
      parseMission :: (Int, Int, Integer, Int) -> Maybe (MissionState, MissionId, UTCTime, Int)
      parseMission (0, _, _, _) = Nothing
      parseMission (a, b, c, d) = Just $
        ( case a of
            1 -> Running
            2 -> Complete
        , MissionId b
        , parseTime c
        , d )
    in
      Deck <$>
        (DeckId <$> v .: "api_id") <*>
        (fmap parseMaybeShipId  <$> v .: "api_ship") <*>
        (parseMission <$> v .: "api_mission")

data ChargeKind =
  Fuel |
  Bullet |
  Both

data MissionStart = MissionStart
  { completeTime :: UTCTime
  } deriving (Show)

instance FromJSON MissionStart where
  parseJSON (Object v) =
    MissionStart <$>
      (parseTime <$> v .: "api_complatetime")
