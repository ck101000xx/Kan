{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Kan.Api.Actions.Types where
import Control.Applicative
import Data.Aeson
import Data.Ratio
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Vector
import Kan.Api.Actions.TH

newtype DeckId = DeckId Int deriving (Show, Eq, ToJSON, FromJSON)

newtype ShipId = ShipId Int deriving (Show, Eq, ToJSON, FromJSON)

newtype MissionId = MissionId Int deriving (Show, Eq, ToJSON, FromJSON)

data Basic = Basic
  { memberId :: Int
  -- TODO 
  } deriving (Show)

deriveApiData ''Basic

data Deck = Deck
  { ship :: Vector (Maybe ShipId)
  , mission :: Maybe (MissionState, MissionId, UTCTime, Int)
  } deriving (Show)


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
        , posixSecondsToUTCTime  . fromRational $ c % 1000
        , d )
    in
      Deck <$>
        (fmap parseMaybeShipId  <$> v .: "api_ship") <*>
        (parseMission <$> v .: "api_mission")

data ChargeKind =
  Fuel |
  Bullet |
  Both

