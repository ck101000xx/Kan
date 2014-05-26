{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Kan.Api.Actions.Types where
import Control.Applicative
import Data.Aeson
import Data.Time.Clock.POSIX
import Data.Vector
import Kan.Api.Actions.TH

newtype ShipId = ShipId Int deriving (Show, Eq, ToJSON, FromJSON)

newtype MissionId = MissionId Int deriving (Show, Eq, ToJSON, FromJSON)

data Basic = Basic
  { memberId :: Int
  -- TODO 
  } deriving (Show)

deriveApiData ''Basic

data Deck = Deck
  { ship :: Vector (Maybe ShipId)
  , mission :: (Int, MissionId, POSIXTime, Int)
  } deriving (Show)


instance FromJSON Deck where
  parseJSON (Object v) =
    let
      parseMaybeShipId :: Int -> Maybe (ShipId)
      parseMaybeShipId -1 = Nothing
      parseMaybeShipId i  = Just (ShipId i)
    in
      Deck <$>
        (fmap parseMaybeShipId  <$> v .: "api_ship")

data ChargeKind =
  Fuel |
  Bullet |
  Both
