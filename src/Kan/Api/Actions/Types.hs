{-# LANGUAGE TemplateHaskell #-}
module Kan.Api.Actions.Types where
import Kan.Api.Actions.TH

data Basic = Basic
  { memberId :: Int
  -- TODO 
  } deriving (Show)

deriveApiData ''Basic

class ShipList a where
  ship :: a -> [Ship]

data Ship = Ship
  { id :: Int
  } deriving (Show)

data DeckPort = DeckPort
  { deckPort_ship :: [Ship]
  } deriving (Show)

instance ShipList DeckPort where
  ship = deckPort_ship

data Port = Port
  { port_ship :: [Ship]
  , deckPort :: [DeckPort]
  -- TODO
  } deriving (Show)

instance ShipList Port where
  ship = port_ship

deriveApiData ''Ship
deriveApiData ''DeckPort
deriveApiData ''Port

