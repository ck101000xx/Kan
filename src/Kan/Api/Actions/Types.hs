{-# LANGUAGE TemplateHaskell #-}
module Kan.Api.Actions.Types where
import Kan.Api.Actions.TH

data Basic = Basic
  { memberId :: Int
  -- TODO 
  } deriving (Show)

deriveApiData ''Basic

data Ship = Ship
  { id :: Int
  } deriving (Show)

deriveApiData ''Ship

