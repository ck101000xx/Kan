{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Kan.Api.Actions
 ( module Kan.Api.Actions.Types
 , apiStart2
 , apiBasic
 , apiShip
 , apiDeck
 , apiHokyuCharge
 , apiMissionStart
 , apiMissionResult
 ) where

import Kan.Api
import Kan.Api.Actions.Types
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Logger
import Data.Aeson
import Data.ByteString.Char8 as B
import Data.Monoid
import Data.Time.Clock.POSIX

api' :: (MonadIO m) =>  ByteString -> [(ByteString, ByteString)] -> ApiT m ()
api' path params = go >> return ()
  where
    go :: (MonadIO m) => ApiT m Value
    go = api path params

apiStart2 :: (MonadIO m, FromJSON a) => ApiT m a
apiStart2 = api "/api_start2" []

apiGetMember :: (FromJSON a, MonadIO m) => ByteString -> [(ByteString, ByteString)] -> ApiT m a
apiGetMember = api . ("/api_get_member" <>)

apiBasic :: (MonadIO m) => ApiT m Basic
apiBasic = apiGetMember "/basic" []

apiShip :: (MonadIO m) => ApiT m [Ship]
apiShip = apiGetMember "/ship" []

apiDeck :: (MonadIO m) => ApiT m [Deck]
apiDeck = apiGetMember "/deck" []

apiHokyuCharge :: (MonadIO m) => ChargeKind -> [ShipId] -> ApiT m ()
apiHokyuCharge kind ids =
  api' "/api_req_hokyu/charge" $
    [ ("api_kind", convertKind kind)
    , ("api_onslot", "1")
    , ("api_id_items", convertIds ids) ]
  where
    convertKind kind =
      case kind of
        Fuel   -> "1"
        Bullet -> "2"
        Both   -> "3"
    convertIds = B.intercalate "," . fmap convertId
    convertId (ShipId i) = pack . show $ i

apiMissionStart :: (MonadIO m) => DeckId -> MissionId -> ApiT m MissionStart
apiMissionStart (DeckId deck) (MissionId mission) = do
  api "/api_req_mission/start" $
    [ ("api_deck_id", pack . show $ deck)
    , ("api_mission_id", pack . show $ mission)
    ]

apiMissionResult :: (MonadIO m) => DeckId -> ApiT m ()
apiMissionResult (DeckId deck) =
  apiPort >> api' "/api_req_mission/result" [("api_deck_id", pack . show $ deck)]

apiPort :: (MonadIO m) => ApiT m ()
apiPort = do
  portNum <- fmap memberId apiBasic >>= fmap (lift . liftIO) getPortNum
  api' "/api_port/port" [("api_port", portNum), ("api_sort_key", "5"), ("spi_sort_order", "2")]
  where
    getPortNum memberId = do
      time <- fmap floor getPOSIXTime
      return . pack $
        '1' : show (memberId `mod` 1000) ++
        show ((9999999999 - time - memberId) * array !! (memberId `mod` 10)) ++
        suffix
      where
        suffix = "1000"
        array = [1802, 9814, 5616, 4168, 7492, 5188, 2753, 8118, 6381, 7636]
