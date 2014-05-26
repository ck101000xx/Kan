{-# LANGUAGE OverloadedStrings #-}
module Kan.Api.Actions
 ( start2
 , basic
 , port
 ) where

import Kan.Api
import Control.Applicative
import Control.Monad.Trans
import Data.Aeson
import Data.ByteString.Char8
import Data.Monoid
import Data.Time.Clock.POSIX

apiStart2 :: (MonadIO m, FromJSON a) => ApiT m a
apiStart2 = api "/api_start2" []

apiGetMember :: (MonadIO m) => ByteString -> (ByteString, ByteString) -> ApiT m a
apiGetMember = api . ("/api_get_member" <>)

apiBasic :: (MonadIO m) => ApiT m Basic
apiBasic = getMember "/basic" []

apiShip :: (MonadIO m) => ApiT m [Ship]
apiShip = getMember "ship" []

apiPort :: (Functor m, MonadIO m, FromJSON a) => ApiT m a
apiPort = do
  fmap memberId basic
  portNum <- fmap memberId basic >>= fmap (lift . liftIO) getPortNum
  api "/api_port/port" [("api_port", portNum), ("api_sort_key", "5"), ("spi_sort_order", "2")]
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


