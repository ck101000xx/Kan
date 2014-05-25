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
import Data.Time.Clock.POSIX

start2 :: (MonadIO m, FromJSON a) => ApiT m a
start2 = api "/api_start2" []

basic :: (MonadIO m) => ApiT m Basic
basic = api "/api_get_member/basic" []

port :: (Functor m, MonadIO m, FromJSON a) => ApiT m a
port = do
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


