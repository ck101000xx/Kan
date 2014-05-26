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

