{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Kan.Api
  ( module Kan.Api.Types
  , newEnv
  , runApiT
  , api
  ) where

import Kan.Api.Types
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Error
import Data.Aeson
import Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import Data.Char (ord)
import Data.Default
import Data.Either
import Data.Monoid
import Network.HTTP.Client
import Network.HTTP.Types

runApiT :: Env -> ApiT m a -> m (Either ApiError a)
runApiT env = runErrorT . flip runReaderT env . unApiT

newEnv ::
  ByteString -> -- token
  ByteString -> -- server
  IO Env
newEnv token server = do
  manager <- newManager defaultManagerSettings
  return $ Env
    { token = token
    , server = server
    , manager = manager }

makeRequest :: (MonadReader Env m) =>
  ByteString -> -- path
  [(ByteString, ByteString)] -> -- params
  m Request

makeRequest path params = do
  (server, token) <- asks $ liftM2 (,) server token
  let extra =  [("api_token", token), ("api_verno", version)]
  return $
    urlEncodedBody (extra ++ params) $
      def
        { host = server
        , path = "/kcsapi" <> path
        , requestHeaders =(hReferer, "http://" <> server) : requestHeaders def }
  where
    version = "1"

sendRequest ::
  ( MonadReader Env (t m)
  , MonadError ApiError (t m)
  , MonadIO m
  , MonadTrans t
  ) =>
  Request -> t m (L.ByteString)
sendRequest req =
  asks manager >>=
  lift . liftIO . try . httpLbs req >>=
  either (throwError . HttpError) (return . responseBody)

parseResponse :: (MonadError ApiError m, FromJSON a) => L.ByteString -> m (ApiResponse a)
parseResponse body = do
  let svdata = L.dropWhile (/= (fromIntegral . ord) '{') body
  either (throwError . ParseError) return $ eitherDecode svdata

api :: (MonadIO m, FromJSON a) =>
  ByteString -> -- path
  [(ByteString, ByteString)] -> -- params
  ApiT m a
api path params = do
  response <- makeRequest path params >>= sendRequest >>= parseResponse
  when (apiResult response /= 1) $ throwError (KCSError (apiResultMsg response))
  maybe (throwError undefined) return (apiData response)

