{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Kan.Api.Types where
import Data.Aeson
import Data.ByteString.Char8
import Control.Applicative
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Trans
import System.Log.FastLogger (LogStr)
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Error
import Network.HTTP.Client (Manager, HttpException)

type Token = ByteString

data ApiError
  = KCSError String
  | ParseError String
  | HttpError HttpException
  | UnknownError String
  deriving (Show)

instance Error ApiError where
  strMsg = UnknownError

data Env = Env
  { manager :: Manager
  , token :: ByteString
  , server :: ByteString
  }

newtype ApiT m a = ApiT
  { unApiT :: ReaderT Env (LoggingT (ErrorT ApiError m)) a
  } deriving (Functor, Applicative, Monad, MonadReader Env, MonadError ApiError, MonadLogger)

runApiT :: (MonadLogger m) =>
  ApiT m a ->
  Env ->
  (Loc -> LogSource -> LogLevel -> LogStr -> IO ()) ->
  m (Either ApiError a)
runApiT action env logger = runErrorT . flip runLoggingT logger . flip runReaderT env . unApiT $ action

instance MonadTrans ApiT where
  lift = ApiT . lift . lift . lift 

instance (MonadBase b m) => MonadBase b (ApiT m) where
  liftBase = liftBaseDefault

instance MonadTransControl ApiT where
  newtype StT ApiT a = StApi {unStApi :: StT (ErrorT ApiError) (StT LoggingT (StT (ReaderT Env) a)) }
  liftWith f = ApiT $
    liftWith $ \runStReader ->
      liftWith $ \runStLogging ->
        liftWith $ \runStError ->
          f $ (liftM StApi . runStError . runStLogging . runStReader . unApiT)
  restoreT = ApiT . restoreT . restoreT . restoreT . liftM unStApi

instance MonadBaseControl b m => MonadBaseControl b (ApiT m) where
  newtype StM (ApiT m) a = StMApi {unStMApi :: ComposeSt ApiT m a}
  liftBaseWith = defaultLiftBaseWith StMApi
  restoreM     = defaultRestoreM   unStMApi

data ApiResponse a = ApiResponse
  { apiResult :: Int
  , apiResultMsg :: String
  , apiData :: Maybe a
  }

instance (FromJSON a) => FromJSON (ApiResponse a) where
  parseJSON (Object v) =
    ApiResponse <$>
      v .: "api_result" <*>
      v .: "api_result_msg" <*>
      v .:? "api_data"

