{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Kan.Api.Types where
import Data.Aeson
import Data.ByteString.Char8
import Control.Applicative
import Control.Monad.Trans
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
  { unApiT :: ReaderT Env (ErrorT ApiError m) a
  } deriving (Functor, Monad, MonadReader Env, MonadError ApiError, MonadIO)

instance MonadTrans ApiT where
  lift = ApiT . lift . lift 

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

