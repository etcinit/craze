{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Craze.Types where

import Control.Exception (SomeException)

import Data.ByteString    (ByteString)
import Data.Default.Class (Default, def)
import Data.Text          (Text)
import Network.Curl       (CurlOption, CurlResponse_, respBody)

-- | A 'RacerHandler' is simply a function for transforming a response after it
-- is received. The handler is only applied to successful requests before they
-- are checked by the 'RacerChecker'.
--
-- This is primarily for extracting or parsing a 'CurlResponse_' before doing
-- any further work. The type returned by the handler will be used as the
-- input of the checker and will be the return type of functions like
-- 'raceGet'.
--
type RacerHandler headerTy bodyTy a = CurlResponse_ headerTy bodyTy -> IO a

-- | A function that computes whether or not a result is valid or not.
--
-- A racer will discard successful responses it get from its clients if they do
-- not pass the checker.
--
-- This step allows the racer to potentially discard responses that, while
-- technically successful, do not contain the expected result (e.g. APIs that
-- return errors as HTTP 200s, rate limitting messages, or unexpected formats).
--
type RacerChecker a = a -> Bool

-- | A provider is simply a factory function for 'ProviderOptions', which are
-- used to configure a client.
type RacerProvider = IO ProviderOptions

-- | Configuration used to set up an individual client in the race.
data ProviderOptions = ProviderOptions
  { -- | Options to pass down to Curl.
    poOptions :: [CurlOption]
    -- | Number of microseconds to delay the request by.
    --
    -- Delays can be used to give other clients a headstart. This is useful
    -- in cases were some clients are more costly to use than others (e.g.
    -- Bandwidth costs, resource usage, etc).
    --
  , poDelay   :: Maybe Int
    -- | A tag to identify this type provider.
    --
    -- Tags are not required to be unique, but they are generally more helpful
    -- if they are.
  , poTag     :: Text
  } deriving (Show)

instance Default ProviderOptions where
  def = ProviderOptions
    { poOptions = []
    , poDelay = Nothing
    , poTag = "default"
    }

-- | The status of running a single client.
data ClientStatus a
  -- | A successful response (passed the checker). A race will usually only
  -- have one successful response.
  = Successful a
  -- | A response that was received but failed to pass the checker.
  | Failed a
  -- | An exception thrown while using the client.
  | Errored SomeException
  -- | The operation is still pending, was cancelled, or was never started.
  | Pending
  deriving (Show)

-- | The result of a racing operation. This can be used to collect statistics
-- on which providers win more often, etc.
data RacerResult a = RacerResult
  { rrResponse  :: Maybe a
  , rrWinner    :: Maybe ProviderOptions
  , rrProviders :: [RacerProvider]
  , rrStatuses  :: [(Text, ClientStatus a)]
  }

-- | A record describing the rules for racing requests.
data Racer headerTy bodyTy a = Racer
  { racerHandler    :: RacerHandler headerTy bodyTy a
  , racerChecker    :: RacerChecker a
  -- | On a `Racer`, each `RaceProvider` represents a separate client
  -- configuration. When performing a race, each provider will be used to spwan
  -- a client and perform a request. This allows one to control the number of
  -- requests performed and with which `CurlOption`s.
  , racerProviders  :: [RacerProvider]
  -- | When set to `True`, debugging messages will be written to stdout.
  , racerDebug      :: Bool
  -- | When set to `True`, the Racer will attempt to return the last response
  -- in the event that all responses failed to pass the checker. This can be
  -- used for identifying error conditions.
  , racerReturnLast :: Bool
  }

instance Default (Racer [(String,String)] ByteString ByteString) where
  def = Racer
    { racerHandler = pure . respBody
    , racerChecker = const True
    , racerProviders = []
    , racerDebug = False
    , racerReturnLast = False
    }
