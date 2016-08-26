{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE ImpredicativeTypes    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}

-- | Craze is a small module for performing multiple similar HTTP GET requests
-- in parallel. This is performed through the `raceGet` function, which will
-- perform all the requests and pick the first successful response that passes
-- a certain check, meaning that the parallel requests are essentially racing
-- against each other.
--
-- __What is the usefulness of this?__
--
-- If you are dealing with data source or API that is very unreliable (high
-- latency, random failures) and there are no limitations on performing
-- significantly more requests, then performing multiple requests (through
-- direct connections, proxies, VPNs) may increase the chances of getting a
-- successful response faster and more reliably.
--
-- However, if using a different data source or transport is a possibility, it
-- is potentially a better option that this approach.
--
-- __Examples:__
--
-- Performing two parallel GET requests against https://chromabits.com and
-- returning the status code of the first successful one:
--
-- The providers generate two client configurations. The handler "parses" the
-- response (in this case it just gets the status code). Finally, the checker
-- filters out responses that we don't consider valid (anything that is not
-- HTTP 200 in this case).
--
-- >>> :set -XOverloadedStrings
-- >>> :{
--  let racer = (Racer
--                { racerProviders =
--                    [ simpleTagged [] "Client A"
--                    , simpleTagged [] "Client B"
--                    ]
--                , racerHandler = return . respStatus
--                , racerChecker = (200 ==)
--                , racerDebug = False
--                , racerReturnLast = False
--                } :: Racer [(String, String)] ByteString Int)
--  in (raceGet racer "https://chromabits.com" >>= print)
-- :}
-- Just 200
--
module Network.Craze (
  -- * Types
    RacerHandler
  , RacerChecker
  , Racer(..)
  , RacerProvider
  , ProviderOptions(..)
  , RacerResult(..)
  , ClientStatus(..)
  -- * Functions
  , raceGet
  , raceGetResult
  -- * Providers
  -- $providers
  , simple
  , simpleTagged
  , delayed
  , delayedTagged
  -- * Deprecated
  , defaultRacer
  , defaultProviderOptions
  ) where

import Control.Monad (when)
import Data.Map.Lazy (keys, lookup)
import Prelude       hiding (lookup)

import           Control.Concurrent.Async
import           Control.Monad.State      (runStateT)
import           Control.Monad.Trans      (MonadIO, liftIO)
import           Data.ByteString          (ByteString)
import           Data.Default.Class       (def)
import           Data.Text                (Text, pack)
import qualified Data.Text.IO             as TIO
import           Network.Curl

import Network.Craze.Internal
import Network.Craze.Types

-- | Perform a GET request on the provided URL using all providers in
-- parallel.
--
-- Rough summary of the algorithm:
--
-- - Start all requests
-- - Wait for a request to finish.
--
--     * If the request is successful, apply the handler on it.
--
--         - If the result of the handler passes the checker, cancel all other
--           requests, and return the result.
--         - If the check fails, go back to waiting for another request to
--           finish.
--
--     * If the request fails, go back to waiting for another request to
--       finish.
--
raceGet
  :: (Eq a, CurlHeader ht, CurlBuffer bt, MonadIO m)
  => Racer ht bt a
  -> URLString
  -> m (Maybe a)
raceGet r url = rrResponse <$> raceGetResult r url

-- | Same as @raceGet@, but returns a @RacerResult@ which contains more
-- information about the race performed.
raceGetResult
  :: (Eq a, CurlHeader ht, CurlBuffer bt, MonadIO m)
  => Racer ht bt a
  -> URLString
  -> m (RacerResult a)
raceGetResult r@Racer{..} url = do
  initialState@RaceState{..} <- makeRaceState (pack url) r

  let asyncs = keys _rsClientMap

  when racerDebug . liftIO $ do
    TIO.putStr "[racer] Created Asyncs: "
    print $ asyncThreadId <$> asyncs

  (maybeResponse, finalState) <- runStateT waitForOne initialState

  pure $ case maybeResponse of
    Nothing -> RacerResult
      { rrResponse = Nothing
      , rrWinner = Nothing
      , rrProviders = racerProviders
      , rrStatuses = extractStatuses finalState
      }
    Just (as, response) -> RacerResult
      { rrResponse = Just response
      , rrWinner = _csOptions <$> lookup as _rsClientMap
      , rrProviders = racerProviders
      , rrStatuses = extractStatuses finalState
      }

-- $providers
--
-- 'RacerProvider' provide client configurations. Craze comes bundled with a
-- few built-in providers which can be used for quickly building client
-- configurations.

-- | A simple provider. It does not delay requests.
simple :: Monad m => [CurlOption] -> m ProviderOptions
simple xs = pure $ def { poOptions = xs }

-- | Like @simple@, but with a tag for identification.
simpleTagged :: Monad m => [CurlOption] -> Text -> m ProviderOptions
simpleTagged xs t = do
  opts <- simple xs
  pure $ opts { poTag = t }

-- | A provider which will delay a request by the provided number of
-- microseconds.
delayed :: Monad m => [CurlOption] -> Int -> m ProviderOptions
delayed xs d = pure $ def
  { poOptions = xs
  , poDelay = Just d
  }

-- | Like @delayed@, but with a tag for identification.
delayedTagged :: Monad m => [CurlOption] -> Int -> Text -> m ProviderOptions
delayedTagged xs d t = do
  opts <- delayed xs d
  pure $ opts { poTag = t }

-- | A `Racer` with some default values.
--
-- __Note:__ The handler will extract the response body as a `ByteString` and
-- ignore everything else, hence the type:
--
-- @
-- Racer [(String, String)] ByteString ByteString
-- @
--
-- If this is not the desired behavior, or if the response should be parsed or
-- processed, you should use the `Racer` constructor directly and provide all
-- fields.
defaultRacer :: Racer [(String,String)] ByteString ByteString
defaultRacer = def
{-# DEPRECATED defaultRacer "Use Data.Default.Class.def instead" #-}

-- | A default set of options for a provider.
defaultProviderOptions :: ProviderOptions
defaultProviderOptions = def
{-# DEPRECATED defaultProviderOptions "Use Data.Default.Class.def instead" #-}
