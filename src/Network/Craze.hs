{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Craze is a small module for performing multiple similar HTTP GET requests
-- in parallel. This is performed through the `raceGet` function, which will
-- perform all the requests and pick the first successful response that passes
-- a certain check, menaing that the parallel requests are essentially racing
-- against each other.
--
-- __What is the usefulness of this?__
--
-- If you are dealing with data source or API that is very unreliable (high
-- latency, random failures) and there are no limitations or consequences on
-- perfoming significantly more requests, then performing multiple requests
-- (through direct connections, proxies, VPNs) may increase the chances of
-- getting a successful response faster and more reliably.
--
-- However, if using a different data source or transport is a possibility, it
-- is potentially a better option that this approach.
--
-- __Examples:__
--
-- Performing two parallel GET requests against https://chromabits.com and
-- returning the status code of the first successful one:
--
-- >>> :{
--  let racer = (Racer
--                { racerProviders = [return [], return []]
--                , racerHandler = return . respStatus
--                , racerChecker = (200 ==)
--                , racerDebug = False
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
  -- * Functions
  , defaultRacer
  , raceGet
  ) where

import Data.ByteString (ByteString)
import Data.Default.Class (Default, def)
import Network.Curl
import Control.Concurrent.Async

-- | A `RacerHandler` is simply a function for transforming a response after it
-- is received. The handler is only applied to successful requests before they
-- are checked by the `RacerChecker`.
--
-- This is primarily for extracting or parsing a `CurlResponse_` before doing
-- any further work. The type returned by the handler will be used as the
-- input of the checker and will be the return type of `raceGet`.
type RacerHandler headerTy bodyTy a = CurlResponse_ headerTy bodyTy -> IO a

-- | A function that computes whether or not a result is valid or not.
-- Successful responses that do not pass the checker are discarded.
--
-- This should help filtering out successful responses that do not, for some
-- reason, have the expected result (e.g. Random content changes, Rate
-- Limitting, etc).
type RacerChecker a = a -> Bool

-- | A function that returns a list of `CurlOption`s to use for making a
-- request.
type RacerProvider = IO [CurlOption]

-- | A record describing the rules for racing requests.
data Racer headerTy bodyTy a = Racer
  { racerHandler :: RacerHandler headerTy bodyTy a
  , racerChecker :: RacerChecker a
  -- | On a `Racer`, each `RaceProvider` represents a separate client
  -- configuration. When performing a race, each provider will be used to spwan
  -- a client and perform a request. This allows one to control the number of
  -- requests performed and with which `CurlOption`s.
  , racerProviders :: [RacerProvider]
  -- | When set to `True`, debugging messages will be written to stdout.
  , racerDebug :: Bool
  }

instance Default (Racer [(String,String)] ByteString ByteString) where
  def = Racer
    { racerHandler = return . respBody
    , racerChecker = const True
    , racerProviders = []
    , racerDebug = False
    }

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
--         - If the check fails, go back to waiting for another request to finish.
--
--     * If the request fails, go back to waiting for another request to finish.
--
raceGet
  :: (Eq a, CurlHeader ht, CurlBuffer bt)
  => Racer ht bt a
  -> URLString
  -> IO (Maybe a)
raceGet r url = do
  asyncs <- mapM performGetAsync_ (racerProviders r)

  if (racerDebug r) 
  then do
    putStr "[racer] Created Asyncs: "
    print (map asyncThreadId asyncs)
  else return ()

  waitForOne asyncs (racerHandler r) (racerChecker r) (racerDebug r)
    where
      performGetAsync_
        :: (CurlHeader ht, CurlBuffer bt)
        => RacerProvider
        -> IO (Async (CurlResponse_ ht bt))
      performGetAsync_ = performGetAsync url

waitForOne
  :: (Eq a)
  => [Async (CurlResponse_ ht bt)]
  -> RacerHandler ht bt a
  -> RacerChecker a
  -> Bool
  -> IO (Maybe a)
waitForOne asyncs handler check debug
  = if null asyncs then pure Nothing else do
    winner <- waitAnyCatch asyncs

    case winner of
      (as, Right a) -> do
        result <- handler a

        if check result then do
          cancelAll (except as asyncs)

          if debug 
          then do
            putStr "[racer] Winner: "
            print (asyncThreadId as)
          else return ()

          pure $ Just result
        else waitForOne (except as asyncs) handler check debug
      (as, Left _) -> waitForOne (except as asyncs) handler check debug

cancelAll :: [Async a] -> IO ()
cancelAll = mapM_ (async . cancel)

except :: (Eq a) => a -> [a] -> [a]
except x xs = filter (x /=) xs

performGetAsync
  :: (CurlHeader ht, CurlBuffer bt)
  => URLString
  -> RacerProvider
  -> IO (Async (CurlResponse_ ht bt))
performGetAsync url provider = async $ provider >>= curlGetResponse_ url

