{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}

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
--                { racerProviders =
--                    [ return defaultProviderOptions
--                    , return defaultProviderOptions
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
  , RacerResult
  -- * Functions
  , defaultRacer
  , defaultProviderOptions
  , raceGet
  , raceGetResult
  -- * Providers
  , simple
  , simpleTagged
  , delayed
  , delayedTagged
  -- * Results
  , rrResponse
  , rrWinner
  , rrProviders
  ) where

import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async
import           Control.Monad            (when)
import           Data.ByteString          (ByteString)
import           Data.Default.Class       (Default, def)
import           Data.Map.Lazy            (Map, delete, elems, fromList, keys,
                                           lookup, mapWithKey)
import           Data.Monoid              ((<>))
import           Data.Text                (Text, pack)
import qualified Data.Text.IO             as TIO
import           Network.Curl
import           Prelude                  hiding (lookup)

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

-- | A function that returns the @ProviderOptions@ to use for making a request.
type RacerProvider = IO ProviderOptions

-- | Options for a specific provider.
data ProviderOptions = ProviderOptions
  { -- | Options to pass down to Curl.
    poOptions :: [CurlOption]
    -- | Number of microseconds to delay the request by.
  , poDelay   :: Maybe Int
    -- | A tag to identify this type provider.
  , poTag     :: Text
  } deriving (Show)

instance Default ProviderOptions where
  def = ProviderOptions
    { poOptions = []
    , poDelay = Nothing
    , poTag = "default"
    }

-- | The result of a racing operation. This can be used to collect statistics
-- on which providers win more often, etc.
data RacerResult a = RacerResult
  { rrResponse  :: Maybe a
  , rrWinner    :: Maybe ProviderOptions
  , rrProviders :: [RacerProvider]
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
    { racerHandler = return . respBody
    , racerChecker = const True
    , racerProviders = []
    , racerDebug = False
    , racerReturnLast = False
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

-- | A default set of options for a provider.
defaultProviderOptions :: ProviderOptions
defaultProviderOptions = def

-- | A simple provider. It does not delay requests.
simple :: [CurlOption] -> IO ProviderOptions
simple xs = pure $ def { poOptions = xs }

-- | Like @simple@, but with a tag for identification.
simpleTagged :: [CurlOption] -> Text -> IO ProviderOptions
simpleTagged xs t = do
  opts <- simple xs
  pure $ opts { poTag = t }

-- | A provider which will delay a request by the provided number of
-- microseconds.
delayed :: [CurlOption] -> Int -> IO ProviderOptions
delayed xs d = pure $ def
  { poOptions = xs
  , poDelay = Just d
  }

-- | Like @delayed@, but with a tag for identification.
delayedTagged :: [CurlOption] -> Int -> Text -> IO ProviderOptions
delayedTagged xs d t = do
  opts <- delayed xs d
  pure $ opts { poTag = t }

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
  :: (Eq a, CurlHeader ht, CurlBuffer bt)
  => Racer ht bt a
  -> URLString
  -> IO (Maybe a)
raceGet r url = rrResponse <$> raceGetResult r url

-- | Same as @raceGet@, but returns a @RacerResult@ which contains more
-- information about the race performed.
raceGetResult
  :: (Eq a, CurlHeader ht, CurlBuffer bt)
  => Racer ht bt a
  -> URLString
  -> IO (RacerResult a)
raceGetResult r url = do
  asyncs <- fromList <$> mapM performGetAsync_ (racerProviders r)

  when (racerDebug r) $ do
    TIO.putStr "[racer] Created Asyncs: "
    print . elems $ mapWithKey identifier asyncs

  maybeResponse <- waitForOne
    asyncs (racerHandler r) (racerChecker r) (racerDebug r) (racerReturnLast r)

  pure $ case maybeResponse of
    Nothing -> RacerResult
      { rrResponse = Nothing
      , rrWinner = Nothing
      , rrProviders = racerProviders r
      }
    Just (as, response) -> RacerResult
      { rrResponse = Just response
      , rrWinner = lookup as asyncs
      , rrProviders = racerProviders r
      }
    where
      performGetAsync_
        :: (CurlHeader ht, CurlBuffer bt)
        => RacerProvider
        -> IO (Async (CurlResponse_ ht bt), ProviderOptions)
      performGetAsync_ = performGetAsync url

waitForOne
  :: (Eq a)
  => Map (Async (CurlResponse_ ht bt)) ProviderOptions
  -> RacerHandler ht bt a
  -> RacerChecker a
  -> Bool
  -> Bool
  -> IO (Maybe (Async (CurlResponse_ ht bt), a))
waitForOne asyncs handler check debug returnLast
  = if null asyncs then pure Nothing else do
    winner <- waitAnyCatch (keys asyncs)

    case winner of
      (as, Right a) -> do
        result <- handler a

        let remaining = delete as asyncs

        if check result then do
          cancelAll (keys remaining)

          when debug $ do
            TIO.putStr "[racer] Winner: "
            print (asyncThreadId as)

          pure $ Just (as, result)
          else (if returnLast && null remaining
            then do
              when debug $ do
                TIO.putStr "[racer] Reached last. Returning: "
                print (asyncThreadId as)

              pure $ Just (as, result)
            else waitForOne remaining handler check debug returnLast
          )
      (as, Left _) -> waitForOne
        (delete as asyncs) handler check debug returnLast

cancelAll :: [Async a] -> IO ()
cancelAll = mapM_ (async . cancel)

identifier :: Async (CurlResponse_ ht bt) -> ProviderOptions -> Text
identifier a o = poTag o <> ":" <> (pack . show . asyncThreadId $ a)

performGetAsync
  :: (CurlHeader ht, CurlBuffer bt)
  => URLString
  -> RacerProvider
  -> IO (Async (CurlResponse_ ht bt), ProviderOptions)
performGetAsync url provider = do
  options <- provider

  responseAsync <- async $ do
    case poDelay options of
      Nothing -> pure ()
      Just delay -> threadDelay delay

    curlGetResponse_ url (poOptions options)

  pure (responseAsync, options)

