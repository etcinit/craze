{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}

module Network.Craze.Internal where

import           Control.Exception (SomeException)
import           Control.Monad     (forM, when)
import           Data.Map.Lazy     (Map)
import qualified Data.Map.Lazy     as M
import           Data.Monoid       ((<>))

import           Control.Concurrent.Async.Lifted (Async, async, asyncThreadId,
                                                  cancel, waitAnyCatch)
import           Control.Concurrent.Lifted       (threadDelay)
import           Control.Lens                    (at, makeLenses, use, (&),
                                                  (.~), (?=))
import           Control.Monad.State             (MonadState)
import           Control.Monad.Trans             (MonadIO, liftIO)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import qualified Data.Text.IO                    as TIO
import           Network.Curl                    (CurlBuffer, CurlHeader,
                                                  CurlResponse_,
                                                  curlGetResponse_)

import Network.Craze.Types

type ClientMap ht bt a = Map (Async (CurlResponse_ ht bt)) (ClientState a)

data ClientState a = ClientState
  { _csOptions :: ProviderOptions
  , _csStatus  :: ClientStatus a
  }

data RaceState ht bt a = RaceState
  { _rsClientMap  :: ClientMap ht bt a
  , _rsChecker    :: RacerChecker a
  , _rsHandler    :: RacerHandler ht bt a
  , _rsDebug      :: Bool
  , _rsReturnLast :: Bool
  }

makeLenses ''ClientState
makeLenses ''RaceState

extractStatuses :: RaceState ht bt a -> [(Text, ClientStatus a)]
extractStatuses RaceState{..} = M.elems $ makeTuple <$>  _rsClientMap
  where
    makeTuple :: ClientState a -> (Text, ClientStatus a)
    makeTuple ClientState{..} = (poTag _csOptions, _csStatus)

makeRaceState
  :: (CurlHeader ht, CurlBuffer bt)
  => Text
  -> Racer ht bt a
  -> IO (RaceState ht bt a)
makeRaceState url Racer{..} = do
  providerMap <- makeClientMap url racerProviders

  pure $ RaceState
    providerMap
    racerChecker
    racerHandler
    racerDebug
    racerReturnLast

makeClientMap
  :: (CurlHeader ht, CurlBuffer bt)
  => Text
  -> [RacerProvider]
  -> IO (ClientMap ht bt a)
makeClientMap url providers = M.fromList <$> forM providers (makeClient url)

makeClient
  :: (CurlHeader ht, CurlBuffer bt)
  => Text
  -> RacerProvider
  -> IO (Async (CurlResponse_ ht bt), ClientState a)
makeClient url provider = do
  options <- provider
  future <- async $ performGet url options

  pure (future, ClientState options Pending)

performGet
  :: (CurlHeader ht, CurlBuffer bt)
  => Text
  -> ProviderOptions
  -> IO (CurlResponse_ ht bt)
performGet url ProviderOptions{..} = do
  case poDelay of
    Nothing -> pure ()
    Just delay -> threadDelay delay

  curlGetResponse_ (T.unpack url) poOptions

cancelAll :: MonadIO m => [Async a] -> m ()
cancelAll = liftIO . mapM_ (async . cancel)

cancelRemaining
  :: (MonadIO m, MonadState (RaceState ht bt a) m)
  => m ()
cancelRemaining = do
  remaining <- onlyPending <$> use rsClientMap

  cancelAll $ M.keys remaining

identifier :: Async (CurlResponse_ ht bt) -> ProviderOptions -> Text
identifier a o = poTag o <> ":" <> (T.pack . show . asyncThreadId $ a)

onlyPending :: ClientMap ht bt a -> ClientMap ht bt a
onlyPending = M.filter (isPending . _csStatus)

isPending :: ClientStatus a -> Bool
isPending Pending = True
isPending _ = False

markAsSuccessful
  :: (MonadState (RaceState ht bt a) m)
  => Async (CurlResponse_ ht bt)
  -> a
  -> m ()
markAsSuccessful key result = do
  maybePrevious <- use $ rsClientMap . at key

  case maybePrevious of
    Just previous -> (rsClientMap . at key)
      ?= (previous & csStatus .~ Successful result)
    Nothing -> pure ()

markAsFailure
  :: (MonadState (RaceState ht bt a) m)
  => Async (CurlResponse_ ht bt)
  -> a
  -> m ()
markAsFailure key result = do
  maybePrevious <- use $ rsClientMap . at key

  case maybePrevious of
    Just previous -> (rsClientMap . at key)
      ?= (previous & csStatus .~ Failed result)
    Nothing -> pure ()

markAsErrored
  :: (MonadState (RaceState ht bt a) m)
  => Async (CurlResponse_ ht bt)
  -> SomeException
  -> m ()
markAsErrored key result = do
  maybePrevious <- use $ rsClientMap . at key

  case maybePrevious of
    Just previous -> (rsClientMap . at key)
      ?= (previous & csStatus .~ Errored result)
    Nothing -> pure ()

waitForOne
  :: (Eq a, MonadIO m, MonadState (RaceState ht bt a) m)
  => m (Maybe (Async (CurlResponse_ ht bt), a))
waitForOne = do
  debug <- use rsDebug
  providerMap <- use rsClientMap

  let asyncs = _csOptions <$> onlyPending providerMap

  if null asyncs then pure Nothing else do
    winner <- liftIO $ waitAnyCatch (M.keys asyncs)

    case winner of
      (as, Right a) -> do
        handler <- use rsHandler
        check <- use rsChecker
        returnLast <- use rsReturnLast
        result <- liftIO $ handler a

        if check result then do
          markAsSuccessful as result
          cancelRemaining

          when debug . liftIO $ do
            TIO.putStr "[racer] Winner: "
            print (asyncThreadId as)

          pure $ Just (as, result)
          else do
            markAsFailure as result

            remaining <- M.keys . onlyPending <$> use rsClientMap

            if returnLast && null remaining
              then do
                when debug . liftIO $ do
                  TIO.putStr "[racer] Reached last. Returning: "
                  print (asyncThreadId as)

                pure $ Just (as, result)
              else waitForOne
      (as, Left ex) -> markAsErrored as ex >> waitForOne
