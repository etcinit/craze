{-# LANGUAGE OverloadedStrings #-}

module Network.CrazeSpec where

import Control.Concurrent
import Control.Monad.IO.Class
import Data.ByteString           (ByteString, isInfixOf)
import Network.Craze
import Network.Curl
import Network.HTTP.Base         hiding (port)
import Network.HTTP.Proxy.Server
import Test.Hspec

oneSecond :: Int
oneSecond = 1000000

runDelayedProxy :: Integer -> Int -> ByteString -> IO ()
runDelayedProxy port delay fixedBody = proxyMain (def :: Settings ByteString)
  { responseModifier = \_ res
      -> threadDelay (delay * oneSecond)
      >> return (res { rspBody = fixedBody})
  , portnum = port
  , hostname = Just "localhost"
  }

racer :: Racer [(String, String)] ByteString ByteString
racer = Racer
  { racerHandler = pure . respBody
  , racerChecker = not . isInfixOf "Something"
  , racerProviders
      = [simple [CurlProxy "localhost", CurlProxyPort 8082]
      , simple [CurlProxy "localhost", CurlProxyPort 8083]
      , simple [CurlProxy "localhost", CurlProxyPort 8084]
      , delayed [CurlProxy "localhost", CurlProxyPort 8085] 2000000
      ]
  , racerDebug = True
  , racerReturnLast = False
  }

failingRacer :: Racer [(String, String)] ByteString ByteString
failingRacer = racer
  {  racerChecker = isInfixOf "OMG WHY"
  ,  racerReturnLast = True
  ,  racerProviders
       = [simple [CurlProxy "localhost", CurlProxyPort 8086]
       , simple [CurlProxy "localhost", CurlProxyPort 8087]
       , simple [CurlProxy "localhost", CurlProxyPort 8088]
       , delayed [CurlProxy "localhost", CurlProxyPort 8089] 2000000
       ]
  }

spec :: Spec
spec = describe "Network.Craze" $
  describe "raceGet" $ do
    it "should race GET requests" $ do
      proxies <- mapM (liftIO . forkIO)
        [ runDelayedProxy 8082 5 "Hoogle"
        , runDelayedProxy 8083 2 "Hayoo"
        , runDelayedProxy 8084 1 "Something"
        , runDelayedProxy 8085 1 "Slow"
        ]

      liftIO $ threadDelay (1 * oneSecond) >> putStrLn "Waited 1 secs..."

      response <- liftIO $ raceGet racer "http://www.google.com"

      liftIO $ mapM_ killThread proxies

      response `shouldSatisfy`
        \x -> case x of
          Just a -> "Hayoo" `isInfixOf` a
          Nothing -> False
    it "should return the last when requested" $ do
      proxies <- mapM (liftIO . forkIO)
        [ runDelayedProxy 8086 10 "Hoogle"
        , runDelayedProxy 8087 2 "Hayoo"
        , runDelayedProxy 8088 1 "Something"
        , runDelayedProxy 8089 1 "Slow"
        ]

      liftIO $ threadDelay (1 * oneSecond) >> putStrLn "Waited 1 secs..."

      response <- liftIO $ raceGet failingRacer "http://www.google.com"

      liftIO $ mapM_ killThread proxies

      response `shouldSatisfy`
        \x -> case x of
          Just a -> "Hoogle" `isInfixOf` a
          Nothing -> False

