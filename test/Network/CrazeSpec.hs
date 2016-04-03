{-# LANGUAGE OverloadedStrings #-}

module Network.CrazeSpec where

import Test.Hspec
import Control.Concurrent
import Control.Monad.IO.Class
import Data.ByteString (ByteString, isInfixOf)
import Network.Craze
import Network.Curl
import Network.HTTP.Proxy.Server
import Network.HTTP.Base hiding (port)

oneSecond :: Int
oneSecond = 1000000

runDelayedProxy :: Integer -> Int -> ByteString -> IO ()
runDelayedProxy port delay fixedBody = proxyMain (def :: Settings ByteString)
  { responseModifier = (\_ res 
      -> threadDelay (delay * oneSecond) 
      >> return (res { rspBody = fixedBody})
      )
  , portnum = port
  , hostname = Just "localhost"
  }

racer :: Racer [(String, String)] ByteString ByteString
racer = Racer
  { racerHandler = \res -> return (respBody res)
  , racerChecker = \x -> not (isInfixOf "Something" x)
  , racerProviders
      = [pure [CurlProxy "localhost", CurlProxyPort 8082]
      , pure [CurlProxy "localhost", CurlProxyPort 8083]
      , pure [CurlProxy "localhost", CurlProxyPort 8084]
      ]
  , racerDebug = True
  }

spec :: Spec
spec = describe "Network.Craze" $
  describe "raceGet" $
    it "should race GET requests" $ do
      proxies <- mapM (liftIO . forkIO)
        [runDelayedProxy 8082 5 "Hoogle"
        ,runDelayedProxy 8083 2 "Hayoo"
        ,runDelayedProxy 8084 1 "Something"
        ]

      liftIO $ threadDelay (1 * oneSecond) >> putStrLn "Waited 1 secs..."

      response <- liftIO $ raceGet racer "http://www.google.com"

      liftIO $ mapM_ killThread proxies

      response `shouldSatisfy`
        \x -> case x of
          Just a -> isInfixOf "Hayoo" a
          Nothing -> False
