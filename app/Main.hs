{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Main (main) where

import Data.Text (Text)
import Network.Craze
import Options.Generic (ParseRecord, Generic, getRecord, unHelpful, (<?>)(..))

data Options = Options
  { clients :: Int <?> "Number of concurrent clients to use."
  , targetUri :: String <?> "Target URI for the GET request."
  , debug :: Bool
  }
  deriving (Generic, Show)

instance ParseRecord Options

getOptions :: Text -> IO Options
getOptions = getRecord

main :: IO ()
main = do
  opts <- getOptions "Craze"

  let clientCount = (unHelpful . clients) opts
  let racer = defaultRacer
               { racerProviders = map ((const . return) []) [1..clientCount]
               , racerDebug = (debug opts)
               }

  result <- raceGet racer ((unHelpful . targetUri) opts)

  print result
  return ()

