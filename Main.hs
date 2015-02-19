{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString.Char8 as C8 -- pack (String -> ByteString)
import Control.Applicative -- <$>
import Network.Wreq
import Control.Lens -- &, ?~, and ^.
import Data.Yaml

data MyConfig = MyConfig {
    apiKey :: String
  } deriving Show

instance FromJSON MyConfig where
  parseJSON (Object m) = MyConfig <$> m .: "apiKey"
  parseJSON x = fail ("not an object: " ++ show x)

loadConfig :: String -> IO MyConfig
loadConfig file =
  either (error . show) id <$>
  decodeFileEither file

main :: IO ()
main = do
  config <- loadConfig "./config.yml"
  let opts = defaults & auth ?~ basicAuth (C8.pack $ apiKey config) ""
  r <- getWith opts "https://app.asana.com/api/1.0/users/me"
  print $ show $ r ^. responseBody
