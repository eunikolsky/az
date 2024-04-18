#!/usr/bin/env stack
-- stack script --resolver=lts-22.17

{-# OPTIONS_GHC -Wall #-}

import Conduit
import Control.Monad
import Network.HTTP.Client
import Network.HTTP.Simple
import System.Directory

type URL = String

getFile :: URL -> FilePath -> IO ()
getFile url file = do
  exists <- doesFileExist file
  unless exists $ do
    req <- parseUrlThrow url
    runResourceT $ httpSink req $ \_ -> sinkFile file

main :: IO ()
main = do
  getFile "https://www.az511.gov/map/mapIcons/Incidents" "incidents.json"
  getFile "https://www.az511.gov/map/mapIcons/Cameras" "cameras.json"
