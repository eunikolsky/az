#!/usr/bin/env stack
-- stack script --resolver=lts-22.17

{-# OPTIONS_GHC -Wall #-}

import Conduit
import Network.HTTP.Client
import Network.HTTP.Simple

type URL = String

getFile :: URL -> FilePath -> IO ()
getFile url file = do
  req <- parseUrlThrow url
  runResourceT $ httpSink req $ \_ -> sinkFile file

main :: IO ()
main = do
  getFile "https://www.az511.gov/map/mapIcons/Incidents" "incidents.json"
  getFile "https://www.az511.gov/map/mapIcons/Cameras" "cameras.json"
