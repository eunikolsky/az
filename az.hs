#!/usr/bin/env stack
-- stack script --resolver=lts-22.17 --package=aeson --package=bytestring --package=conduit --package=directory --package=microlens --package=microlens-aeson --package=http-client --package=http-conduit --package=text --package=vector

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

import Conduit
import Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Vector qualified as V ((!))
import Lens.Micro
import Lens.Micro.Aeson
import Network.HTTP.Client
import Network.HTTP.Simple
import System.Directory

type URL = String

getFile :: URL -> FilePath -> IO ByteString
getFile url file = do
  putStrLn $ mconcat [url, " → ", file]
  exists <- doesFileExist file
  if exists
    then putStrLn "already exists"
    else do
      req <- parseUrlThrow url
      runResourceT $ httpSink req $ \_ -> sinkFile file

  BS.readFile file

data Coord = Coord { _lat :: !Double, _long :: !Double }

instance FromJSON Coord where
  parseJSON = withArray "Coord" $ \a ->
    flip (withScientific "lat") (a V.! 0) $ \lat ->
    flip (withScientific "long") (a V.! 1) $ \long ->
    pure $ Coord (realToFrac lat) (realToFrac long)

type ItemId = Text

data Item = Item !ItemId !Coord

instance FromJSON Item where
  parseJSON = withObject "Item" $ \o -> Item
    <$> o .: "itemId"
    <*> o .: "location"

loadJSON :: URL -> FilePath -> IO [Item]
loadJSON url file = do
  bs <- getFile url file
  case traverse fromJSON $ bs ^? key "item2" ^.. _Just . values of
    Success items -> pure items
    Error err -> error err

main :: IO ()
main = do
  let count itemType url file = putStrLn . (<> " " <> itemType) . show . length =<< loadJSON url file
  count "incidents" "https://www.az511.gov/map/mapIcons/Incidents" "incidents.json"
  count "cameras" "https://www.az511.gov/map/mapIcons/Cameras" "cameras.json"
