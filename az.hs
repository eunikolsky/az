#!/usr/bin/env stack
-- stack script --resolver=lts-22.17 --package=aeson --package=bytestring --package=conduit --package=directory --package=filepath --package=microlens --package=microlens-aeson --package=http-client --package=http-conduit --package=text --package=vector

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

import Conduit
import Control.Monad
import Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V ((!))
import Lens.Micro
import Lens.Micro.Aeson
import Network.HTTP.Client
import Network.HTTP.Simple
import System.Directory
import System.FilePath

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

data Coord = Coord { lat :: !Double, long :: !Double }
  deriving Show

instance FromJSON Coord where
  parseJSON = withArray "Coord" $ \a ->
    flip (withScientific "lat") (a V.! 0) $ \lat ->
    flip (withScientific "long") (a V.! 1) $ \long ->
    pure $ Coord (realToFrac lat) (realToFrac long)

type ItemId = Text

data Item = Item { iId :: !ItemId, iLocation :: !Coord }
  deriving Show

instance FromJSON Item where
  parseJSON = withObject "Item" $ \o -> Item
    <$> o .: "itemId"
    <*> o .: "location"

type Distance = Double -- km

-- https://stackoverflow.com/questions/27928/calculate-distance-between-two-latitude-longitude-points-haversine-formula/21623206#21623206
haversineDistance :: Coord -> Coord -> Distance
haversineDistance c0 c1 =
  let r = 6371
      p = pi/180
      a = 0.5 - cos((lat c1 - lat c0) * p) / 2 + cos(lat c0 * p) * cos(lat c1 * p) * (1 - cos((long c1 - long c0) * p)) / 2
  in 2 * r * asin(sqrt a)

loadJSON :: URL -> FilePath -> IO [Item]
loadJSON url file = do
  bs <- getFile url file
  case traverse fromJSON $ bs ^? key "item2" ^.. _Just . values of
    Success items -> pure items
    Error err -> error err

loadIncidents :: IO [Incident]
loadIncidents = fmap Incident <$> loadJSON "https://www.az511.gov/map/mapIcons/Incidents" "incidents.json"

loadCameras :: IO [Camera]
loadCameras = fmap Camera <$> loadJSON "https://www.az511.gov/map/mapIcons/Cameras" "cameras.json"

newtype Incident = Incident { incidentItem :: Item }
  deriving Show

newtype Camera = Camera { cameraItem :: Item }
  deriving Show

findCloseCoords :: Distance -> [Incident] -> [Camera] -> [(Incident, Camera, Distance)]
findCloseCoords maxDist xs cameras = do
  i@(Incident x) <- xs
  c@(Camera y) <- cameras
  let dist = iLocation x `haversineDistance` iLocation y
  guard $ dist <= maxDist
  pure (i, c, dist)

downloadCameraImage :: Camera -> IO ()
downloadCameraImage Camera{cameraItem=Item{iId}} = do
  let iIdS = T.unpack iId
  bs <- getFile ("https://www.az511.gov/map/data/Cameras/" <> iIdS) (iIdS <.> "json")
  let url = bs ^?! nth 0 . key "imageUrl" . _String
      urlS = T.unpack url
      filename = takeFileName urlS
  void $ getFile urlS filename

main :: IO ()
main = do
  incidents <- loadIncidents
  cameras <- loadCameras
  putStrLn $ mconcat [show $ length incidents, " incidents, ", show $ length cameras, " cameras"]

  let closeItems = findCloseCoords 0.2 incidents cameras
  print closeItems

  traverse_ downloadCameraImage $ closeItems ^.. each._2
