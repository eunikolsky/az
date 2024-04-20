#!/usr/bin/env stack
-- stack script --resolver=lts-22.17 --package=aeson --package=blaze-html --package=bytestring --package=conduit --package=containers --package=directory --package=filepath --package=microlens --package=microlens-aeson --package=http-client --package=http-conduit --package=http-types --package=text --package=time --package=vector

{-# OPTIONS_GHC -Wall -Wprepositive-qualified-module #-}
{-# LANGUAGE DerivingStrategies, OverloadedStrings #-}

import Conduit
import Control.Monad
import Data.Aeson
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Lazy qualified as L
import Data.List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
import Data.Vector qualified as V ((!))
import Data.Version
import Lens.Micro
import Lens.Micro.Aeson
import Network.HTTP.Client
import Network.HTTP.Simple
import Network.HTTP.Types.Status
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import Text.Blaze.Html.Renderer.Utf8
import Text.Blaze.Html5 ((!), Html)
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes as A

version :: Version
version = makeVersion [0, 1, 0]

type URL = String

getFile :: URL -> FilePath -> IO ByteString
getFile url file = do
  exists <- doesFileExist file
  reqHeaders <- if exists
    then do
      modTime <- getModificationTime file
      let timeStr = formatTime defaultTimeLocale rfc822DateFormat modTime
      pure [("If-Modified-Since", C8.pack timeStr)]
    else pure mempty
  putStrLn . mconcat $ [url, " → ", file]
    <> if null reqHeaders then mempty else [" ", show reqHeaders]

  req <- do
    r <- parseUrlThrow url
    pure r { requestHeaders = reqHeaders }
  runResourceT $ httpSink req $ \res ->
    if responseStatus res == notModified304
      then liftIO (putStrLn "not modified")
      else do
        sinkFile file
        let maybeLastModifiedStr = listToMaybe $ getResponseHeader "Last-Modified" res
        liftIO . putStrLn $ maybe "no Last-Modified" (("Last modified: " <>) . C8.unpack) maybeLastModifiedStr
        forM_ maybeLastModifiedStr $ setModTime file

  BS.readFile file

setModTime :: (MonadIO m, MonadFail m) => FilePath -> ByteString -> m ()
setModTime file bs = do
  let timeStr = C8.unpack bs
  modTime <- parseTimeM False defaultTimeLocale rfc822DateFormat timeStr
  liftIO $ setModificationTime file modTime

data Coord = Coord { lat :: !Double, long :: !Double }
  deriving (Eq, Ord)

instance Show Coord where
  show (Coord lat long) = mconcat ["(", show lat, ", ", show long, ")"]

instance FromJSON Coord where
  parseJSON = withArray "Coord" $ \a ->
    flip (withScientific "lat") (a V.! 0) $ \lat ->
    flip (withScientific "long") (a V.! 1) $ \long ->
    pure $ Coord (realToFrac lat) (realToFrac long)

type ItemId = Text

data Item = Item { iId :: !ItemId, iLocation :: !Coord }
  deriving (Show, Eq, Ord)

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

newtype Incident = Incident { incidentItem :: Item }
  deriving newtype (Show, Eq, Ord)

newtype Camera = Camera { cameraItem :: Item }
  deriving newtype (Show, Eq, Ord)

loadIncidents :: IO [Incident]
loadIncidents = fmap Incident <$> loadJSON "https://www.az511.gov/map/mapIcons/Incidents" "incidents.json"

loadCameras :: IO [Camera]
loadCameras = fmap Camera <$> loadJSON "https://www.az511.gov/map/mapIcons/Cameras" "cameras.json"

findCloseCoords :: Distance -> [Incident] -> [Camera] -> [(Incident, Camera)]
findCloseCoords maxDist xs cameras = do
  i@(Incident x) <- xs
  c@(Camera y) <- cameras
  let dist = iLocation x `haversineDistance` iLocation y
  guard $ dist <= maxDist
  pure (i, c)

data FullCamera = FullCamera
  { fcCamera :: !Camera
  , fcFile :: !FilePath
  , fcImageURL :: !URL
  }
  deriving (Eq, Ord)

downloadCameraImage :: Camera -> IO FullCamera
downloadCameraImage camera@Camera{cameraItem=Item{iId}} = do
  let iIdS = T.unpack iId
  bs <- getFile ("https://www.az511.gov/map/data/Cameras/" <> iIdS) (iIdS <.> "json")
  let url = bs ^?! nth 0 . key "imageUrl" . _String
      urlS = T.unpack url
      filename = takeFileName urlS
  void $ getFile urlS filename
  pure FullCamera{fcCamera=camera, fcFile=filename, fcImageURL=T.unpack url}

data FullIncident = FullIncident
  { fiIncident :: !Incident
  , fiDescription :: !Text
  }
  deriving (Eq, Ord)

downloadFullIncident :: Incident -> IO FullIncident
downloadFullIncident incident@Incident{incidentItem=Item{iId}} = do
  let iIdS = T.unpack iId
  bs <- getFile ("https://www.az511.gov/map/data/Incidents/" <> iIdS) (iIdS <.> "json")
  let description = bs ^?! key "details" . key "detailLang1" . key "eventDescription" . _String
  pure FullIncident{fiIncident=incident, fiDescription=description}

generateHTML :: Map FullIncident (Set FullCamera) -> Html
generateHTML incidents = H.docTypeHtml $ do
  H.head $ do
    H.title "Incidents"
    H.style "img {max-width: 100%;}"
  H.body $
    forM_ (M.toList incidents) $ \(incident, cameras) -> do
      H.h2 . H.toHtml $ fiDescription incident
      forM_ cameras $ \camera -> do
        H.div $ H.a ! href (H.toValue $ fcImageURL camera) $ "original URL"
        let filepathValue = H.toValue $ fcFile camera
        H.a ! href filepathValue $ H.img ! src filepathValue ! alt "camera"

groupByIncident :: [(Incident, Camera)] -> Map Incident (Set Camera)
groupByIncident = M.fromListWith (<>) . fmap (second S.singleton)

findFullCamera :: Set FullCamera -> Camera -> FullCamera
-- weird that there is no `S.find`?!
findFullCamera cameras camera = getSingle $ S.filter ((== camera) . fcCamera) cameras

getSingle :: Set a -> a
getSingle s | S.size s == 1 = S.elemAt 0 s
            | otherwise = error $ "unexpected size of set " <> show (S.size s) <> ", expected 1"

findFullIncident :: Set FullIncident -> Incident -> FullIncident
findFullIncident incidents incident = getSingle $ S.filter ((== incident) . fiIncident) incidents

generateCamerasPage :: IO ()
generateCamerasPage = do
  incidents <- loadIncidents
  cameras <- loadCameras
  putStrLn $ mconcat [show $ length incidents, " incidents, ", show $ length cameras, " cameras"]

  let closeItems = findCloseCoords 0.2 incidents cameras
  print closeItems

  let incidentsWithCameras = groupByIncident closeItems
      closeCameras = foldl' (<>) mempty . fmap snd . M.toList $ incidentsWithCameras
  fullCameras <- fmap S.fromList . traverse downloadCameraImage $ S.toList closeCameras
  fullIncidents <- fmap S.fromList . traverse downloadFullIncident $ M.keys incidentsWithCameras
  let fullIncidentsWithCameras = M.mapKeys (findFullIncident fullIncidents) $
        S.map (findFullCamera fullCameras) <$> incidentsWithCameras
  L.writeFile "index.html" . renderHtml $ generateHTML fullIncidentsWithCameras

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> generateCamerasPage
    ["--version"] -> putStrLn $ showVersion version
    xs -> die $ "unknown arguments " <> show xs
