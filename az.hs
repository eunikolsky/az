#!/usr/bin/env stack
-- stack script --resolver=lts-22.17 --package=aeson --package=aeson-pretty --package=blaze-html --package=bytestring --package=conduit --package=containers --package=directory --package=filepath --package=microlens --package=microlens-aeson --package=http-client --package=http-conduit --package=http-types --package=optparse-applicative --package=tagsoup --package=text --package=time --package=vector

{-# OPTIONS_GHC -Wall -Wprepositive-qualified-module #-}
{-# LANGUAGE DerivingStrategies, OverloadedStrings #-}

import Conduit
import Control.Monad
import Data.Aeson
import Data.Aeson.Encode.Pretty
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
import Data.Text.Lazy qualified as LT
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy.Encoding qualified as LT (decodeUtf8)
import Data.Time
import Data.Vector qualified as V ((!))
import Data.Version
import Lens.Micro
import Lens.Micro.Aeson
import Network.HTTP.Client
import Network.HTTP.Simple
import Network.HTTP.Types.Status
import Options.Applicative qualified as O
import System.Directory
import System.FilePath
import Text.Blaze.Html.Renderer.Utf8
import Text.Blaze.Html5 ((!), Html)
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match

version :: Version
version = makeVersion [0, 2, 0]

userAgent :: ByteString
userAgent = "az/" <> C8.pack (showVersion version)

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
    pure r { requestHeaders = reqHeaders <> [("User-Agent", userAgent)] }
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
  deriving (Eq, Ord)

instance Show Item where
  show Item{iId, iLocation} = mconcat ["Item ", T.unpack iId, " at ", show iLocation]

instance FromJSON Item where
  parseJSON = withObject "Item" $ \o -> Item
    <$> o .: "itemId"
    <*> o .: "location"

type Distance = Double -- km

toMeters :: Distance -> Double
toMeters = (* 1000)

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
  deriving newtype (Eq, Ord)

instance Show Incident where
  show (Incident Item{iId, iLocation}) = mconcat ["Incident ", T.unpack iId, " at ", show iLocation]

newtype Camera = Camera { cameraItem :: Item }
  deriving newtype (Eq, Ord)

instance Show Camera where
  show (Camera Item{iId, iLocation}) = mconcat ["Camera ", T.unpack iId, " at ", show iLocation]

loadIncidents :: IO [Incident]
loadIncidents = fmap Incident <$> loadJSON "https://www.az511.gov/map/mapIcons/Incidents" "incidents.json"

loadCameras :: IO [Camera]
loadCameras = fmap Camera <$> loadJSON "https://www.az511.gov/map/mapIcons/Cameras" "cameras.json"

findCloseCoords :: Distance -> [Incident] -> [Camera] -> [(Incident, (Camera, Distance))]
findCloseCoords maxDist xs cameras = do
  i@(Incident x) <- xs
  c@(Camera y) <- cameras
  let dist = iLocation x `haversineDistance` iLocation y
  guard $ dist <= maxDist
  pure (i, (c, dist))

data FullCamera = FullCamera
  { fcCamera :: !Camera
  , fcFile :: !FilePath
  , fcImageURL :: !URL
  }
  deriving (Eq, Ord)

-- using image data URL is cleaner, but:
-- * produces image URLs that get downloaded fine, but the links don't open in
-- firefox for some reason (HTTP?);
-- * and image downloads are noticeably slower than those from the tooltips.
useImageData :: Bool
useImageData = False

downloadCameraImage :: Camera -> IO FullCamera
downloadCameraImage camera@Camera{cameraItem=Item{iId}} = do
  url <- if useImageData then getURLFromData else getURLFromTooltip
  let urlS = T.unpack url
      filename = takeFileName urlS
  void $ getFile urlS filename
  pure FullCamera{fcCamera=camera, fcFile=filename, fcImageURL=T.unpack url}

  where
    getURLFromData = do
      let iIdS = T.unpack iId
      bs <- getFile ("https://www.az511.gov/map/data/Cameras/" <> iIdS) (iIdS <.> "json")
      pure $ bs ^?! nth 0 . key "imageUrl" . _String

    getURLFromTooltip = do
      let iIdS = T.unpack iId
      bs <- decodeUtf8 <$> getFile ("https://www.az511.gov/tooltip/Cameras/" <> iIdS <> "?lang=en") (iIdS <.> "html")
      let tags = parseTags bs
          img = fromJust $ find (tagOpen (== "img") (any ((== "class") . fst))) tags
          relativeURL = fromAttrib "data-lazy" img
      when (T.null relativeURL) $ error "Didn't find camera image URL"
      pure $ "https://www.az511.gov" <> relativeURL

data FullIncident = FullIncident
  { fiIncident :: !Incident
  , fiDescription :: !Text
  , fiJSON :: !Text
  }
  deriving (Eq, Ord)

downloadFullIncident :: Incident -> IO FullIncident
downloadFullIncident incident@Incident{incidentItem=Item{iId}} = do
  let iIdS = T.unpack iId
  bs <- getFile ("https://www.az511.gov/map/data/Incidents/" <> iIdS) (iIdS <.> "json")
  let description = bs ^?! key "details" . key "detailLang1" . key "eventDescription" . _String
      fiJSON = prettyShowJSON bs
  pure FullIncident{fiIncident=incident, fiDescription=description, fiJSON}

prettyShowJSON :: ByteString -> Text
-- TODO avoid double json decoding
prettyShowJSON = LT.toStrict . LT.decodeUtf8 . encodePretty' conf . decodeStrict @Value
  where conf = defConfig { confIndent = Spaces 2 }

generateHTML :: ZonedTime -> Map FullIncident (Set (FullCamera, Distance)) -> Html
generateHTML genTime incidents = H.docTypeHtml $ do
  H.head $ do
    H.title "AZ Incidents"
    H.style "img {max-width: 100%; vertical-align: middle;}"
  H.body $ do
    forM_ (M.toList incidents) $ \(incident, cameras) -> do
      H.h2 . H.toHtml $ fiDescription incident
      H.details $ do
        H.summary "incident details"
        H.pre . H.toHtml $ fiJSON incident

      forM_ (sortCamerasByDistance cameras) $ \(camera, distance) -> do
        H.div $ do
          "distance to incident: "
          H.toHtml . show @Int . round . toMeters $ distance
          " m | "
          H.a ! A.href (H.toValue $ fcImageURL camera) $ "original URL"
        let filepathValue = H.toValue $ fcFile camera
        H.a ! A.href filepathValue $ H.img ! A.src filepathValue ! A.alt "camera"

    H.div $ do
      "Courtesy of "
      H.a ! A.href "https://az511.gov/" $ "AZ 511"
      " | Generated at "
      H.toHtml $ formatTime defaultTimeLocale "%F %T %EZ" genTime

  where
    sortCamerasByDistance = sortOn snd . S.toList

groupByIncident :: [(Incident, (Camera, Distance))] -> Map Incident (Set (Camera, Distance))
groupByIncident = M.fromListWith (<>) . fmap (second S.singleton)

findFullCamera :: Set FullCamera -> Camera -> FullCamera
-- weird that there is no `S.find`?!
findFullCamera cameras camera = getSingle $ S.filter ((== camera) . fcCamera) cameras

getSingle :: Set a -> a
getSingle s | S.size s == 1 = S.elemAt 0 s
            | otherwise = error $ "unexpected size of set " <> show (S.size s) <> ", expected 1"

findFullIncident :: Set FullIncident -> Incident -> FullIncident
findFullIncident incidents incident = getSingle $ S.filter ((== incident) . fiIncident) incidents

generateCamerasPage :: Distance -> IO ()
generateCamerasPage maxDist = do
  incidents <- loadIncidents
  cameras <- loadCameras
  putStrLn $ mconcat [show $ length incidents, " incidents, ", show $ length cameras, " cameras"]

  let closeItems = findCloseCoords maxDist incidents cameras
  print closeItems

  let incidentsWithCameras = groupByIncident closeItems
      closeCameras :: Set Camera = foldl' (<>) mempty . fmap (S.map fst . snd) . M.toList $ incidentsWithCameras
  fullCameras <- fmap S.fromList . traverse downloadCameraImage $ S.toList closeCameras
  fullIncidents <- fmap S.fromList . traverse downloadFullIncident $ M.keys incidentsWithCameras
  let fullIncidentsWithCameras = M.mapKeys (findFullIncident fullIncidents) $
        S.map (first $ findFullCamera fullCameras) <$> incidentsWithCameras
  now <- getZonedTime
  L.writeFile "index.html" . renderHtml $ generateHTML now fullIncidentsWithCameras

between :: Ord a => (a, a) -> a -> a
between (a, b) x = a `max` x `min` b

maxDistParser :: O.Parser Distance
maxDistParser = fmap (between (0, 1)) . O.option O.auto $
  O.long "max-dist" <> O.short 'd'
  <> O.help "Max distance for collation of incidents and cameras, km, in range [0; 1]"
  <> O.value 0.2 <> O.showDefault
  <> O.metavar "MAX_DIST"

main :: IO ()
main = generateCamerasPage =<< O.execParser opts
  where
    opts = O.info (maxDistParser O.<**> O.simpleVersioner ver O.<**> O.helper) $
      O.fullDesc <> O.progDesc "Generates a page with traffic camera images near incidents in Arizona"
    ver = showVersion version
