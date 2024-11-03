#!/usr/bin/env stack
-- stack script --resolver=lts-22.17 --package=aeson --package=aeson-pretty --package=blaze-html --package=bytestring --package=conduit --package=containers --package=directory --package=filepath --package=microlens --package=microlens-aeson --package=mtl --package=http-client --package=http-conduit --package=http-types --package=optparse-applicative --package=process --package=tagsoup --package=text --package=time --package=unix --package=vector

{-# OPTIONS_GHC -Wall -Wprepositive-qualified-module -Werror=incomplete-patterns -Werror=missing-fields -Werror=tabs #-}
{-# LANGUAGE DerivingStrategies, MultiWayIf, OverloadedStrings #-}

import Conduit
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Lazy qualified as L
import Data.List
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy qualified as LT
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
import System.IO
import System.Posix.Files
import System.Process
import Text.Blaze.Html.Renderer.Utf8
import Text.Blaze.Html5 ((!), Html)
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match

version :: Version
version = makeVersion [0, 4, 2]

userAgent :: ByteString
userAgent = "az/" <> C8.pack (showVersion version)

type URL = String

data Website = Website { wsURL :: !URL, wsName :: !Text, wsStateAbbrev :: !Text, wsGetRelURL :: [Tag Text] -> Text }

-- | Monad `Prog` provides access to the source website information.
type Prog = ReaderT Website IO

-- Helpers to extract data returned by `getFile`.
fileData :: (ByteString, a) -> ByteString
fileData = fst

cacheFilepath :: (a, FilePath) -> FilePath
cacheFilepath = snd

ensureDirectory :: FilePath -> IO ()
ensureDirectory = createDirectoryIfMissing True

getFile :: URL -> FilePath -> Prog (ByteString, FilePath)
getFile url _file = do
  dir <- asks $ T.unpack . wsStateAbbrev
  liftIO $ ensureDirectory dir
  let file = dir </> _file
  exists <- liftIO $ doesFileExist file
  reqHeaders <- if exists
    then do
      modTime <- liftIO $ getModificationTime file
      let timeStr = formatTime defaultTimeLocale rfc822DateFormat modTime
      pure [("If-Modified-Since", C8.pack timeStr)]
    else pure mempty
  liftIO . putStrLn . mconcat $ [url, " → ", file]

  req <- do
    r <- parseRequest url
    pure r { requestHeaders = reqHeaders <> [("User-Agent", userAgent)] }
  liftIO . runResourceT $ httpSink req $ \res -> let status = responseStatus res in if
    | status == notModified304 -> liftIO (putStrLn "not modified")
    | status >= ok200 && status < multipleChoices300 -> do
        sinkFile file
        let maybeLastModifiedStr = listToMaybe $ getResponseHeader "Last-Modified" res
        forM_ maybeLastModifiedStr $ setModTime file
    | otherwise -> error $ "unexpected status " <> show status

  (,) <$> (liftIO . BS.readFile) file <*> pure file

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

loadJSON :: FilePath -> URL -> Prog [Item]
loadJSON file url = do
  bs <- fileData <$> getFile url file
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

basedURL :: Text -> Prog URL
basedURL t = do
  baseURL <- asks wsURL
  pure $ baseURL <> (T.unpack t)

loadIncidents :: Prog [Incident]
loadIncidents = basedURL "/map/mapIcons/Incidents" >>= fmap (fmap Incident) . loadJSON "incidents.json"

loadCameras :: Prog [Camera]
loadCameras = basedURL "/map/mapIcons/Cameras" >>= fmap (fmap Camera) . loadJSON "cameras.json"

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
  , fcTooltip :: !Text
  }
  deriving (Eq, Ord)

downloadCameraImage :: Camera -> Prog FullCamera
downloadCameraImage camera@Camera{cameraItem=Item{iId}} = do
  (url, tooltip) <- getURLFromTooltip
  let filename = takeFileName url
  cacheFile <- cacheFilepath <$> getFile url filename
  let fcTooltip = T.replace "\r\n" "\n" tooltip
  pure FullCamera{fcCamera=camera, fcFile=cacheFile, fcImageURL=url, fcTooltip}

  where
    -- using image data URL is cleaner, but:
    -- * it produces image URLs that get downloaded fine, but the links don't
    -- open in firefox for some reason (HTTP?);
    -- * and image downloads are noticeably slower than those from the tooltips.
    getURLFromTooltip = do
      let iIdS = T.unpack iId
      tootltipURL <- basedURL $ "/tooltip/Cameras/" <> iId <> "?lang=en"
      bs <- decodeUtf8 . fileData <$> getFile tootltipURL (iIdS <.> "html")
      getRelURL <- asks wsGetRelURL
      let tags = parseTags bs
          relativeURL = getRelURL tags
      url <- basedURL relativeURL
      when (null url) $ error "Didn't find camera image URL"
      pure (url, bs)

data FullIncident = FullIncident
  { fiIncident :: !Incident
  -- | Detailed description, if any. It's `Nothing` if the incident "disappeared" between fetching all incidents and querying this one.
  , fiDescription :: !(Maybe Text)
  , fiJSON :: !Text
  }
  deriving (Eq, Ord)

downloadFullIncident :: Incident -> Prog FullIncident
downloadFullIncident incident@Incident{incidentItem=Item{iId}} = do
  let iIdS = T.unpack iId
  url <- basedURL $ "/map/data/Incidents/" <> iId
  bs <- fileData <$> getFile url (iIdS <.> "json")
  let description =
        bs ^? key "details" . key "detailLang1" . key "eventDescription" . _String
        <|> bs ^? key "description" . _String
      fiJSON = prettyShowJSON bs
  pure FullIncident{fiIncident=incident, fiDescription=description, fiJSON}

prettyShowJSON :: ByteString -> Text
-- TODO avoid double json decoding
prettyShowJSON = LT.toStrict . LT.decodeUtf8 . encodePretty' conf . decodeStrict @Value
  where conf = defConfig { confIndent = Spaces 2 }

type UnorderedIncidentCameras = Map FullIncident (Set (FullCamera, Distance))
type IncidentCameras = NonEmpty (FullIncident, [(FullCamera, Distance)])
type IncidentCamerasByWebsite = NonEmpty (Website, IncidentCameras)

generateHTML :: ZonedTime -> IncidentCamerasByWebsite -> Html
generateHTML genTime incidents = H.docTypeHtml $ do
  let websites = NE.toList $ NE.map fst incidents
      states = T.toUpper . T.intercalate ", " . fmap wsStateAbbrev $ websites
      websiteLinks = mconcat . intersperse ", " $ do
        Website{wsURL, wsName} <- websites
        pure $ H.a ! A.href (H.toValue $ wsURL) $ (H.toHtml wsName)

  H.head $ do
    H.title . H.toHtml $ mconcat [states, " Incidents"]
    H.style "img {max-width: 100%; vertical-align: middle;} details {display: inline;}"
  H.body $ do
    forM_ incidents $ \(Website{wsStateAbbrev}, stateIncidents) -> do
      H.h1 . H.toHtml . T.toUpper $ wsStateAbbrev

      forM_ stateIncidents $ \(incident, cameras) -> do
        H.h2 . H.toHtml . fromMaybe "<no details>" $ fiDescription incident
        H.details $ do
          H.summary "incident details"
          H.pre . H.toHtml $ fiJSON incident

        forM_ cameras $ \(camera, distance) -> do
          H.div $ do
            "distance to incident: "
            H.toHtml . show @Int . round . toMeters $ distance
            " m | "
            H.a ! A.href (H.toValue $ fcImageURL camera) $ "original URL"
            " | "
            H.details $ do
              H.summary "camera details"
              H.pre . H.toHtml $ fcTooltip camera
          let filepathValue = H.toValue $ fcFile camera
          H.a ! A.href filepathValue $ H.img ! A.src filepathValue ! A.alt "camera"

    H.div $ do
      "Courtesy of "
      websiteLinks
      " | Generated at "
      H.toHtml $ formatTime defaultTimeLocale "%F %T %EZ" genTime
      " by "
      H.code . H.toHtml . decodeUtf8 $ userAgent
      " | You do not need to enable JavaScript to run this \"app\"!"

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

getIncidentCameras :: Distance -> Prog UnorderedIncidentCameras
getIncidentCameras maxDist = do
  incidents <- loadIncidents
  cameras <- loadCameras
  liftIO . putStrLn $ mconcat ["Total: ", show $ length incidents, " incidents, ", show $ length cameras, " cameras"]

  let closeItems = findCloseCoords maxDist incidents cameras
  -- print closeItems

  let incidentsWithCameras = groupByIncident closeItems
      closeCameras :: Set Camera = foldl' (<>) mempty . fmap (S.map fst . snd) . M.toList $ incidentsWithCameras
  liftIO . putStrLn $ mconcat ["Interesting: ", show $ length incidentsWithCameras, " incidents, ", show $ length closeCameras, " cameras"]

  fullCameras <- fmap S.fromList . traverse downloadCameraImage $ S.toList closeCameras
  fullIncidents <- fmap S.fromList . traverse downloadFullIncident $ M.keys incidentsWithCameras
  pure . M.mapKeys (findFullIncident fullIncidents) $
        S.map (first $ findFullCamera fullCameras) <$> incidentsWithCameras

generateCamerasPage :: IncidentCamerasByWebsite -> IO FilePath
generateCamerasPage incidentCameras = do
  now <- getZonedTime
  let filename = "index.html"
  L.writeFile filename . renderHtml $ generateHTML now incidentCameras
  pure filename

-- | "Opens" a file using the `open` command (for macos). Prints stdout, if any, to stderr.
open :: FilePath -> IO ()
open f = readProcess "open" [f] "" >>= logStdout
  where logStdout s | null s = pure ()
                    | otherwise = hPutStrLn stderr $ mconcat ["open ", f, " said: ", s]

-- | Moves incidents with certain keywords in the name to the top of the list.
prioritizeIncidents :: IncidentCameras -> IncidentCameras
prioritizeIncidents incidents = fromJust . NE.nonEmpty $ sortByName prioritized <> sortByName others
  where
    (prioritized, others) = NE.partition hasPriotitizedName incidents
    hasPriotitizedName = maybe False isPrioritized . fiDescription . fst
    isPrioritized = containsKeyword . T.toLower
    containsKeyword t = any (`T.isInfixOf` t) ["crash", "fire"]
    sortByName = sortOn (fiDescription . fst)

orderIncidentCameras :: UnorderedIncidentCameras -> Maybe IncidentCameras
orderIncidentCameras = (fmap . fmap) (second sortCamerasByDistance) . NE.nonEmpty . M.toList
  where sortCamerasByDistance = sortOn snd . S.toList

run :: Distance -> IO ()
run maxDist = do
  maybeIncidentCamerasByWebsite :: [Maybe (Website, IncidentCameras)]
    <- flip traverse websites $ \website -> flip runReaderT website $ do
      incidents <- getIncidentCameras maxDist
      let maybeOrderedIncidents = orderIncidentCameras incidents
          maybePrioritizedIncidents = prioritizeIncidents <$> maybeOrderedIncidents
          maybePrioritizedIncidentsWithWebsite = (website,) <$> maybePrioritizedIncidents
      pure maybePrioritizedIncidentsWithWebsite
  let incidentCamerasByWebsiteList :: [(Website, IncidentCameras)] = catMaybes maybeIncidentCamerasByWebsite
      incidentCamerasByWebsite = NE.nonEmpty incidentCamerasByWebsiteList

  case incidentCamerasByWebsite of
    Just incidentCameras -> generateCamerasPage incidentCameras >>= open
    Nothing -> putStrLn "no incidents with cameras found"

  where
    websites =
      [ Website { wsURL = "https://www.az511.gov" , wsName = "AZ 511", wsStateAbbrev = "az", wsGetRelURL = dataLazyFromFirstImg }
      , Website { wsURL = "https://511.idaho.gov" , wsName = "Idaho 511", wsStateAbbrev = "id", wsGetRelURL = dataLazyFromFirstImg }
      , Website { wsURL = "https://www.511ny.org" , wsName = "511NY", wsStateAbbrev = "ny", wsGetRelURL = srcFromCCTVImageImg }
      ]

    dataLazyFromFirstImg = fromAttrib "data-lazy" . fromJust . find (tagOpen (== "img") (any ((== "class") . fst)))
    srcFromCCTVImageImg = fromAttrib "src" . fromJust . find (tagOpen (== "img") (any (== ("class", "cctvImage"))))

-- | Runs the action in the program's `XdgCache`-based directory, creating it if necessary.
inCacheDir :: IO a -> IO a
inCacheDir action = do
  cacheDir <- getXdgDirectory XdgCache "az"
  ensureXdgDirectory cacheDir
  withCurrentDirectory cacheDir action

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM cond action = cond >>= \b -> unless b action

-- | Ensures the given directory exists, creating it with file mode `700` if necessary.
ensureXdgDirectory :: FilePath -> IO ()
ensureXdgDirectory dir = unlessM (doesDirectoryExist dir) $ do
  createDirectory dir
  let perm700 = ownerModes
  -- `setPermissions` doesn't reset group/owner permissions
  setFileMode dir perm700

between :: Ord a => (a, a) -> a -> a
between (a, b) x = a `max` x `min` b

maxDistParser :: O.Parser Distance
maxDistParser = fmap (between (0, 1)) . O.option O.auto $
  O.long "max-dist" <> O.short 'd'
  <> O.help "Max distance for collation of incidents and cameras, km, in range [0; 1]"
  <> O.value 0.2 <> O.showDefault
  <> O.metavar "MAX_DIST"

main :: IO ()
main = inCacheDir . run =<< O.execParser opts
  where
    opts = O.info (maxDistParser O.<**> O.simpleVersioner ver O.<**> O.helper) $
      O.fullDesc <> O.progDesc "Generates a page with traffic camera images near incidents in Arizona"
    ver = showVersion version
