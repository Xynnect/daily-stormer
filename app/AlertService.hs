{-# LANGUAGE ViewPatterns, OverloadedStrings #-}

import Text.Feed.Import
import Text.Feed.Types
import Text.RSS.Syntax
import Network.Download
import Network.HTTP.Types(status200)
import qualified Network.Wai as W
import qualified Network.Wai.Handler.Warp as W
import Network.Discord
import Network.Discord.Rest

import Control.Concurrent.Chan
import Control.Concurrent(forkIO, threadDelay)
import Control.Monad
import qualified Data.ByteString.Lazy as BSL
import Data.Either
import Data.IORef
import Data.Maybe (catMaybes)
import Data.Time.Clock
import Data.Time.Clock.POSIX(getPOSIXTime)
import Data.Text as T(pack)
import Data.Text.Lazy as TL(pack)
import Data.Text.Lazy.Encoding as TL(encodeUtf8)
import Text.Blaze.Html5 as H hiding (main)
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Renderer.Utf8 (renderMarkup)
import qualified Data.Set as S
import Pipes
import Prelude hiding (head)

data GlobalState = GlobalState {
  currentAlertsRef :: IORef (S.Set String),
  discordChan :: Chan String
  }

main :: IO ()
main = do
  st <- initialize
  startTimer st
  W.run 2000 $ \request respond -> do
    text <- renderState st
    respond $ W.responseLBS status200 [] text
    
renderState :: GlobalState -> IO BSL.ByteString
renderState (currentAlertsRef -> alertsRef) = do
  alerts <- readIORef alertsRef
  return $ renderMarkup $ html $ do
    head $ do
      title "The Daily Stormer"
      -- link ! rel "stylesheet " type_ "text/css" ! href "http://assets.daily-stormer.michaelburge.us"
    body $ do
      table ! class_ "alerts" $ forM_ alerts $ \alert -> do
        tr $
          td $ toMarkup alert
  where
    title = H.title
          
initialize :: IO GlobalState
initialize = do
  titles <- fetchAlerts
  alertsRef <- newIORef titles
  discordChan' <- initializeDiscordBot
  return $ GlobalState {
    currentAlertsRef = alertsRef,
    discordChan = discordChan'
    }

discordToken = undefined
discordChannel = undefined

initializeDiscordBot :: IO (Chan String)
initializeDiscordBot = do
  chan <- newChan
  forkIO $ do
    runBot (Bot discordToken) $ do
      with ReadyEvent $ \(Init v u _ _ _) -> do
        liftIO $ putStrLn $ "Connected to gateway " ++ show v ++ " as user " ++ show u
        loop discordChannel chan
  return chan
  where
    loop discordChannel chan = do
      alert <- liftIO $ readChan chan
      fetch' $ CreateMessage discordChannel (T.pack alert) Nothing

startTimer :: GlobalState -> IO ()
startTimer st@(currentAlertsRef -> alertsRef) = do
  threadId <- forkIO loop
  return ()
  where
    loop = do
      threadDelay $ 10 * seconds
      oldAlerts <- readIORef alertsRef
      updatedAlerts <- fetchAlerts
      let newAlerts = updatedAlerts `S.difference` oldAlerts
      unless (S.null newAlerts) $ do
        atomicWriteIORef alertsRef updatedAlerts
        notifyListeners st newAlerts
      loop
    seconds = 1000000

notifyListeners :: GlobalState -> S.Set String -> IO ()
notifyListeners (discordChan -> dchan) newAlerts = forM_ newAlerts $ \alert -> do
  terminalListener alert
  discordListener dchan alert
  -- INSERT OTHER LISTENERS HERE

terminalListener :: String -> IO ()
terminalListener alert = print alert

discordListener :: Chan String -> String -> IO ()
discordListener chan alert = writeChan chan alert

feedUrls = [
  "http://www.nhc.noaa.gov/nhc_at1.xml",
  "http://www.nhc.noaa.gov/nhc_at2.xml",
  "http://www.nhc.noaa.gov/nhc_at3.xml",
  "http://www.nhc.noaa.gov/nhc_at4.xml",
  "http://www.nhc.noaa.gov/nhc_at5.xml",
  "http://www.nhc.noaa.gov/nhc_ep1.xml",
  "http://www.nhc.noaa.gov/nhc_ep2.xml",
  "http://www.nhc.noaa.gov/nhc_ep3.xml",
  "http://www.nhc.noaa.gov/nhc_ep4.xml",
  "http://www.nhc.noaa.gov/nhc_ep5.xml"
  ]

fetchAlerts :: IO (S.Set String)
fetchAlerts = do
  feeds <- rights <$> mapM openAsFeed feedUrls
  let mTitles = flip Prelude.map feeds $ \feed -> case feed of
        RSSFeed (rssChannel -> (rssTitle -> title)) -> Just title
        _ -> Nothing
  let titles = catMaybes mTitles
  return $ S.fromList titles
