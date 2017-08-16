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
import Data.IORef
import Data.Time.Clock
import Data.Time.Clock.POSIX(getPOSIXTime)
import Data.Text(pack)
import qualified Data.Set as S
import Pipes

data GlobalState = GlobalState {
  currentAlertsRef :: IORef (S.Set String),
  discordChan :: Chan String
  }

main :: IO ()
main = do
  st <- initialize
  startTimer st
  W.run 2000 $ \request respond -> do
    respond $ W.responseLBS status200 [] "BEWARE OF HURRICANES"

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
      fetch' $ CreateMessage discordChannel (pack alert) Nothing

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
        atomicWriteIORef alertsRef newAlerts
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

fetchAlerts :: IO (S.Set String)
fetchAlerts = do
  now <- getCurrentTime
  putStrLn "Fetched!"
  return $ S.fromList [ "example", show now ]
