{-# LANGUAGE ViewPatterns, OverloadedStrings #-}

import Text.Feed.Import
import Text.Feed.Types
import Text.RSS.Syntax
import Network.Download
import Network.HTTP.Types(status200)
import qualified Network.Wai as W
import qualified Network.Wai.Handler.Warp as W

import Control.Concurrent(forkIO, threadDelay)
import Control.Monad
import Data.IORef
import Data.Time.Clock
import qualified Data.Set as S

data GlobalState = GlobalState {
  currentAlertsRef :: IORef (S.Set String)
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
  return $ GlobalState { currentAlertsRef = alertsRef }
  
startTimer :: GlobalState -> IO ()
startTimer (currentAlertsRef -> alertsRef) = do
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
        notifyListeners newAlerts
      loop
    seconds = 1000000

notifyListeners :: S.Set String -> IO ()
notifyListeners newAlerts = forM_ newAlerts $ \alert -> do
  terminalListener alert
  -- INSERT OTHER LISTENERS HERE

terminalListener :: String -> IO ()
terminalListener alert = print alert

fetchAlerts :: IO (S.Set String)
fetchAlerts = do
  now <- getCurrentTime
  putStrLn "Fetched!"
  return $ S.fromList [ "example", show now ]
