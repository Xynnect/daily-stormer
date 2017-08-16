{-# LANGUAGE ViewPatterns #-}

import Text.Feed.Import
import Text.Feed.Types
import Text.RSS.Syntax
import Network.Download

import Control.Monad

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

fetchFeed :: String -> IO Feed
fetchFeed url = either error id <$> openAsFeed url

main = do
  feeds <- mapM fetchFeed feedUrls
  forM feeds $ \feed -> case feed of
    RSSFeed (rssChannel -> (rssTitle -> title)) -> print title
    _ -> error "Unexpected feed type"
