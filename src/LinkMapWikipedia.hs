{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module LinkMapWikipedia where

import Data.List
import Data.Maybe
import Text.HTML.DOM as H (readFile, parseLBS)
import qualified Data.Text.Lazy as T
import Text.XML
import Text.XML.Cursor
import Text.XML.Scraping
import Text.XML.Selector.TH

import Network.HTTP.Conduit
import Control.Monad
import Control.Applicative
import qualified Data.Map as M
import System.IO as IO

writeTo = "files/Links_Wikipedia.txt"
startFrom = "http://ja.wikipedia.org/wiki/Haskell"
maxStep = 10

pickLink :: Cursor -> Maybe (String, String)
pickLink c = do
  href <- mhref
  let t = innerText [c]
  let url = abs $ T.fromStrict $ attrs M.! href
  return (T.unpack t, T.unpack url)
  
 where
  n = node c
  attrs = elementAttributes $ (\(NodeElement e) -> e) $ n
  mhref = lookup "href" $ zip (fmap nameLocalName $ M.keys attrs) (M.keys attrs)
  abs url = case "/wiki/" `T.isPrefixOf` url of
    True -> "http://ja.wikipedia.org" `T.append` url
    False -> url

isInWikipedia :: (String, String) -> Bool
isInWikipedia (_, url) = "http://ja.wikipedia.org/wiki/" `isPrefixOf` url

linksInPage :: Cursor -> [(String, String)]
linksInPage = catMaybes . fmap pickLink . queryT [jq| body div#mw-content-text a |]

writeLinks :: String -> [(String, String)] -> IO Int
writeLinks now ps = do
  mapM_ (appendFile writeTo) $ fmap (\(a,b) -> now ++ ", " ++ a ++ ", " ++ b ++ "\n") ps
  s <- IO.readFile writeTo
  return $ length $ lines s

build :: IO ()
build = build' 0 startFrom []
  where
    build' n now urls = do
      doc <- (fromDocument . H.parseLBS) <$> simpleHttp now
      let ps = filter isValid $ linksInPage doc
      ln <- writeLinks now ps
      putStrLn $ "size: " ++ show ln
      let ps' = filter isInWikipedia ps
      when (n < maxStep) $ forM_ ps' $ \(a,b) -> do
        unless (b `elem` urls) $
          build' (n+1) b (now:urls)

    isValid (a,b) = a /= "" && "http://" `isPrefixOf` b
