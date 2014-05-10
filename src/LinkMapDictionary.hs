{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module LinkMapDictionary where

import Data.List (isPrefixOf, nub)
import Data.Char (isAlpha)
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

writeTo = "files/Links_Dictionarycom.txt"
url = "http://dictionary.reference.com/browse/"
startFrom = "dictionary"
maxStep = 3
maxWords = 10

isInWikipedia :: (String, String) -> Bool
isInWikipedia (_, url) = "http://ja.wikipedia.org/wiki/" `isPrefixOf` url

wordsInPage :: Cursor -> [String]
wordsInPage = take maxWords . filter (and . fmap isAlpha) . words . T.unpack . innerText . queryT [jq| body div#rpane div div.sep_top.shd_hdr.pb2.luna div.KonaBody div.lunatext.results_content.frstluna div.luna-Ent div.dndata |]

writeLinks :: String -> [String] -> IO (Maybe Int)
writeLinks now ps = do
  s <- IO.readFile writeTo
  case now `elem` (nub $ fmap (fst . break (== ',')) $ lines s) of
    True -> return Nothing
    False -> do
      mapM_ (appendFile writeTo) $ fmap (\a -> now ++ "," ++ a++ "\n") ps
      return $ Just $ length $ lines s

build :: IO ()
build = do
  appendFile writeTo ""
  build' 0 startFrom
  where
    build' n now = do
      doc <- (fromDocument . H.parseLBS) <$> simpleHttp (url ++ now)
      let ps = wordsInPage doc
      ln <- writeLinks now ps
      putStrLn $ "size: " ++ show ln
      when (ln /= Nothing && n < maxStep) $ mapM_ (build' (n+1)) ps
