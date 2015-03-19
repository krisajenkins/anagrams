module Main where

import           Control.Applicative ((<$>))
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO

loadDictionary :: FilePath -> IO [Text]
loadDictionary file = T.lines <$> TIO.readFile file

main :: IO ()
main = do ws <- loadDictionary "/usr/share/dict/words"
          print $ take 5 ws
