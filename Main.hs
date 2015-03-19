{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative ((<$>))
import           Data.Char
import           Data.List
import           Data.Maybe
import qualified Data.MultiSet       as MS
import           Data.Ord
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import qualified Data.Tree           as Tree
import           System.Environment

type Dictionary = [Text]
type Remainder = MS.MultiSet Char
type Word = Text
type Anagram = ([Word], Remainder)

dullWord :: Word -> Bool
dullWord "a" = False
dullWord "i" = False
dullWord s = T.length s <= 1

loadDictionary :: FilePath -> IO Dictionary
loadDictionary file =
  (sortBy (flip (comparing T.length)) .
   filter (not . dullWord) .
   T.lines) <$>
  TIO.readFile file

toMatchSet :: Word -> Remainder
toMatchSet = MS.fromList . T.unpack . T.toLower

extractWord :: Remainder -> Word -> Maybe Remainder
extractWord remainderSet word =
  if MS.isSubsetOf wordSet remainderSet
     then Just (MS.difference remainderSet wordSet)
     else Nothing
  where wordSet = toMatchSet word

unfoldAnagram :: Dictionary -> Anagram -> (Anagram, [Anagram])
unfoldAnagram dictionary anagram@(wordsSoFar,remainderSoFar) =
  (anagram,anagrams')
  where anagrams' =
          mapMaybe anagramStep dictionary
        anagramStep newWord =
          case extractWord remainderSoFar newWord of
            Nothing -> Nothing
            Just newRemainder ->
              Just (newWord : wordsSoFar,newRemainder)

anagrams :: Dictionary -> Word -> [[Word]]
anagrams dictionary word =
  map fst $
  filter (MS.null . snd) $
  Tree.flatten $
  Tree.unfoldTree (unfoldAnagram dictionary)
                  ([],toMatchSet word)

main :: IO ()
main =
  do [phrase] <- getArgs
     ws <- loadDictionary "/usr/share/dict/words"
     mapM_ (putStrLn . T.unpack . T.unwords) $
       anagrams ws (T.pack $ filter (not . isSpace) phrase)
