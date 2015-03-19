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

type Word = Text
type Dictionary = [Word]
type LetterSet = MS.MultiSet Char
type AnagramStep = (MS.MultiSet Word, LetterSet)

interestingWord :: Word -> Bool
interestingWord "a" = True
interestingWord "i" = True
interestingWord s = T.length s > 1

loadDictionary :: FilePath -> IO Dictionary
loadDictionary file =
  (sortBy (flip (comparing T.length)) .
   filter interestingWord .
   T.lines) <$>
  TIO.readFile file

toLetterSet :: Word -> LetterSet
toLetterSet = MS.fromList . T.unpack . T.toLower

extractWord :: LetterSet -> Word -> Maybe LetterSet
extractWord letterSet word =
  if MS.isSubsetOf wordSet letterSet
     then Just (MS.difference letterSet wordSet)
     else Nothing
  where wordSet = toLetterSet word

unfoldAnagram :: Dictionary -> AnagramStep -> (AnagramStep, [AnagramStep])
unfoldAnagram dictionary anagram@(wordsSoFar,letterSetSoFar) =
  (anagram,anagrams')
  where anagrams' =
          mapMaybe anagramStep dictionary
        anagramStep newWord =
          case extractWord letterSetSoFar newWord of
            Nothing -> Nothing
            Just newLetterSet ->
              Just (MS.insert newWord wordsSoFar,newLetterSet)

anagrams :: Dictionary -> Word -> [[Word]]
anagrams dictionary word =
  map (MS.toList . fst) $
  filter (MS.null . snd) $
  Tree.flatten $
  Tree.unfoldTree (unfoldAnagram dictionary)
                  (MS.empty,toLetterSet word)

main :: IO ()
main =
  do [phrase] <- getArgs
     ws <- loadDictionary "/usr/share/dict/words"
     mapM_ (putStrLn . T.unpack . T.unwords) $
       anagrams ws
                (T.pack $
                 filter (not . isSpace) phrase)
