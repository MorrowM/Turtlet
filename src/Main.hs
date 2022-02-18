module Main where

import           Wordle

import           Colourista
import           Control.Monad
import           Data.FileEmbed
import           Data.Text                      ( Text )
import qualified Data.Text                     as T


rightOrDie :: Either Text a -> a
rightOrDie = either (error . T.unpack . ("error: " <>)) id

embedWordList :: Text -> WordList wlty
embedWordList = rightOrDie . parseWordList

masterWords :: WordList Master
masterWords = embedWordList $(embedStringFile "data/master-words.txt")

guessWords :: WordList Guess
guessWords =
  embedWordList
    $  $(embedStringFile "data/guess-words.txt")
    <> $(embedStringFile "data/master-words.txt")

main :: IO ()
main = do
  greenMessage
    "\n\n\
    \        ┌────────────────────────────────────┐       .==.    \n\
    \        │ Welcome to Turtlet, a Wordle Solver│    __/-^-^\\  \n\
    \        └────────────────────────────────────┘   (' )^-^-^\\)\n\
    \                                                  `^UU^^UU^  \n"
  infoMessage " Press Ctrl-C to exit at any time."
  runGame guessWords masterWords

simulate :: IO ()
simulate = do
  masters <- replicateM 50 (selectRandomWord masterWords)
  let results = map
        (\master -> simulateGame (Just ("trace" :: Wordlet Guess))
                                 master
                                 guessWords
                                 masterWords
        )
        masters
      len = length <$> results
  print results
  print len
  print (mean len)

mean :: [Int] -> Double
mean xs = fromIntegral (sum xs) / fromIntegral (length xs)

