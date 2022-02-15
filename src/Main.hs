module Main where

import           Wordle

import           Colourista
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
guessWords = embedWordList $(embedStringFile "data/guess-words.txt")

main :: IO ()
main = do
  greenMessage
    "\n\n\n\
    \        ┌────────────────────────────────────┐       .==.    \n\
    \        │ Welcome to Turtlet, a Wordle Solver│    __/-^-^\\  \n\
    \        └────────────────────────────────────┘   (' )^-^-^\\)\n\
    \                                                  `^UU^^UU^"
  infoMessage " Press Ctrl-C to exit at any time."
  infoMessage " Type \"done\" once you've guessed successfully.\n"
  runGame guessWords masterWords

