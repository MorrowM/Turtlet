module Main where

import           Wordle

import           Colourista
import           Control.Monad                  ( (<=<) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           System.Exit                    ( die )


rightOrDie :: Either Text a -> IO a
rightOrDie = either (die . T.unpack . ("error: " <>)) pure

fetchWordList :: FilePath -> IO (WordList wlty)
fetchWordList = rightOrDie . parseWordList <=< T.readFile

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
  masterWords <- fetchWordList @Master "data/master-words.txt"
  guessWords  <- fetchWordList @Guess "data/guess-words.txt"
  runGame guessWords masterWords

