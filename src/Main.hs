module Main where

import           Wordle

import           Colourista
import           Data.FileEmbed
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           System.Environment             ( getArgs )
import           System.Exit                    ( die )


rightOrDie :: Either Text a -> a
rightOrDie = either (error . Text.unpack . ("error: " <>)) id

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
  args <- getArgs
  case args of
    [] -> mainGame
    [simulateMe] | Right wordlet <- parseWordlet (Text.pack simulateMe) ->
      simulate guessWords masterWords wordlet
    _ -> die "error parsing command line args"

 where
  mainGame = do
    greenMessage
      "\n\n\
            \        ┌────────────────────────────────────┐       .==.    \n\
            \        │ Welcome to Turtlet, a Wordle Solver│    __/-^-^\\  \n\
            \        └────────────────────────────────────┘   (' )^-^-^\\)\n\
            \                                                  `^UU^^UU^  \n"
    infoMessage " Press Ctrl-C to exit at any time."
    runGame guessWords masterWords

