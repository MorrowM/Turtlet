
module Wordle
  ( Wordlet
  , parseWordList
  , parseWordlet
  , WordList
  , runGame
  , WordletType(..)
  , simulateGame
  , selectRandomWord
  , simulate
  ) where

import           Colourista                     ( blue
                                                , bold
                                                , cyan
                                                , formatWith
                                                , green
                                                , magenta
                                                , red
                                                , reset
                                                )
import           Control.Concurrent             ( threadDelay )
import           Control.Concurrent.Async       ( AsyncCancelled
                                                , cancel
                                                , withAsync
                                                )
import           Control.Exception              ( catch
                                                , evaluate
                                                )
import           Control.Monad                  ( forever
                                                , void
                                                )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import           Data.Char                      ( isAsciiLower
                                                , toLower
                                                , toUpper
                                                )
import           Data.Coerce                    ( coerce )
import           Data.Foldable                  ( for_ )
import           Data.Function                  ( on )
import           Data.Kind                      ( Type )
import           Data.List                      ( delete
                                                , maximumBy
                                                , sortOn
                                                )
import qualified Data.List.NonEmpty            as NE
import           Data.List.NonEmpty             ( NonEmpty )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Ord                       ( comparing )
import           Data.Proxy                     ( Proxy(Proxy) )
import           Data.String                    ( IsString(..) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           Data.Traversable               ( mapAccumL )
import qualified Data.Vector                   as VV
import           Data.Vector.Sized              ( Vector )
import qualified Data.Vector.Sized             as V
import           GHC.IO.Handle                  ( hFlush )
import           GHC.TypeNats                   ( type (-)
                                                , type (<=)
                                                , KnownNat
                                                )
import           System.Console.ANSI            ( clearLine
                                                , hideCursor
                                                , showCursor
                                                )
import           System.Console.Haskeline       ( InputT
                                                , defaultSettings
                                                , getInputLine
                                                , handleInterrupt
                                                , outputStr
                                                , outputStrLn
                                                , runInputT
                                                , withInterrupt
                                                )
import           System.Exit                    ( exitSuccess )
import           System.IO                      ( stdout )
import           System.Random                  ( randomRIO )

-- | A tag specifying whether a type represents a master word or a guess word.
-- Intended to be used with the @DataKinds@ language extension.
data WordletType = Guess | Master

-- | A Wordle word list, tagged with a phantom type specifying
-- whether it's a list of master words or a guess words.
type WordList :: WordletType -> Type
newtype WordList wlty = WordList { getWordList :: VV.Vector (Wordlet wlty) }
  deriving (Eq, Ord, Show)

-- | Convert a @Vector 5@ to a tuple.
toTuple :: forall a . Vector 5 a -> (a, a, a, a, a)
toTuple v = (i @0, i @1, i @2, i @3, i @4)
 where
  i :: forall n . (KnownNat n, n <= 4) => a
  i = V.index' @n @(4 - n) v Proxy

-- | A Wordle word, tagged with a phantom type specifying
-- whether it's a master word or a guess word.
type Wordlet :: WordletType -> Type
newtype Wordlet wlty = Wordlet (Vector 5 Char)
  deriving (Eq, Ord)

instance Show (Wordlet wlty) where
  show w = "\"" <> wordletToString w <> "\""

-- | Converts a wordlet to a string.
wordletToString :: Wordlet wlty -> String
wordletToString (Wordlet wordlet) =
  let (a, b, c, d, e) = toTuple wordlet in [a, b, c, d, e]
instance IsString (Wordlet wlty) where
  fromString [a, b, c, d, e] = Wordlet $ V.fromTuple (a, b, c, d, e)
  fromString _               = error "Wordlet must be 5 characters long"

-- | A Wordle color.

data Color = Gray | Yellow  | Green
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | A Wordle color pattern.
newtype Colors = Colors (Vector 5 Color)
  deriving (Eq, Ord)

{-# COMPLETE Wordlet #-}

colorList :: Colors -> [Color]
colorList (Colors v) = let (a, b, c, d, e) = toTuple v in [a, b, c, d, e]

instance Show Colors where
  show colors = toChar <$> colorList colors
   where
    toChar Gray   = 'X'
    toChar Yellow = 'Y'
    toChar Green  = 'G'

instance IsString Colors where
  fromString = toRight . parseColors . Text.pack
   where
    toRight (Right a) = a
    toRight (Left  _) = error "parseColors failed"

-- | Parse a Wordle word list from some 'Text'
parseWordList :: Text -> Either Text (WordList wlty)
parseWordList txt = do
  let word_lines = Text.lines txt
  wordlets <- traverse parseWordlet word_lines
  pure $ WordList $ VV.fromList wordlets

-- | Parse a 5 letter word from some 'Text'
parseWordlet :: Text -> Either Text (Wordlet wlty)
parseWordlet txt@(Text.unpack -> [a,b,c,d,e]) | Text.all isAsciiLower txt =
  Right $ Wordlet $ V.fromTuple (a, b, c, d, e)
parseWordlet txt =
  Left $ "Invalid word encountered while parsing word list: " <> txt

-- | Parse a single color from some 'Text'.
parseColor :: Char -> Either Text Color
parseColor = go . toLower
 where
  go 'g' = Right Green
  go 'y' = Right Yellow
  go 'x' = Right Gray
  go c =
    Left
      $  "Invalid color "
      <> Text.pack (show c)
      <> ": color must be one of 'g', 'y', or 'x'"

-- | Parse a color pattern from some 'Text'.
parseColors :: Text -> Either Text Colors
parseColors txt = case Text.unpack txt of
  [a, b, c, d, e] ->
    Colors <$> traverse parseColor (V.fromTuple (a, b, c, d, e))
  _ -> Left "Invalid input: input must be 5 characters long"

-- | Check if a given color pattern is consistent with a given
-- guess word and master word. 
consistentWith :: Colors -> Wordlet Guess -> Wordlet Master -> Bool
consistentWith colors guess master = guess `againstMaster` master == colors

-- | Match a guess word against a master word, yielding the appropriate
-- color pattern just like Wordle does. 
againstMaster :: Wordlet Guess -> Wordlet Master -> Colors
againstMaster (Wordlet guess) (Wordlet master) = Colors colors
 where
  possibleYellows = V.toList master
  (_, colors)     = mapAccumL go possibleYellows (V.zip guess master)

  go :: [Char] -> (Char, Char) -> ([Char], Color)
  go canBeYellow (guessChar, masterChar)
    | guessChar == masterChar      = (delete guessChar canBeYellow, Green)
    | guessChar `elem` canBeYellow = (delete guessChar canBeYellow, Yellow)
    | otherwise                    = (canBeYellow, Gray)

-- | Calculate the best possible guess from a word list of possible guesses,
-- and a word list of possible solutions.
bestGuess :: WordList Guess -> WordList Master -> Wordlet Guess
bestGuess (WordList guesses) masters = case VV.toList $ getWordList masters of
  [x] -> coerce x
  [x, _] -> coerce x
  _ -> preferMaster masters . selectBestCandidates score . VV.toList $ guesses
 where
  score guess =
    maximum
      $ let hitmap = makeHitMap guess masters
            result | null hitmap = guessNotFound
                   | otherwise   = hitmap
        in  result

-- | Throw a formatted exception when evaluated that a guess could not be found.
guessNotFound :: a
guessNotFound = errorWithoutStackTrace
  $ formatWith [red, bold] "Error: Couldn't find a candidate guess!"

-- | Select a word from a nonempty collection of words, preferring a word
-- that appears in a given master word list.   
preferMaster :: WordList Master -> NonEmpty (Wordlet Guess) -> Wordlet Guess
preferMaster masters =
  maximumBy (comparing $ \g -> coerce g `elem` getWordList masters)

-- | Calculate a collection of words that have the best score, given
-- a scoring function and a list of words.
selectBestCandidates
  :: (Wordlet Guess -> Int) -> [Wordlet Guess] -> NonEmpty (Wordlet Guess)
selectBestCandidates score guesses = result
 where
  taggedWithScore = map (\g -> (g, score g)) guesses
  sortedByScore   = sortOn snd taggedWithScore
  bestScoreGroup  = case NE.groupBy ((==) `on` snd) sortedByScore of
    (best : _rest) -> best
    []             -> guessNotFound
  result = fmap fst bestScoreGroup

-- | Calculates a count of how often each possible color pattern
-- shows up when guessing a given guess against a collection of
-- master words.
makeHitMap :: Wordlet Guess -> WordList Master -> Map Colors Int
makeHitMap guess (WordList masters) =
  Map.fromListWith (+) . VV.toList $ fmap ((, 1) . againstMaster guess) masters

-- | Remove possible solutions given a guess word and the feedback received from guessing it. 
filterMasters :: Colors -> Wordlet Guess -> WordList Master -> WordList Master
filterMasters colors guess (WordList masters) =
  WordList $ VV.filter (consistentWith colors guess) masters

-- | Run the main game loop.
runGame :: WordList Guess -> WordList Master -> IO ()
runGame guesses masters =
  handleInterrupt (putStrLn "Exiting..." *> exitSuccess)
    $ runInputT defaultSettings
    $ withInterrupt
    $ void runGame'
 where
  runGame' = do
    outputStrLn
      $ question
          "Do you have a starter word you'd like to use? (type \"no\" to generate it)"
    getUserInput $ \input -> case parseWordlet input of
      Right guess
        | guess `elem` getWordList guesses
        -> singleRound guess masters >>= gameLoop
        | otherwise
        -> pure $ Left $ errmsg "Couldn't find that guess in the guess list."
      _ | input `elem` ["n", "no"] -> gameLoop masters
      Left err                     -> pure $ Left $ errmsg err


  singleRound guess currentMasters = do
    outputStrLn
      $ question "What was the result? (type \"help\" for more information)"
    getUserInput $ \input -> case parseColors input of
      Right colors
        | colors == "ggggg" -> do
          outputStrLn $ formatWith [green, bold] "Congratulations!"
          outputStrLn $ question "Would you like to play again? (yes/no)"
          getUserInput $ \input' -> if
            | input' `elem` ["y", "yes"] -> Right <$> runGame'
            | input' `elem` ["n", "no"] -> Right <$> liftIO exitSuccess
            | otherwise -> pure $ Left $ errmsg "Sorry, didn't get that."
        | otherwise -> pure $ Right $ filterMasters colors guess currentMasters
      _ | input == "help" -> pure $ Left $ formatWith
        [blue]
        "  ⓘ  A valid response consists of 5 characters,\n\
        \     each of which is one of: 'x' (gray), 'y' (yellow), or 'g' (green)."


      Left err -> pure $ Left $ errmsg err

  gameLoop currentMasters = do
    let guess = bestGuess guesses currentMasters
    outputStr blue
    liftIO $ withAsync (loadingEllipse "  🧠 Thinking") $ \a -> do
      void $ evaluate guess
      cancel a
    outputStr $ reset <> "\n"
    outputStrLn $ formatWith [cyan] $ "Guess this word: " <> formatWith
      [bold]
      (wordletToString guess)
    newMasters <- singleRound guess currentMasters
    gameLoop newMasters

  question s = formatWith [magenta] $ "⇛  " <> s
  errmsg = formatWith [red]

-- | Fetch user input from the terminal given a function that validates it.
getUserInput :: (Text -> InputT IO (Either Text a)) -> InputT IO a
getUserInput f = loop
 where
  loop = do
    input <- getInputLine "> "
    res   <- traverse (f . Text.map toLower . Text.pack) input
    case res of
      Nothing         -> loop
      Just (Left err) -> do
        outputStrLn (Text.unpack err)
        loop
      Just (Right a) -> pure a

-- | Create a looping elipse text animation given some prefix text.
-- For example, @loadingEllipse "Loading"@ will create an animation
-- that grows from @Loading@ to @Loading... @. This function is intended to be
-- used with 'withAsync'.
loadingEllipse :: Text -> IO ()
loadingEllipse prefix = (hideCursor *> loop)
  `catch` \(_ :: AsyncCancelled) -> once "..." *> showCursor

 where
  loop = forever $ for_ [0 .. 3] $ \n -> once (Text.replicate n ".")

  once s = do
    void $ threadDelay 300000
    clearLine
    Text.putStr $ "\r" <> prefix <> s
    hFlush stdout


simulateGame
  :: Maybe (Wordlet Guess)
  -> Wordlet Master
  -> WordList Guess
  -> WordList Master
  -> [(Colors, Wordlet Guess)]
simulateGame starting_guess master guess_list master_list =
  case starting_guess of
    Nothing -> go (bestGuess guess_list master_list)
    Just wo -> go wo
 where
  go guess
    | guess == coerce master
    = [("ggggg", guess)]
    | otherwise
    = let colors = (guess `againstMaster` master)
      in  (colors, guess) : simulateGame
            Nothing
            master
            guess_list
            (filterMasters colors guess master_list)

selectRandomWord :: WordList a -> IO (Wordlet a)
selectRandomWord wordlist = do
  i <- randomRIO (0, VV.length $ getWordList wordlist)
  pure ((VV.! i) $ getWordList wordlist)

colorToEmoji :: Color -> Char
colorToEmoji Gray   = '⬛'
colorToEmoji Yellow = '🟨'
colorToEmoji Green  = '🟩'

simulate :: WordList Guess -> WordList Master -> Wordlet Master -> IO ()
simulate guessWords masterWords master =
  putStr $ ("🐢\nWordle XXX " <> show (length result) <> "/6\n\n") <> formatted
 where
  result =
    simulateGame (Just ("raise" :: Wordlet Guess)) master guessWords masterWords
  formatted = unlines $ formatRound <$> result
  formatRound (colors, wordlet) =
    (colorToEmoji <$> colorList colors)
      <> " ||`"
      <> map toUpper (wordletToString wordlet)
      <> "`||"


