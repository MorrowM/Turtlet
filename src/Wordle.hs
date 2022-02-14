
module Wordle
  ( Wordlet
  , parseWordList
  , WordList
  , runGame
  , WordletType(..)
  ) where

import           Data.Char                      ( isAsciiLower
                                                , toLower
                                                )
import           Data.Coerce                    ( coerce )
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
import           GHC.TypeNats                   ( type (-)
                                                , type (<=)
                                                , KnownNat
                                                )
import           System.Exit                    ( exitSuccess )

data WordletType = Guess | Master

type WordList :: WordletType -> Type
newtype WordList wlty = WordList { getWordList :: VV.Vector (Wordlet wlty) }
  deriving (Eq, Ord, Show)

toTuple :: forall a . Vector 5 a -> (a, a, a, a, a)
toTuple v = (i @0, i @1, i @2, i @3, i @4)
 where
  i :: forall n . (KnownNat n, n <= 4) => a
  i = V.index' @n @(4 - n) v Proxy

type Wordlet :: WordletType -> Type
newtype Wordlet wlty = Wordlet (Vector 5 Char)
  deriving (Eq, Ord)

instance Show (Wordlet wlty) where
  show (Wordlet wordlet) =
    let (a, b, c, d, e) = toTuple wordlet in ['"', a, b, c, d, e, '"']

instance IsString (Wordlet wlty) where
  fromString [a, b, c, d, e] = Wordlet $ V.fromTuple (a, b, c, d, e)
  fromString _               = error "Wordlet must be 5 characters long"

data Color = Gray | Yellow  | Green
  deriving (Eq, Ord, Show, Enum, Bounded)

newtype Colors = Colors (Vector 5 Color)
  deriving (Eq, Ord)

{-# COMPLETE Wordlet #-}

instance Show Colors where
  show (Colors v) =
    let (a, b, c, d, e) = toTuple v
        toChar Gray   = 'X'
        toChar Yellow = 'Y'
        toChar Green  = 'G'
    in  toChar <$> [a, b, c, d, e]

instance IsString Colors where
  fromString = toRight . parseColors . Text.pack
   where
    toRight (Right a) = a
    toRight (Left  _) = error "parseColors failed"

parseWordList :: Text -> Either Text (WordList wlty)
parseWordList txt = do
  let word_lines = Text.lines txt
  wordlets <- traverse parseWordlet word_lines
  pure $ WordList $ VV.fromList wordlets

parseWordlet :: Text -> Either Text (Wordlet wlty)
parseWordlet txt@(Text.unpack -> [a,b,c,d,e]) | Text.all isAsciiLower txt =
  Right $ Wordlet $ V.fromTuple (a, b, c, d, e)
parseWordlet txt =
  Left $ "Invalid word encountered while parsing word list: " <> txt

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
      <> ": color must be one of 'g', 'y', or 'b'"

parseColors :: Text -> Either Text Colors
parseColors txt = case Text.unpack txt of
  [a, b, c, d, e] ->
    Colors <$> traverse parseColor (V.fromTuple (a, b, c, d, e))
  _ -> Left "Invalid input: input must be 5 characters long"

consistentWith :: Colors -> Wordlet Guess -> Wordlet Master -> Bool
consistentWith colors guess master = guess `againstMaster` master == colors

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

bestGuess :: WordList Guess -> WordList Master -> Wordlet Guess
bestGuess (WordList guesses) masters = case VV.toList $ getWordList masters of
  [x] -> coerce x
  [x, _] -> coerce x
  _ -> preferMaster masters . selectBestCandidates score . VV.toList $ guesses
  where score guess = maximum (makeHitMap guess masters)

preferMaster :: WordList Master -> NonEmpty (Wordlet Guess) -> Wordlet Guess
preferMaster masters =
  maximumBy (comparing $ \g -> coerce g `elem` getWordList masters)

selectBestCandidates
  :: (Wordlet Guess -> Int) -> [Wordlet Guess] -> NonEmpty (Wordlet Guess)
selectBestCandidates score guesses = result
 where
  taggedWithScore = map (\g -> (g, score g)) guesses
  sortedByScore   = sortOn snd taggedWithScore
  bestScoreGroup  = case NE.groupBy ((==) `on` snd) sortedByScore of
    (best : _rest) -> best
    []             -> error "Couldn't find a candidate guess!"
  result = fmap fst bestScoreGroup

makeHitMap :: Wordlet Guess -> WordList Master -> Map Colors Int
makeHitMap guess (WordList masters) =
  Map.fromListWith (+) . VV.toList $ fmap ((, 1) . againstMaster guess) masters

filterMasters :: Colors -> Wordlet Guess -> WordList Master -> WordList Master
filterMasters colors guess (WordList masters) =
  WordList $ VV.filter (consistentWith colors guess) masters

runGame :: WordList Guess -> WordList Master -> IO ()
runGame guesses masters = do
  putStrLn "Do you have a starter word you'd like to use?"
  usrInput <- Text.getLine
  case parseWordlet usrInput of
    Right guess -> singleRound guess masters >>= gameLoop
    Left  _     -> gameLoop masters
 where
  singleRound guess currentMasters = do
    putStrLn "What was the result?"
    let inputLoop = do
          usrInput <- Text.getLine
          case parseColors usrInput of
            Left err -> if usrInput == "done"
              then do
                putStrLn "Congrats!"
                exitSuccess
              else Text.putStrLn err *> inputLoop
            Right colors -> do
              let newMasters = filterMasters colors guess currentMasters
              pure newMasters
    inputLoop

  gameLoop currentMasters = do
    let guess = bestGuess guesses currentMasters
    putStrLn "Thinking..."
    putStrLn $ "Guess this word: " <> show guess
    newMasters <- singleRound guess currentMasters
    gameLoop newMasters
