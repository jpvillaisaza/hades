module Hades.Game.Bagels (bagels) where

-- base
import Data.Char (isDigit, toUpper)
import Data.List (nub, sort)

-- hades
import Hades.Game (Game (..))
import Hades.Lib.Random (genDigits, withStdGen)

-- random
import System.Random (RandomGen)

bagels :: Game
bagels = Game
  { gameName = "Bagels"
  , gameRun = withStdGen Nothing play
  }

play :: RandomGen g => g -> IO ()
play g = do
  let (digits, _) = genDigits g
  putStrLn rules
  putStrLn "O. K.  I HAVE A NUMBER IN MIND."
  playWith 1 digits
  putStrLn "HOPE YOU HAD FUN.  BYE."

playWith :: Int -> String -> IO ()
playWith nGuess digits | nGuess > 20 = do
  putStrLn "OH WELL."
  putStrLn ("THAT'S A TWENTY GUESS.  MY NUMBER WAS " <> digits)
playWith nGuess digits = do
  putStr ("GUESS #" <> show nGuess <> "? ")
  guess <- getLine
  case guessDigits digits guess of
    Left err -> do
      putStrLn (prettyErr err)
      playWith nGuess digits
    Right clues | all (== Bagel) clues -> do
      putStrLn "BAGELS"
      playWith (succ nGuess) digits
    Right clues | all (== Fermi) clues -> do
      putStrLn "YOU GOT IT!!!"
    Right clues -> do
      let clues' = sort (filter (/= Bagel) clues)
      putStrLn (unwords (fmap (fmap toUpper . show) clues'))
      playWith (succ nGuess) digits

guessDigits :: String -> String -> Either Err [Clue]
guessDigits digits guesses
  | length guesses /= 3 = Left ErrLength
  | not (all isDigit guesses) = Left ErrDigits
  | nub guesses /= guesses = Left ErrDups
  | otherwise = Right (zipWith toClue digits guesses)
  where
    toClue digit guess
      | digit == guess = Fermi
      | guess `elem` digits = Pico
      | otherwise = Bagel

data Clue
  = Pico
  | Fermi
  | Bagel
  deriving (Eq, Ord, Show)

data Err
  = ErrDigits
  | ErrDups
  | ErrLength
  deriving (Eq)

prettyErr :: Err -> String
prettyErr err =
  case err of
    ErrDigits ->
      "WHAT?"
    ErrDups ->
      "OH, I FORGOT TO TELL YOU THAT THE NUMBER I HAVE IN MIND\n\
      \HAS NO TWO DIGITS THE SAME"
    ErrLength ->
      "TRY GUESSING A THREE-DIGIT NUMBER."

rules :: String
rules =
  unlines
    [ "I AM THINKING OF A THREE-DIGIT NUMBER.  TRY TO GUESS"
    , "MY NUMBER AND I WILL GIVE YOU CLUES AS FOLLOWS:"
    , "   PICO   - ONE DIGIT CORRECT BUT IN THE WRONG POSITION"
    , "   FERMI  - ONE DIGIT CORRECT AND IN THE RIGHT POSITION"
    , "   BAGELS - NO DIGITS CORRECT"
    ]

----------------------------------------------------------------------
-- * Bibliography
----------------------------------------------------------------------

-- $bib
-- * Ahl, David H. (1978). /BASIC Computer Games/. Workman Publishing.
