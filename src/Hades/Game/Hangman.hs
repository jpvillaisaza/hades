module Hades.Game.Hangman (hangmanCommand) where

-- base
import Control.Monad (unless)

-- hades
import Hades.Lib.Random (genWord, withStdGen)

-- optparse-applicative
import Options.Applicative

-- random
import System.Random (RandomGen)

data Opt = Opt
  { optSeed :: Maybe Int
  }

optParser :: Parser Opt
optParser =
  Opt <$> optional seedParser
  where
    seedParser =
      option auto
        (long "seed"
        <> metavar "SEED"
        <> help "the seed"
        )

run :: Opt -> IO ()
run opt = do
  withStdGen (optSeed opt) run'

hangmanCommand :: Mod CommandFields (IO ())
hangmanCommand = command "hangman"
  (info (run <$> optParser)
        (progDesc "Play Hangman"))

run' :: RandomGen g => g -> IO ()
run' g = do
  (word, _) <- genWord g
  play 1 word (fmap (, False) word) mempty

play :: Int -> String -> [(Char, Bool)] -> [Char] -> IO ()
play n word guessed previous = do
  putStrLn (fmap (\(x, b) -> if b then x else '-') guessed)
  unless (null previous) (putStrLn ("Previous guesses: " <> previous))
  putStr ("Guess " <> show n <> "? ")
  guess <- getLine
  case guess of
    [] ->
      play n word guessed previous
    [guess1] -> do
      let new = update guess1 guessed
      if and (fmap snd new)
        then
          putStrLn "You got it!"
        else
          play (succ n) word new (guess1:previous)
    _ ->
      if guess == word
        then
          putStrLn "You got it!"
        else
          play (succ n) word guessed previous

update :: Char -> [(Char, Bool)] -> [(Char, Bool)]
update guess guessed =
  [(letter, isGuessed || guess == letter) | (letter, isGuessed) <- guessed]

----------------------------------------------------------------------
-- * Bibliography
----------------------------------------------------------------------

-- $bib
-- * Ahl, David H. (1978). /BASIC Computer Games/. Workman Publishing.
-- * Hutton, Graham (2016). /Programming in Haskell/. 2nd ed.
--   Cambridge University Press.
