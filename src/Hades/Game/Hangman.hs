module Hades.Game.Hangman (game, update) where

-- base
import Control.Monad (unless)

-- hades
import Hades.Game (Game(..), mSeedParser, mWordParser)
import Hades.Lib.Random (genWord, withStdGen)

-- optparse-applicative
import Options.Applicative (Parser)

game :: Game Opt
game = Game
  { gameName = "Hangman"
  , gameDescription = "Hangman"
  , gameParser = optParser
  , gameRunner = run
  }

data Opt = Opt
  { optSeed :: Maybe Int
  , optWord :: Maybe String
  }

optParser :: Parser Opt
optParser =
  Opt <$> mSeedParser <*> mWordParser

run :: Opt -> IO ()
run opt = do
  word <-
    case optWord opt of
      Just word ->
        pure word
      Nothing ->
        withStdGen (optSeed opt) $ \g -> do
          (word, _) <- genWord g
          pure word
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

-- |
--
-- >>> update 'a' [('a', False)]
-- [('a', True)]
-- >>> update 'b' [('a', False)]
-- [('a', False)]

update :: Char -> [(Char, Bool)] -> [(Char, Bool)]
update guess =
  fmap (\(letter, guessed) -> (letter, guessed || guess == letter))

----------------------------------------------------------------------
-- * Bibliography
----------------------------------------------------------------------

-- $bib
-- * Ahl, David H. (1978). /BASIC Computer Games/. Workman Publishing.
-- * Hutton, Graham (2016). /Programming in Haskell/. 2nd ed.
--   Cambridge University Press.
