module Hades.Game.Hangman (hangman) where

-- base
import Control.Monad (unless)

-- hades
import Hades.Lib.Random (genWord, withStdGen)

-- random
import System.Random (RandomGen)

hangman :: IO ()
hangman = do
  putStrLn "Hangman"
  withStdGen Nothing run'

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
