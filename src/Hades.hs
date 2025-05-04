module Hades (main) where

-- base
import Data.Char (toLower)
import Data.List (find)
import System.Environment (getArgs)
import System.Exit (die)

-- hades
import Hades.Game (Game(..))
import Hades.Game.Bagels (bagels)
import Hades.Game.Hangman (hangman)
import Hades.Game.Word (word)

games :: [Game]
games =
  [ bagels
  , hangman
  , word
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [g] ->
      case find (\game -> fmap toLower (gameName game) == g) games of
        Just game ->
          gameRun game
        Nothing ->
          die ("Unknown game: " <> g)
    _ ->
      pure ()
