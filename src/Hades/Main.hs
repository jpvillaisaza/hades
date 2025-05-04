module Hades.Main (main) where

-- base
import System.Environment (getArgs)

-- hades
import Hades.Game.Bagels (bagels)
import Hades.Game.Hangman (hangman)
import Hades.Game.Word (word)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["bagels"] ->
      bagels
    ["hangman"] ->
      hangman
    ["word"] ->
      word
    _ ->
      pure ()
