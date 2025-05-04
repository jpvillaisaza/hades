module Hades.Game (Game(..)) where

-- base
import System.Console.GetOpt (OptDescr)

data Game = Game
  { gameName :: String
  , gameRun :: IO ()
  }
