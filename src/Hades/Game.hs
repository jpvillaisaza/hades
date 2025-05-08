module Hades.Game
  ( Game(..)
  , mkGameCommand
  , mSeedParser
  ) where

-- base
import Data.Char (toLower)

-- optparse-applicative
import Options.Applicative

data Game opt = Game
  { gameName :: String
  , gameDescription :: String
  , gameParser :: Parser opt
  , gameRunner :: opt -> IO ()
  }

mkGameCommand :: Game a -> Mod CommandFields (IO ())
mkGameCommand game =
  command (fmap toLower (gameName game)) parserInfo
  where
    parserInfo =
      info
        (gameRunner game <$> gameParser game)
        (progDesc (gameDescription game))

seedParser :: Parser Int
seedParser =
  option auto $
    long "seed"
      <> metavar "SEED"

mSeedParser :: Parser (Maybe Int)
mSeedParser =
  optional seedParser
