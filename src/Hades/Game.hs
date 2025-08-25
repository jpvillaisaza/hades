module Hades.Game
  ( Game(..)
  , mkGameCommand
  , mSeedParser
  , mWordParser
  ) where

-- base
import Data.Char (toLower)

-- optparse-applicative
import Options.Applicative

data Game opt = Game
  { gameName :: String
  , gameDescription :: String
  , gameOptParser :: Parser opt
  , gameRunner :: opt -> IO ()
  }

mkGameCommand :: Game opt -> Mod CommandFields (IO ())
mkGameCommand game =
  command (fmap toLower (gameName game)) parserInfo
  where
    parserInfo =
      info
        (gameRunner game <$> gameOptParser game)
        (progDesc (gameDescription game))

seedParser :: Parser Int
seedParser =
  option auto $
    long "seed"
      <> metavar "SEED"

mSeedParser :: Parser (Maybe Int)
mSeedParser =
  optional seedParser

wordParser :: Parser String
wordParser =
  strOption $
    long "word"
      <> metavar "WORD"

mWordParser :: Parser (Maybe String)
mWordParser =
  optional wordParser
