module Hades.Main (main) where

-- base
import Control.Monad (join)
import Data.Version (showVersion)
import System.IO (BufferMode(..), hSetBuffering, stdin, stdout)

-- hades
import Hades.Game (mkGameCommand)
import qualified Hades.Game.Bagels as Bagels (game)
import qualified Hades.Game.Hangman as Hangman (game)
import qualified Hades.Game.Word as Word (game)
import Paths_hades (version)

-- optparse-applicative
import Options.Applicative

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout NoBuffering
  join (customExecParser (prefs showHelpOnEmpty) parserInfo)
  where
    parserInfo =
      info
        (gameCommands <**> helper <**> simpleVersioner (showVersion version))
        (fullDesc <> header "Hades")

gameCommands :: Parser (IO ())
gameCommands =
  hsubparser $
    commandGroup "Available games:"
      <> mkGameCommand Bagels.game
      <> mkGameCommand Hangman.game
      <> mkGameCommand Word.game
      <> metavar "GAME"
