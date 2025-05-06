module Hades.Main (main) where

-- base
import Data.Version (showVersion)

-- hades
import Hades.Game.Bagels (bagelsCommand)
import Hades.Game.Hangman (hangmanCommand)
import Hades.Game.Word (wordCommand)
import Paths_hades (version)

-- optparse-applicative
import Options.Applicative

main :: IO ()
main = do
  runCmd <- customExecParser (prefs showHelpOnEmpty) description
  runCmd

description :: ParserInfo (IO ())
description =
  info (games <**> helper <**> simpleVersioner (showVersion version))
    (fullDesc
    <> progDesc "prog desc"
    <> header "hades header"
    )

games :: Parser (IO ())
games =
  hsubparser
    (commandGroup "Available games:"
    <> bagelsCommand
    <> hangmanCommand
    <> wordCommand
    <> metavar "GAME"
    )
