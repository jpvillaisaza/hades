module Hades.Main (main) where

-- hades
import Hades.Game.Bagels (bagelsCommand)
import Hades.Game.Hangman (hangmanCommand)
import Hades.Game.Word (wordCommand)

-- optparse-applicative
import Options.Applicative

main :: IO ()
main = do
  runCmd <- customExecParser (prefs showHelpOnEmpty) description
  runCmd

description :: ParserInfo (IO ())
description =
  info (games <**> helper <**> simpleVersioner "TODO")
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
