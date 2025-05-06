module Hades.Game.Word (wordCommand) where

-- hades
import Hades.Lib.Random (genWordOf, withStdGen)

-- optparse-applicative
import Options.Applicative

-- random
import System.Random (RandomGen)

data Opt = Opt
  { optSeed :: Maybe Int
  }

optParser :: Parser Opt
optParser =
  Opt <$> optional seedParser
  where
    seedParser =
      option auto
        (long "seed"
        <> metavar "SEED"
        <> help "the seed"
        )

wordCommand :: Mod CommandFields (IO ())
wordCommand = command "word"
  (info (run <$> optParser)
        (progDesc "Play word"))

run :: Opt -> IO ()
run opt =
  withStdGen (optSeed opt) run'

run' :: RandomGen g => g -> IO ()
run' g = do
  (word, _) <- genWordOf (\s -> length s == 5) g
  play 1 word (fmap (, False) word)

play :: Int -> String -> [(Char, Bool)] -> IO ()
play attempt word exacts = do
  putStr "GUESS? "
  guess <- getLine
  case guess of
    "?" ->
      putStrLn ("THE SECRET WORD IS " <> show word)
    gs | length gs /= 5 -> do
      putStrLn "YOU MUST GUESS A 5 LETTER WORD. START AGAIN."
      play attempt word exacts
    _ -> do
      let ms = [x | x <- word, x `elem` guess]
      let exact = zipWith (\x (l, g) -> (l, x == l || g)) guess exacts
      putStrLn ms
      putStrLn (fmap (\(l, g) -> if g then l else '-') exact)
      if word == guess
        then putStrLn ("It took " <> show attempt <> " guesses!")
        else play (succ attempt) word exact

----------------------------------------------------------------------
-- * Bibliography
----------------------------------------------------------------------

-- $bib
-- * Ahl, David H. (1978). /BASIC Computer Games/. Workman Publishing.
