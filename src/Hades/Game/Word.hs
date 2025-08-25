module Hades.Game.Word (game) where

-- hades
import Hades.Game (Game(..), mSeedParser, mWordParser)
import Hades.Lib.Random (genWordOf, withStdGen)

-- optparse-applicative
import Options.Applicative (Parser)

game :: Game Opt
game = Game
  { gameName = "Word"
  , gameDescription = "Word"
  , gameOptParser = optParser
  , gameRunner = runner
  }

data Opt = Opt
  { optSeed :: Maybe Int
  , optWord :: Maybe String
  }

optParser :: Parser Opt
optParser =
  Opt <$> mSeedParser <*> mWordParser

runner :: Opt -> IO ()
runner opt = do
  word <-
    case optWord opt of
      Just word ->
        pure word
      Nothing ->
        withStdGen (optSeed opt) $ \g -> do
          (word, _) <- genWordOf (\s -> length s == 5) g
          pure word
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
      let exact = update guess exacts
      putStrLn ms
      putStrLn (fmap (\(l, g) -> if g then l else '-') exact)
      if word == guess
        then putStrLn ("It took " <> show attempt <> " guesses!")
        else play (succ attempt) word exact

-- |
--
-- >>> update "ab" [('a', False), ('b', False)]
-- [('a', True), ('b', True)]
-- >>> update "ba" [('a', False), ('b', False)]
-- [('a', False), ('b', False)]

update :: String -> [(Char, Bool)] -> [(Char, Bool)]
update guess =
  zipWith (\x (l, g) -> (l, x == l || g)) guess

----------------------------------------------------------------------
-- * Bibliography
----------------------------------------------------------------------

-- $bib
-- * Ahl, David H. (1978). /BASIC Computer Games/. Workman Publishing.
