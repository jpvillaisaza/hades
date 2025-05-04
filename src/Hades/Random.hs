module Hades.Random
  ( genElement
  , genLine
  , genLineOf
  , genWord
  , genWordOf
  , withStdGen
  )where

--base
import Control.Monad.IO.Class (MonadIO)

-- bytestring
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

-- random
import System.Random

genElement :: RandomGen g => [a] -> g -> (a, g)
genElement xs g0 =
  let (i, g1) = uniformR (0, length xs - 1) g0 in (xs !! i, g1)

genWord :: RandomGen g => g -> IO (String, g)
genWord =
  genWordOf (const True)

genWordOf :: RandomGen g => (String -> Bool) -> g -> IO (String, g)
genWordOf =
  genLineOf "/usr/share/dict/words"

genLine :: RandomGen g => FilePath -> g -> IO (String, g)
genLine fp =
  genLineOf fp (const True)

genLineOf :: RandomGen g => FilePath -> (String -> Bool) -> g -> IO (String, g)
genLineOf fp p g = do
  contents <- filter f . BS8.lines <$> BS.readFile fp
  let (a, b) = genElement contents g
  pure (BS8.unpack a, b)
  where
    f word =
      p (BS8.unpack word)

withStdGen :: MonadIO m => Maybe Int -> (StdGen -> m a) -> m a
withStdGen mSeed action =
  maybe initStdGen (pure . mkStdGen) mSeed >>= action
