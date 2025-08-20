module Hades.Game.HangmanSpec (spec) where

-- QuickCheck
import Test.QuickCheck

-- base
import Data.List (partition)

-- hades
import Hades.Game.Hangman

-- hspec
import Test.Hspec (Spec, describe, context)
import Test.Hspec.QuickCheck

spec :: Spec
spec =
  describe "update" $ do
    context "when the guess is not in the word" $ do
      prop "does not update the guessed letters (empty word)" $
        \guess ->
          update guess mempty == mempty
      prop "does not update the guessed letters" $
        forAll arbitrary $ \guessed -> do
          let sd guess = guess `notElem` fmap fst guessed
          forAll (arbitrary `suchThat` sd) $ \guess ->
            update guess guessed == guessed
    context "when the guess is in the word" $ do
      prop "updates the guessed letters" $
        forAll arbitrary $ \guessed1 -> do
          let sd guess = guess `elem` fmap fst guessed1
          forAll (arbitrary `suchThat` sd) $ \guess ->
            let
              guessed2 = update guess guessed1
              (same, rest) =
                partition ((== guess) . fst) guessed2
            in
              all snd same
                && rest == filter ((/= guess) . fst) guessed1

    prop "is idempotent" $
      \guess guessed ->
        update guess (update guess guessed) == update guess guessed

    prop "preserves the original letters and their order" $
      \guess guessed ->
        fmap fst (update guess guessed) == fmap fst guessed
