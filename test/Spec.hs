{-# LANGUAGE FlexibleInstances #-}

module Main where

import Control.Monad (unless)
import Data.Map.Monoidal (lookup)
import Data.Maybe (fromJust)
import Data.Vector (Vector, filter, fromList, head, sum)
import Data.Word (Word32)
import Debug.Trace ()
import Match
  ( Match (Match, price, productSymbol, quantity),
    Side (..),
  )
import ProductResult
  ( ProductResult (volume),
    getMatchResults,
    vwap,
  )
import System.Exit (exitFailure)
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    Gen,
    elements,
    isSuccess,
    listOf1,
    quickCheckResult,
    suchThat,
  )
import Prelude hiding (filter, head, lookup, null, sum)

instance Arbitrary Side where
  arbitrary = elements [Bid, Ask]

instance Arbitrary Match where
  arbitrary =
    Match
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> (arbitrary :: Gen Word32) `suchThat` (> 0)

instance Arbitrary (Vector Match) where
  arbitrary = fromList <$> listOf1 arbitrary

prop_volume :: Vector Match -> Bool
prop_volume matches =
  let first = head matches
      symbol = productSymbol first
      matching = filter (\m -> productSymbol m == symbol) matches
      v = sum $ fmap quantity matching
   in volume (fromJust (lookup symbol (getMatchResults matches))) == v

prop_vwap :: Vector Match -> Bool
prop_vwap matches =
  let first = head matches
      symbol = productSymbol first
      matching = filter (\m -> productSymbol m == symbol) matches
      v = sum $ fmap quantity matching
      wp = sum $ fmap (\m -> price m * fromIntegral (quantity m)) matching
   in vwap (fromJust (lookup symbol (getMatchResults matches))) == fromIntegral wp / fromIntegral v

main :: IO ()
main = do
  let tests = [quickCheckResult prop_volume, quickCheckResult prop_vwap]
  success <- fmap (Prelude.all isSuccess) . Prelude.sequence $ tests
  unless success exitFailure