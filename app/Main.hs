{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Aeson (encode)
import Data.ByteString.Lazy (getContents, putStr)
import Data.Csv (HasHeader (NoHeader), decode)
import Data.Map.Monoidal (MonoidalMap, singleton)
import Data.Vector (Vector)
import Match (Match (Match, price, productSymbol, quantity))
import ProductResult (ProductResult (ProductResult, volume, weightedPrice))
import Prelude hiding (getContents, putStr)

getMatchResults :: Vector Match -> MonoidalMap String ProductResult
getMatchResults = foldMap (\match -> singleton (productSymbol match) (toResult match))
  where
    toResult match = ProductResult {weightedPrice = price match * quantity match, volume = quantity match}

main :: IO ()
main = do
  result <- decode NoHeader <$> getContents
  case result of
    Left error -> putStrLn error
    Right matches -> putStr $ encode $ getMatchResults matches
