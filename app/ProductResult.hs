{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ProductResult where

import Data.Aeson (KeyValue ((.=)), ToJSON (toEncoding), pairs)
import Data.Function (on)
import Data.Int (Int64)
import Data.Map.Monoidal (MonoidalMap, singleton)
import Data.Vector (Vector)
import Data.Word (Word32)
import GHC.Generics (Generic)
import Match (Match (Match, price, productSymbol, quantity))

data ProductResult = ProductResult
  { weightedPrice :: Int64,
    volume :: Word32
  }
  deriving (Generic, Show)

vwap :: ProductResult -> Double
vwap = (/) <$> fromIntegral . weightedPrice <*> fromIntegral . volume

matchToResult :: Match -> ProductResult
matchToResult match =
  ProductResult
    { weightedPrice = price match * fromIntegral (quantity match),
      volume = quantity match
    }

-- this will create a map for every item and then mconcat them together.. it probably would be more
-- memory efficient to use foldr to create only one and incrementally mconcat each item
getMatchResults :: Vector Match -> MonoidalMap String ProductResult
getMatchResults = foldMap (\match -> singleton (productSymbol match) (matchToResult match))

instance ToJSON ProductResult where
  toEncoding p@(ProductResult weightedPrice volume) =
    pairs ("vwap" .= vwap p <> "volume" .= volume)

instance Semigroup ProductResult where
  p1 <> p2 =
    ProductResult
      { weightedPrice = weightedPrice p1 + weightedPrice p2,
        volume = volume p1 + volume p2
      }

instance Monoid ProductResult where
  mempty = ProductResult {weightedPrice = 0, volume = 0}