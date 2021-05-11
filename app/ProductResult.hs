{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ProductResult where

import Data.Aeson (KeyValue ((.=)), ToJSON (toEncoding), pairs)
import Match (Match (price, quantity))
import Data.Function (on)
import GHC.Generics (Generic)
import Data.Int ( Int64 )
import Data.Word ( Word32 )

data ProductResult = ProductResult
  { weightedPrice :: Int64,
    volume :: Word32
  }
  deriving (Generic, Show)

vwap :: ProductResult -> Float
vwap = (/) <$> fromIntegral . weightedPrice <*> fromIntegral . volume

matchToResult :: Match -> ProductResult
matchToResult match = ProductResult {
  weightedPrice = price match * fromIntegral (quantity match),
  volume = quantity match
  }

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
