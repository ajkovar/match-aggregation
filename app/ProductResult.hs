{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ProductResult where

import Data.Aeson (KeyValue ((.=)), ToJSON (toEncoding), pairs)
import Data.Function (on)
import GHC.Generics (Generic)

data ProductResult = ProductResult
  { weightedPrice :: Int,
    volume :: Int
  }
  deriving (Generic, Show)

vwap :: ProductResult -> Float
vwap = ((/) `on` fromIntegral) <$> weightedPrice <*> volume

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
