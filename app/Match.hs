{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Match where

import Data.Csv (FromField (..), FromRecord)
import GHC.Generics (Generic)
import Data.Word ( Word32 )
import Data.Int ( Int64 )

data Side = Bid | Ask deriving (Generic, Show)

instance FromField Side where
  parseField "Bid" = pure Bid
  parseField "Ask" = pure Ask
  parseField _ = fail "Invalid format for Side"

data Match = Match
  { makerAccountId :: !String,
    takerAccountId :: !String,
    productSymbol :: !String,
    takerSide :: !Side,
    price :: !Int64,
    quantity :: !Word32
  }
  deriving (Generic, Show)

instance FromRecord Match