module Main where

import Data.Aeson (encode)
import Data.ByteString.Lazy (getContents, putStr)
import Data.Csv (HasHeader (NoHeader), decode)
import Data.Map.Monoidal (MonoidalMap, singleton)
import Data.Vector (Vector)
import Match (Match (Match, productSymbol))
import ProductResult (ProductResult (ProductResult, volume, weightedPrice), matchToResult)
import Prelude hiding (getContents, putStr)

-- this will create a map for every item and then mconcat them together.. it probably would be more 
-- memory efficient to use foldr to create only one and incrementally mconcat each item
getMatchResults :: Vector Match -> MonoidalMap String ProductResult
getMatchResults = foldMap (\match -> singleton (productSymbol match) (matchToResult match))

main :: IO ()
main = do
  result <- decode NoHeader <$> getContents
  case result of
    Left error -> putStrLn error
    Right matches -> putStr $ encode $ getMatchResults matches