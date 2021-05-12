module Main where

import Data.Aeson (encode)
import Data.ByteString.Lazy (getContents, putStr)
import Data.Csv (HasHeader (NoHeader), decode)
import ProductResult (ProductResult (ProductResult, volume, weightedPrice), getMatchResults)
import Prelude hiding (getContents, putStr)

main :: IO ()
main = do
  result <- decode NoHeader <$> getContents
  case result of
    Left error -> putStrLn error
    Right matches -> putStr $ encode $ getMatchResults matches