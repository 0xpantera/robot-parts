module Main where

import Lib.Types
import Lib
import qualified Data.Map as Map
import Data.Maybe

main :: IO ()
main = do
  putStrLn "First part ID:"
  id1 <- getLine
  putStrLn "Second part ID:"
  id2 <- getLine
  let part1 = Map.lookup (read id1) partsDB
  let part2 = Map.lookup (read id2) partsDB
  let cheaper = (minPart) <$> part1 <*> part2
  let returnPart = mconcat ["The cheapest part is "
                           , (name cheapPart)]
        where cheapPart = fromJust cheaper
  putStrLn returnPart
