module Main where

import           Control.Monad  (replicateM)
import           Criterion.Main
import           System.Random  (randomIO)

main :: IO ()
main = do
  listOfInt <- randomListOfInt 10000000 -- generate 10 million random numbers
  defaultMain
    [ bgroup
        "quicksort" -- benchmark the quicksort on those 10 million numbers
        [ bench "array of 10 million numbers using idiomatic Haskell" $
          whnf quicksort listOfInt
        ]
    ]

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (p:xs) = quicksort lesser ++ [p] ++ quicksort greater
  where
    lesser = filter (< p) xs
    greater = filter (>= p) xs

randomListOfInt :: Int -> IO [Int]
randomListOfInt n = replicateM n randomIO
