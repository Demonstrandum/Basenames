module Main where

import Basenames (bases)

import System.Environment
import Data.List (intercalate)

main :: IO ()
main = do
  args <- getArgs
  let (lower, upper) = bounds args
  let names = drop lower . take (upper + 1) $ zip [0..] bases
  putStrLn $ intercalate "\n" $ map showPair names

  where showPair :: (Int, String) -> String
        showPair (n, s) = show n ++ ": " ++ s
        bounds [] = (0, 35)
        bounds [v] = (read v :: Int, read v :: Int)
        bounds [l, u] = (read l :: Int, read u :: Int)
        bounds _ = error "unsupported number of arguments."
