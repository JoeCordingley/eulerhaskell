module Euler0022 (answer) where

import Data.List (intercalate, sort)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Monoid (Sum (..))

answer :: IO Int
answer = sum . zipWith score [1 ..] . sort <$> readCommaSeparatedStringsFromFile "0022_names.txt"

readCommaSeparatedStringsFromFile :: String -> IO [String]
readCommaSeparatedStringsFromFile filename = do
  contents <- readFile filename
  return (removeQuotes <$> splitOn ',' contents)

removeQuotes :: String -> String
removeQuotes = tail . init

splitOn :: (Foldable t, Eq a) => a -> t a -> [[a]]
splitOn a = foldr f [[]]
  where
    f x (xs : xss) = if x == a then [] : xs : xss else (x : xs) : xss

score :: Int -> String -> Int
score index s = index * value s
  where
    value = getSum . foldMap (Sum . charScore)

charScore :: Char -> Int
charScore = (!) charScores

charScores :: Map Char Int
charScores = Map.fromList $ zip ['A' .. 'Z'] [1 ..]
