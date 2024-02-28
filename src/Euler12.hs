module Euler12(answer) where

import qualified Data.Map as Map
import Data.Map (Map)

answer :: Int
answer = find ((>500) . numDivisors) triangles

numDivisors :: Int -> Int
numDivisors = product . map (+1) . Map.elems . countElems . primeDecomposition

primeDecomposition :: Int -> [Int]
primeDecomposition 1 = []
primeDecomposition n = first : primeDecomposition (n `div` first) where
  first = find (\p -> n `mod` p == 0) $ takeWhile (\p -> p*p <= n) primes <> [n]

countElems :: Ord a => [a] -> Map a Int
countElems = foldr (\i -> Map.insertWith (+) i 1) Map.empty

triangles :: [Int]
triangles = map (\i -> i * (i + 1) `div` 2) [1..]

find :: (a -> Bool) -> [a] -> a
find _ [] = undefined
find p (a:as) = if p a then a else find p as

primes :: [Int]
primes = 2 : filter isPrime [3..]

isPrime :: Int -> Bool
isPrime n = all (\p -> n `mod` p /= 0) $ takeWhile (\p -> p*p <= n) primes
