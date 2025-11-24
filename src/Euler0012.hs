module Euler12 (answer) where

answer :: Int
answer = find ((> 500) . numDivisors) triangles

numDivisors :: Int -> Int
numDivisors = product . map (+ 1) . groupCounts . primeDecomposition

primeDecomposition :: Int -> [Int]
primeDecomposition = primeDecomposition' primes
  where
    primeDecomposition' _ 1 = []
    primeDecomposition' [] _ = undefined
    primeDecomposition' (p : ps) n
        | n `mod` p == 0 = p : primeDecomposition' (p : ps) (n `div` p)
        | p * p > n = [n]
        | otherwise = primeDecomposition' ps n

groupCounts :: (Eq a) => [a] -> [Int]
groupCounts [] = []
groupCounts (a : as) = groupCounts' a 1 as
  where
    groupCounts' _ n [] = [n]
    groupCounts' a' n (b : bs)
        | a' == b = groupCounts' a' (n + 1) bs
        | otherwise = n : groupCounts' b 1 bs

triangles :: [Int]
triangles = map (\i -> i * (i + 1) `div` 2) [1 ..]

find :: (a -> Bool) -> [a] -> a
find _ [] = undefined
find p (a : as) = if p a then a else find p as

primes :: [Int]
primes = 2 : filter isPrime [3 ..]

isPrime :: Int -> Bool
isPrime n = all (\p -> n `mod` p /= 0) $ takeWhile (\p -> p * p <= n) primes
