module Euler0010(answer) where

answer :: Int
answer = sum $ takeWhile (< 2000000) primes

primes :: [Int]
primes = 2 : filter isPrime [3..]

isPrime :: Int -> Bool
isPrime n = all (\p -> n `mod` p /= 0) $ takeWhile (\p -> p*p <= n) primes
