module Euler7(answer, primes) where

answer :: Int
answer = primes !! index where
 index = 10001 - 1

primes :: [Int]
primes = 2 : filter isPrime [3..]

isPrime :: Int -> Bool
isPrime n = all (\p -> n `mod` p /= 0) $ takeWhile (\p -> p*p <= n) primes
