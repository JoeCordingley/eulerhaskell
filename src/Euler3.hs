module Euler3(answer) where

import Data.List (find)
import Data.Maybe (fromJust)

primeFactors :: Int -> [Int]
primeFactors 1 = []
primeFactors i = firstPrime : (primeFactors $ i `div` firstPrime ) where
  firstPrime = fromJust $ find (\p -> i `mod` p == 0) primes

primes :: [Int]
primes = 2 : filter isPrime [3 ..]

isPrime :: Int -> Bool
isPrime i = not . any (\p -> i `mod` p == 0) $ takeWhile (\p -> p*p <= i) primes

answer :: Int
answer = last $ primeFactors 600851475143
