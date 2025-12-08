module Euler0021 (answer) where

answer :: Int
answer = sum . takeWhile (< 10000) $ amicableNumbers

amicableNumbers :: [Int]
amicableNumbers = filter isamicable [1 ..]

isamicable :: Int -> Bool
isamicable a = a /= b && sumOfProperDivisors b == a
  where
    b = sumOfProperDivisors a
    sumOfProperDivisors = sum . properDivisors

properDivisors :: Int -> [Int]
properDivisors i = filter (divides i) [1 .. (i - 1)]
  where
    divides i j = i `mod` j == 0
