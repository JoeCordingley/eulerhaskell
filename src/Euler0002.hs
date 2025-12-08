module Euler0002(answer) where

fibs :: [Int]
fibs = 0 : 1 : (zipWith (+) fibs $ tail fibs)

isEven :: Int -> Bool
isEven x = x `mod` 2 == 0

answer :: Int
answer = sum [x | x <- takeWhile (<= 4000000) fibs, isEven x]
