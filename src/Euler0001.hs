module Euler1(answer) where

naturalNumbers :: [Int]
naturalNumbers = [0..]

answer :: Int
answer = sum [x | x <- takeWhile (<1000) naturalNumbers , x `mod` 3 == 0 || x `mod` 5 == 0]
