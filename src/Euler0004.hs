module Euler0004(answer) where

answer :: Int
answer = maximum $ filter isPalendrome products

isPalendrome :: Int -> Bool
isPalendrome i = si == (reverse $ si) where si = show i

products :: [Int]
products = [i*j | i <- [111..999], j <- [i..999]]
