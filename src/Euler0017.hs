module Euler0017 (answer) where

import Data.Monoid (Sum(..))

answer :: Int
answer = getSum $ foldMap (Sum . length . numberwords) [1..1000] where
  numberwords x = case x of
    1 -> "one"
    2 -> "two"
    3 -> "three"
    4 -> "four"
    5 -> "five"
    6 -> "six"
    7 -> "seven"
    8 -> "eight"
    9 -> "nine"
    10 -> "ten"
    11 -> "eleven"
    12 -> "twelve"
    13 -> "thirteen"
    15 -> "fifteen"
    18 -> "eighteen"
    _ | x < 20 -> numberwords (x - 10) ++ "teen"
    _ | x < 30 -> "twenty" ++ zeroEmpty numberwords (x - 20)
    _ | x < 40 -> "thirty" ++ zeroEmpty numberwords (x - 30)
    _ | x < 50 -> "forty" ++ zeroEmpty numberwords (x - 40)
    _ | x < 60 -> "fifty" ++ zeroEmpty numberwords (x - 50)
    _ | x >= 80 && x < 90  -> "eighty" ++ zeroEmpty numberwords (x - 80)
    _ | x < 100 -> numberwords (x `div` 10) ++ "ty" ++ zeroEmpty numberwords (x `mod` 10)
    _ | x < 1000 -> numberwords (x `div` 100) ++ "hundred" ++ zeroEmpty (\n -> "and" ++ numberwords n) (x `mod` 100)
    1000 -> "onethousand"
    _ -> undefined
  zeroEmpty _ 0 = ""
  zeroEmpty f n = f n

  
