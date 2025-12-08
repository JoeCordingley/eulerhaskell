module Euler0005(answer) where


answer :: Int
answer = foldr lcf 1 [2..20]

lcf :: Int -> Int -> Int
lcf a b = (a * b) `div` (gcd a b)

