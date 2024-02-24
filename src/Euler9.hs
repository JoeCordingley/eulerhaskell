module Euler9(answer) where


answer :: Int
answer = productTriple . find (addsTo 1000) $ pythagoreanTriples where
  productTriple (a, b, c) = a*b*c
  addsTo n (a, b, c) = a + b + c == n

pythagoreanTriples :: [(Int, Int, Int)]
pythagoreanTriples = fmap generateTriple pairs

pairs :: [(Int, Int)]
pairs = [(x,y) | y <- [2..], x <- [1..(y-1)]]

generateTriple :: (Int, Int) -> (Int, Int, Int)
generateTriple (u, v) = (v*v - u*u, 2*u*v, v*v + u*u)

find :: (a -> Bool) -> [a] -> a
find _ [] = undefined
find p (a:as) = if p a then a else find p as

