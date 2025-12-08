module Euler0006(answer) where

answer :: Int
answer = n*(n+1)*(3*n*(n+1) - 2*(2*n+1)) `div` 12 where
  n = 100
