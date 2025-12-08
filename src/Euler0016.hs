module Euler0016 (answer) where

import Data.List (singleton)

answer :: Int
answer = sum . map (read . singleton) . show $ pow 2 1000 

pow :: Integer -> Integer -> Integer
pow = pow' 1 where
  pow' a _ 0 = a
  pow' a n i = if (i `mod` 2 == 0) then pow' a (n * n) (i `div` 2) else pow' (a * n) n (i - 1)
