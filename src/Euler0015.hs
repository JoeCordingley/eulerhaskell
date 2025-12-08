{-# LANGUAGE FlexibleContexts #-}

module Euler0015(answer) where

answer :: Integer
answer = fact 40 `div` (square $ fact 20)

fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n - 1)

square :: Integer -> Integer
square i = i * i
