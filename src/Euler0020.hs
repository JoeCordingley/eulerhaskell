module Euler20 (answer) where

answer :: Integer
answer = sumOfDigits . factorial $ 100

sumOfDigits :: Integer -> Integer
sumOfDigits 0 = 0
sumOfDigits i = (i `mod` 10) + sumOfDigits (i `div` 10)

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)
