{-# LANGUAGE NamedFieldPuns #-}

module Euler0019 (answer) where

import Data.Bool (bool)
import Data.Monoid (Sum (..))

answer :: Int
answer = length . filter ((== Sunday) . day) . dropWhile ((< 1901) . year) $ takeWhile ((< 2001) . year) firstOfMonths

data DayOfWeek = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Eq, Enum, Show)

data Month = January | February | March | April | May | June | July | August | September | October | November | December deriving (Eq, Show, Bounded, Ord)

months :: [Month]
months = [January, February, March, April, May, June, July, August, September, October, November, December]

data FirstOfMonth = FirstOfMonth {year :: Int, day :: DayOfWeek} deriving (Show)

count :: (a -> Bool) -> [a] -> Int
count f = getSum . foldMap (Sum . bool 0 1 . f)

numberOfDays :: (Integral a1, Num a2) => a1 -> Month -> a2
numberOfDays year month = case month of
  January -> 31
  February -> if year `mod` 4 == 0 && (year `mod` 100 /= 0 || year `mod` 400 == 0) then 29 else 28
  March -> 31
  April -> 30
  May -> 31
  June -> 30
  July -> 31
  August -> 31
  September -> 30
  October -> 31
  November -> 30
  December -> 31

firstOfMonths :: [FirstOfMonth]
firstOfMonths = FirstOfMonth {year = 1900, day = Monday} : firstOfMonths' (nextDay 1900 January Monday) (tail yearMonths)
  where
    firstOfMonths' day ((year, month) : rest) = FirstOfMonth {year, day} : firstOfMonths' (nextDay year month day) rest
    nextDay year month day = toEnum $ (fromEnum day + numberOfDays year month) `mod` 7
    yearMonths = (,) <$> [1900 ..] <*> months
