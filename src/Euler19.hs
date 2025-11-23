module Euler19 (answer) where

import Data.Bool (bool)
import Data.Monoid (Sum(..))
answer :: Int
answer = count ((== Sunday) . day) . dropWhile ((< 1901) . year) $ takeWhile ((< 2001) . year) firstOfMonths 


data DayOfWeek = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Eq, Show, Enum, Bounded, Ord)
data Month = January | February | March | April | May | June | July | August | September | October | November | December deriving (Eq, Show, Bounded, Ord)

data FirstOfMonth = FirstOfMonth { year :: Int, month :: Month, day :: DayOfWeek}

count :: (a -> Bool) -> [a] -> Int
count f = getSum . foldMap (Sum . bool 1 0 . f)

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
firstOfMonths = unfold (FirstOfMonth {year = 1900, month = January, day = Monday}) where
  unfold firstOfMonth = undefined

advance :: (Enum a, Bounded a) => Int -> a -> a
advance n = toEnum . (`mod`cardinality) . (+n) . fromEnum where
  cardinality = (maxBound :: a) - (minBound :: a) + 1
