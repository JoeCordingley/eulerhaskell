{-# LANGUAGE FlexibleContexts #-}

module Euler0023 (answer) where

import Control.Monad (filterM)
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map

answer :: Int
answer = evalState (sum <$> numbersNotSumOfTwoAbundantNumbers) Map.empty

numbersNotSumOfTwoAbundantNumbers :: (MonadState (Map Int Bool) f) => f [Int]
numbersNotSumOfTwoAbundantNumbers = filterM (fmap not . isSumOfPairOfAbundantNumbers) $ takeWhile (<= 28123) positiveNumbers

abundantNumbersMemoized :: (MonadState (Map Int Bool) f) => Int -> f [Int]
abundantNumbersMemoized max = filterM isAbundantMemoized $ takeWhile (<= max) positiveNumbers

isAbundant :: Int -> Bool
isAbundant n = (sum $ properDivisors n) > n

isAbundantMemoized :: (MonadState (Map Int Bool) f) => Int -> f Bool
isAbundantMemoized = memoize isAbundant

positiveNumbers :: [Int]
positiveNumbers = [1 ..]

properDivisors :: Int -> [Int]
properDivisors n = filter (`divides` n) [1 .. (n - 1)]
  where
    a `divides` b = b `mod` a == 0

isSumOfPairOfAbundantNumbers :: (MonadState (Map Int Bool) f) => Int -> f Bool
isSumOfPairOfAbundantNumbers n = do
  as <- abundantNumbersMemoized (n - 1)
  anyM (\a -> isAbundantMemoized (n - a)) as

anyM :: (Applicative f) => (a -> f Bool) -> [a] -> f Bool
anyM f = foldr g (pure False)
  where
    g a fb = (||) <$> f a <*> fb

memoize :: (Ord a, MonadState (Map a b) f) => (a -> b) -> a -> f b
memoize f a = gets (Map.lookup a) >>= maybe (returning (modify . Map.insert a) (f a)) return where

returning :: (Monad f) => (a -> f b) -> a -> f a
returning f a = a <$ f a
