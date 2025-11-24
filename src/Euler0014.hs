{-# LANGUAGE FlexibleContexts #-}

module Euler14 (answer) where

import Control.Arrow ((***))
import Control.Monad.State
import Data.Function (fix)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Monoid (Ap (..))
import Data.Semigroup (Max (..))

answer :: Int
answer = getEq . fst . getMax . fromJust . flip evalState (Map.singleton 1 1) . getAp $ foldMap maxCollatz [1 .. 999999]
  where
    maxCollatz = Ap . fmap maxBySecond . withInput collatzLength
    maxBySecond = Just . Max . (Equal *** id)

collatzLength :: (MonadState (Map Int Int) f) => Int -> f Int
collatzLength = fix $ memoize . collatzLengthUnfixed
  where
    collatzLengthUnfixed recurse = fmap (+ 1) . recurse . collatz

collatz :: Int -> Int
collatz n = if even n then n `div` 2 else 3 * n + 1

memoize :: (Ord a, MonadState (Map a b) f) => (a -> f b) -> a -> f b
memoize f a = gets (Map.lookup a) >>= maybe (f a >>= returning (modify . Map.insert a)) return

newtype Equal a = Equal {getEq :: a}

instance Eq (Equal a) where
  _ == _ = True

instance Ord (Equal a) where
  compare _ _ = EQ

withInput :: (Functor f) => (a -> f b) -> a -> f (a, b)
withInput f a = (,) a <$> f a

returning :: (Monad f) => (a -> f b) -> a -> f a
returning f a = a <$ f a
