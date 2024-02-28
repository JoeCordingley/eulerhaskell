{-# LANGUAGE FlexibleContexts #-}

module Euler14(answer) where

import Control.Monad.State
import qualified Data.Map as Map
import Data.Map (Map)

answer :: Int
answer = flip evalState (Map.singleton 1 1) . fmap (fst . foldr maxBySnd (1,1)) $ traverse (withInput collatzLength) [1..999999] where
  maxBySnd (a, b) (c, d) = if (b > d) then (a, b) else (c, d)
  withInput f a = fmap ((,) a) $ f a

collatzLength :: MonadState (Map Int Int) f => Int -> f Int
collatzLength = fix (memoize . collatzLengthUnfixed) where
  collatzLengthUnfixed recurse i = fmap (+1) $ recurse (collatz i)


memoize :: Ord a => MonadState (Map a b) f => (a -> f b) -> a -> f b
memoize f a = do
  maybeB <- gets $ Map.lookup a
  case maybeB of
    Nothing -> do
      b <- f a
      modify $ Map.insert a b
      return b
    Just b -> return b

collatz :: Int -> Int
collatz n = if n `mod` 2 == 0 then n `div` 2 else 3*n + 1

