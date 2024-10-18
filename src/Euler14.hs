{-# LANGUAGE FlexibleContexts #-}

module Euler14 (answer) where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map

answer :: Int
answer = evalState (fst . foldr maxBySnd (1, 1) <$> traverse (withInput collatzLength) [1 .. 999999]) $ Map.singleton 1 1
  where
    maxBySnd (a, b) (c, d) = if b > d then (a, b) else (c, d)
    withInput f a = (,) a <$> f a

collatzLength :: MonadState (Map Int Int) f => Int -> f Int
collatzLength = fix (memoize . collatzLengthUnfixed)
  where
    collatzLengthUnfixed recurse i = (+ 1) <$> recurse (collatz i)

memoize :: (Ord a, MonadState (Map a b) f) => (a -> f b) -> a -> f b
memoize f a = do
    maybeB <- gets $ Map.lookup a
    case maybeB of
        Nothing -> do
            b <- f a
            modify $ Map.insert a b
            return b
        Just b -> return b

collatz :: Int -> Int
collatz n = if even n then n `div` 2 else 3 * n + 1
