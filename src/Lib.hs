module Lib (
    someFunc,
) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- x :: [(Int, [Int])]
-- x = scanl g [] [1, 2, 3]
--  where
--    g ns a = (a, a : ns)

inits :: [a] -> [(a, [a])]
inits [] = []
inits (a : as) = (a, []) : map g (inits as)
  where
    g (a', as) = (a', a : as)

scanLs :: (a -> b -> b) -> b -> [a] -> [(a, b)]
-- scanLs f _ [] = []
-- scanLs f b (a : as) = (a, b) : scanLs f (f a b) as
scanLs _ _ [] = []
scanLs f b (a : as) = (a, b) : (map . fmap) (f a) (scanLs f b as)
