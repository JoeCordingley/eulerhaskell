module Euler22 (answer) where

answer :: IO Int
answer = fmap (sum . map score) $ readCommaSeparatedStringsFromFile "0022_names.txt"

readCommaSeparatedStringsFromFile :: String -> IO [String]
readCommaSeparatedStringsFromFile filename = do
  contents <- readFile filename
  return (splitOnComma contents)
  where

-- Helper function to split a string by commas
splitOnComma :: String -> [String]
splitOnComma [] = []
splitOnComma s =
  let (word, rest) = break (== ',') s
   in word : case rest of
        [] -> []
        (_ : xs) -> splitOnComma xs

score :: String -> Int
score = undefined
