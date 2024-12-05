{-# LANGUAGE OverloadedStrings #-}
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.List
import Data.List.Split
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T

parse :: Text -> Int
parse = read . T.unpack

parsePair :: Text -> (Int, Int)
parsePair t = (h, l) where
    s = T.splitOn "|" t
    h = parse $ head s
    l = parse $ last s

-- ugh, multimap
mkOrders :: [(Int, Int)] -> IM.IntMap [Int]
mkOrders = foldl' (\acc (k, v) -> IM.insertWith (<>) k [v] acc) IM.empty

middle :: [a] -> a
middle xs = xs !! (length xs `div` 2)

-- A page is correct if the pages it must precede *aren't* in the set of pages we've seen so far
precedes :: IntMap [Int] -> Int -> IntSet -> Bool
precedes order cur seen = case order IM.!? cur of
    Just afters -> not $ any (`IS.member` seen) afters
    Nothing -> True

isCorrect :: IntMap [Int] -> [Int] -> Bool
isCorrect order xs = and pageChecks where
    pageChecks = zipWith (precedes order) xs (IS.fromList <$> inits xs)

-- Slightly-less-than-factorial-brute force:
-- For each value, try inserting it into the list every which way, and take the first of those
-- that still follows the rules.
reorder :: IntMap [Int] -> [Int] -> [Int]
reorder order = foldl' findWorking [] where
    findWorking :: [Int] -> Int -> [Int]
    findWorking sorted next = head $ filter (isCorrect order) (insertions next sorted)

-- xs with y inserted at each position
insertions :: a -> [a] -> [[a]]
insertions y xs = fmap (insertAt y xs) [0..l] where
    l = length xs

-- xs with y inserted at index i
insertAt :: a -> [a] -> Int -> [a]
insertAt y [] _ = [y]
insertAt y xs 0 = y : xs
insertAt y (x:xs) i = x : insertAt y xs (i - 1)

main :: IO ()
main = do
    l <- T.lines <$> T.readFile "input/day5.txt"
    let s = splitOn [""] l
        o = parsePair <$> head s
        updates = fmap (fmap parse) (T.splitOn "," <$> last s)
        orders = mkOrders o
    -- Part 1
    print . sum $ middle <$> filter (isCorrect orders) updates
    -- Part 2
    let wrongs = filter (not . isCorrect orders) updates
        righted = reorder orders <$> wrongs :: [[Int]]
    print . sum $ middle <$> righted
