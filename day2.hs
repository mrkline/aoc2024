import Data.List
import Data.Text qualified as T
import Data.Text.IO qualified as T

parse :: T.Text -> Int
parse = read . T.unpack

ascOrDesc :: [Int] -> Bool
ascOrDesc xs = xs == sort xs || xs == reverse (sort xs)

goodDiff :: (Int, Int) -> Bool
goodDiff (a, b) = d >= 1 && d <= 3 where d = abs (a - b)

safe :: [Int] -> Bool
safe xs = ascOrDesc xs && all goodDiff pairs where
    pairs = zip xs $ tail xs

-- [1, 2, 3, 4, 5] -> [[1,2,3,4,5],[2,3,4,5],[1,3,4,5],[1,2,4,5],[1,2,3,5],[1,2,3,4]]
withouts :: [Int] -> [[Int]]
withouts xs = xs : dropEach 0 where
    dropEach i
        | i == length xs = []
        | otherwise = dropAt i : dropEach (i + 1)
    dropAt idx = let (l, r) = splitAt idx xs in l ++ tail r

dampened :: [Int] -> Bool
dampened xs = any safe $ withouts xs

main :: IO ()
main = do
    i <- T.readFile "input/day2.txt"
    let i' = T.words <$> T.lines i
        input = fmap parse <$> i' :: [[Int]]
    -- Part 1
    print . length $ filter safe input
    -- Part 2
    print . length $ filter dampened input
