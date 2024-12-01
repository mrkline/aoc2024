import Data.List
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T

parse :: Text -> Int
parse = read . T.unpack

main :: IO ()
main = do
    i <- T.readFile "input/day1.txt"
    let pairs = T.words <$> T.lines i
        f = parse . head <$> pairs
        s = parse . last <$> pairs
    -- Part 1
    let f' = sort f
        s' = sort s
        differences =  abs <$> zipWith (-) f' s'
    print $ sum differences
    -- Part 2
    let freq :: Int -> Int
        freq e = length $ filter (== e) s
        similarities = zipWith (*) f (freq <$> f)
    print $ sum similarities
