{-# LANGUAGE OverloadedStrings #-}
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T

parse :: Text -> Int
parse = read . T.unpack

parseLine :: Text -> (Int, [Int])
parseLine l = (parse r, fmap parse (T.words rest)) where
    (r, rest) = (head s, last s)
    s = T.splitOn ":" l

type Op = (Int -> Int -> Int)

-- All permutations of the given ops, n times.
-- Took me way too long to play type Jenga
opPermutations :: [Op] -> Int -> [[Op]]
opPermutations _ 0 = [[]]
opPermutations ops i = mconcat $ perm (opPermutations ops $ i - 1) <$> ops where
    perm :: [[Op]] -> Op -> [[Op]]
    perm ts h = fmap (h:) ts

eval :: [Int] -> [Op] -> Int
eval (x:y:xs) (o:os) = eval (o x y : xs) os
eval [x] _ = x
eval [] _ = error "out of args"
eval _ [] = error "out of ops"

-- The number of valid combinations of the given ops when, applied to the vals, give answer.
valids :: [Op] -> Int -> [Int] -> Int
valids ops answer vals = length $ filter (== answer) perms where
    perms = eval vals <$> opPermutations ops (length vals - 1)

calibration :: [Op] -> (Int, [Int]) -> Int
calibration ops (answer, vals) = if valids ops answer vals > 0 then answer else 0

cat :: Int -> Int -> Int
cat a b = a * (10 ^ (floor (logBase 10 $ fromIntegral b) + 1)) + b

main :: IO ()
main = do
    t <- T.lines <$> T.readFile "input/day7.txt"
    let i = parseLine <$> t
    -- Part 1
    print . sum $ calibration [(+), (*)] <$> i
    -- Part 2
    print . sum $ calibration [(+), (*), cat] <$> i
