import Data.ByteString qualified as BS
import Data.ByteString.Internal (c2w, w2c)
import Data.Vector qualified as V

-- Get the (x, y)th char or Nothing if out of bounds.
idx :: V.Vector BS.ByteString -> (Int, Int) -> Maybe Char
idx v (x, y) = (v V.!? y) >>= (BS.!? x) >>= pure . w2c

-- XMAS every which way
directions :: [[(Int, Int)]]
directions = [
    [(0, 0), (1, 0), (2, 0), (3, 0)],
    [(0, 0), (-1, 0), (-2, 0), (-3, 0)],
    [(0, 0), (0, 1), (0, 2), (0, 3)],
    [(0, 0), (0, -1), (0, -2), (0, -3)],
    [(0, 0), (1, 1), (2, 2), (3, 3)],
    [(0, 0), (1, -1), (2, -2), (3, -3)],
    [(0, 0), (-1, 1), (-2, 2), (-3, 3)],
    [(0, 0), (-1, -1), (-2, -2), (-3, -3)]
    ]

-- Should have used Linear.V2
add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Check a given direction
isXmas :: ((Int, Int) -> Maybe Char) -> (Int, Int) -> [(Int, Int)] -> Bool
isXmas f origin dir = fmap f (add origin <$> dir) == fmap Just "XMAS"

-- Check ALL THE DIRECTIONS
xmases :: ((Int, Int) -> Maybe Char) -> (Int, Int) -> Int
xmases f origin = length $ filter (isXmas f origin) directions

-- M M
--  A
-- S S
diagonals :: [[(Int, Int)]]
diagonals = [
    [(-1, -1), (0, 0), (1, 1)],
    [(-1, 1), (0, 0), (1, -1)]
    ]

-- Does the given diagonal spell SAM/MAS?
isCrossmas :: ((Int, Int) -> Maybe Char) -> (Int, Int) -> [(Int, Int)] -> Bool
isCrossmas f origin diagonal = chars == fmap Just "MAS" || chars == fmap Just "SAM" where
    chars = fmap f (add origin <$> diagonal)

crossmases :: ((Int, Int) -> Maybe Char) -> (Int, Int) -> Bool
crossmases f origin = all (isCrossmas f origin) diagonals

main :: IO ()
main = do
    i <- BS.split (c2w '\n') <$> BS.readFile "input/day4.txt"
    let input = V.fromList i
        h = V.length input :: Int
        w = BS.length (input V.! 0) :: Int
        allIndexes = (,) <$> [0..(w - 1)] <*> [0..(h - 1)]
        input' = idx input
    -- Part 1
    print . sum $ xmases input' <$> allIndexes
    -- Part 2
    print . length $ filter (crossmases input') allIndexes
