import Prelude hiding (Left, Right)
import Control.Lens
import Data.ByteString qualified as BS
import Data.ByteString.Internal (w2c)
import Data.HashSet qualified as HS
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Linear.V2

type Point = V2 Int

type Antennas = HashMap Char [Point]

antennas :: Point -> [Char] -> Antennas
antennas _ [] = mempty
antennas pt (x : xs) = case x of
    '\n' -> antennas (pt & _y +~ 1 & _x .~ 0) xs
    '.' -> antennas (pt & _x +~ 1) xs
    f -> HM.unionWith (<>) (HM.singleton f [pt]) (antennas (pt & _x +~ 1) xs)

antennas' :: [Char] -> Antennas
antennas' = antennas $ V2 0 0

width :: [Char] -> Int
width [] = error "empty grid"
width ('\n' : _) = 0
width (_ : xs) = 1 + width xs

height :: [Char] -> Int
height [] = 0
height ('\n' : xs) = 1 + height xs
height (_ : xs) = height xs

newtype Bounds = Bounds Point
    deriving (Show)

dims :: [Char] -> Bounds
dims xs = Bounds $ V2 (width xs) (height xs)

antinodesOf :: Bounds -> [Point] -> [Point]
antinodesOf _ [] = mempty
antinodesOf bounds (a : as) = filter (inBounds bounds) $ mconcat (antinode a <$> as) <>
    antinodesOf bounds as

antinode :: Point -> Point -> [Point]
antinode a b = [a - dist, b + dist] where
    dist = b - a

inBounds :: Bounds -> Point -> Bool
inBounds (Bounds (V2 bx by)) (V2 x y) = x >= 0 && y >= 0 && x < bx && y < by

harmonicsOf :: Bounds -> [Point] -> [Point]
harmonicsOf _ [] = mempty
harmonicsOf bounds (a : as) = mconcat (harmonic bounds a <$> as) <> harmonicsOf bounds as

harmonic :: Bounds -> Point -> Point -> [Point]
harmonic bounds a b =
    (bb $ iterate (subtract distance) a) ++ (bb . tail $ iterate (+ distance) a) where
    bb = boundedBy bounds
    distance = b - a

boundedBy :: Bounds -> [Point] -> [Point]
boundedBy b = takeWhile (inBounds b)

main :: IO ()
main = do
    bytes <- BS.readFile "input/day8.txt"
    let i = w2c <$> BS.unpack bytes
        bounds = dims i
        ants = antennas' i
    -- Part 1
    -- drawMap bounds . mconcat $ antinodesOf bounds <$> HM.elems ants
    print . HS.size . HS.fromList . mconcat $ antinodesOf bounds <$> HM.elems ants
    -- Part 2
    print . HS.size . HS.fromList . mconcat $ harmonicsOf bounds <$> HM.elems ants
