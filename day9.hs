import Data.Int
import Data.Maybe
import Deque.Lazy (Deque)
import Deque.Lazy qualified as D
import GHC.Exts

parse :: Char -> Int8
parse c = read [c]

data Contents = File Int | Free deriving (Show) -- File ID or free space

data Sector = Sector {
    sContents :: Contents,
    sLen :: Int8
} deriving (Show)


toDisk :: Int -> [Int8] -> [Sector]
toDisk _ [] = []
toDisk fid [f] = [Sector (File fid) f]
toDisk fid (f:s:rest) = Sector (File fid) f : Sector Free s : toDisk (fid + 1) rest

compactByBlock :: Deque Sector -> [Sector]
compactByBlock sm = case D.head sm of
    Just s@(Sector{sContents = File _}) -> s : compactByBlock (D.tail sm)
    Just Sector{sContents = Free} -> compactByBlock $ defragByBlock sm
    Nothing -> []

defragByBlock :: Deque Sector -> Deque Sector
defragByBlock sm = let
    -- We're comparing head and last
    h@Sector{sLen = hl} = fromJust $ D.head sm
    (l@Sector{sContents = lc, sLen = ll}, sansLast) = fromJust $ D.unsnoc sm
    middle = D.tail sansLast
    in case lc of
        Free -> defragByBlock sansLast -- Again, skipping over free space at the end.
        File _ -> go where
            go
                | hl < ll = -- Head free space < last. Shrink last and put some of it up front.
                    D.cons l{sLen = hl} $ D.snoc l{sLen = ll - hl} middle
                | hl > ll = -- Head free space > last. Put last up front after shrinking head.
                    D.cons l $ D.cons h{sLen = hl - ll} middle
                | hl == ll = D.cons l middle -- The last shall be first.
                | otherwise = error "absurd"

checksum :: Int -> [Sector] -> Int
checksum _ [] = 0
checksum posit (Sector{sContents = Free, sLen}:fs) = checksum (posit + fromIntegral sLen) fs
checksum posit (f@Sector{sContents = File fid, sLen}:fs) = if sLen == 0
    then checksum posit fs
    else posit * fid + checksum (posit + 1) (f{sLen = sLen - 1}:fs)

showDisk :: [Sector] -> IO ()
showDisk [] = putStr "\n"
showDisk ((Sector{sContents = File fid, sLen}:ss)) = do
    putStr . mconcat $ replicate (fromIntegral sLen) $ show fid
    showDisk ss
showDisk ((Sector{sContents = Free, sLen}:ss)) = do
    putStr $ replicate (fromIntegral sLen) '.'
    showDisk ss

main :: IO ()
main = do
    s <- getLine
    let m = parse <$> s
        i = toDisk 0 m
        i' = fromList i
    -- Part 1
    print $ checksum 0 $ compactByBlock i'
    -- Part 2
