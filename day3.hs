{-# LANGUAGE OverloadedStrings #-}
import Data.Char (isDigit)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T

takeString :: Text -> Text -> Maybe Text
takeString _ "" = Nothing
takeString pfx s = if pfx `T.isPrefixOf` s then Just (T.drop (T.length pfx) s) else Nothing

takeMul :: Text -> Maybe Text
takeMul = takeString "mul("

takeNum :: Text -> Maybe (Int, Text)
takeNum "" = Nothing
takeNum s = if (not $ T.null digits) then Just (num, rest) else Nothing where
    (digits, rest) = T.span isDigit s
    num = read $ T.unpack digits

takeChar :: Char -> Text -> Maybe Text
takeChar _ "" = Nothing
takeChar c s = if T.head s == c then Just (T.tail s) else Nothing

takeExpr :: Text -> Maybe (Int, Text)
takeExpr t = do
    (firstNum, r) <- takeMul t >>= takeNum
    (secondNum, r2) <- takeChar ',' r >>= takeNum
    fin <- takeChar ')' r2
    pure (firstNum * secondNum, fin)

walk :: Text -> [Int]
walk "" = []
walk s = case takeExpr s of
    Just (i, rest) -> i : walk rest
    Nothing -> walk $ T.tail s

takeDo :: Text -> Maybe Text
takeDo = takeString "do()"

takeDont :: Text -> Maybe Text
takeDont = takeString "don't()"

doIt :: Text -> [Int]
doIt "" = []
doIt s = case takeExpr s of
    Just (i, rest) -> i : doIt rest -- do by default
    Nothing -> case takeDont s of
        Just rest -> doNot rest -- Stop on don't()
        Nothing -> doIt $ T.tail s -- Keep on doin' it.

doNot :: Text -> [Int]
doNot "" = []
doNot s = case takeDo s of -- Don't until we do again.
    Just rest -> doIt rest
    Nothing -> doNot $ T.tail s

main :: IO ()
main = do
    input <- T.readFile "input/day3.txt"
    -- Part 1
    print . sum $ walk input
    -- Part 2
    print . sum $ doIt input
