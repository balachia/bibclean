module Replacement where

import Text.Parsec
import Control.Applicative ((<$>), (<*>))
import Data.List (foldl', elemIndex)

data Replace = Split Char [Replace] | Leaf Char String

-- deal with latex mappings
-- | Return children of a Replace node (leaves return empty lists)
children :: Replace -> [Replace]
children (Split char xs) = xs
children (Leaf char xs) = []

-- | Return the character of a Replace node
replChar :: Replace -> Char
replChar (Split char xs) = char
replChar (Leaf char xs) = char

childChars :: Replace -> String
childChars node = map replChar (children node)

makeReplaceTree :: [(String, String)] -> Replace
makeReplaceTree maps = foldl' insertReplaceRoot (Split '.' []) maps
    where
        insertReplaceRoot :: Replace -> (String, String) -> Replace
        insertReplaceRoot tree (from, to) = Split '.' (updateChildren from to (children tree))
        updateChildren :: String -> String -> [Replace] -> [Replace]
        updateChildren from@(x:[]) to [] = [Leaf x to]
        updateChildren from@(x:xs) to [] = [Split x (updateChildren xs to [])]
        updateChildren from@(x:[]) to (y:ys) | x == (replChar y) = error ("overlapping replacement patterns" ++ [x] ++ " -> " ++ to)
                                             | otherwise = y:(updateChildren from to ys)
        updateChildren from@(x:xs) to (y:ys) | x == (replChar y) = (Split (replChar y) (updateChildren xs to (children y))):ys
                                             | otherwise = y:(updateChildren from to ys)

-- Replace tree based parsers
parseToReplaceRoot :: Replace -> Parsec String () String
parseToReplaceRoot root = do
    pre <- many (noneOf (childChars root))
    post <- eol <|> (choice (map parseChild' (children root))) <|> continueParse
    return (pre ++ post)
    where
        eol = eof >> return []
        parseChild' x = (++) <$> (parseChild x) <*> (parseToReplaceRoot root)
        continueParse = (:) <$> anyChar <*> (parseToReplaceRoot root)

parseChild :: Replace -> Parsec String () String
parseChild (Leaf x to) = try $ do
    char x
    return to
parseChild (Split x children) = try $ do
    char x
    choice (map parseChild children)

