{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative ((<$>), (<*>))
import Text.BibTeX.Parse (file)
import Text.BibTeX.Entry
import Text.BibTeX.Format (entry)
import Text.Parsec as PS
import Text.Parsec.String
{-import Data.List.Split (splitOn)-}
import Data.List (foldl', elemIndex)
import Data.List.Utils (split, replace, join)
import Data.Char (toLower)
import qualified Data.Map as Map
{-import Data.List (intercalate)-}
import Data.Maybe (isJust, fromJust, isNothing)
import qualified Data.ByteString as BL
import Data.ByteString.Char8 (unpack)
{-import qualified Data.Csv as CSV-}
import qualified Data.CSV.Conduit as CSV
import qualified Data.Vector as V
import Debug.Trace (trace)

import Authors

data Replace = Split Char [Replace] | Leaf Char String

instance Show Replace where
    show (Leaf char string) = [char] ++ " :: " ++ string ++ "\n"
    show (Split char (x:xs)) = [char] ++ (show' 0 1 x) ++ (concatMap (show' 1 1) xs)
        where
            show' :: Int -> Int -> Replace -> String
            show' m n (Leaf char' string) = (replicate m ' ') ++ [char'] ++ " :: " ++ string ++ "\n"
            show' m n (Split char' (x':xs')) = (replicate m ' ') ++ [char'] ++ (show' 0 (n+1) x') ++ (concatMap (show' (n+1) (n+1)) xs')

main = do
    result <- parseFromFile file "/Users/avashevko/Documents/library.bib"
    oldres <- parseFromFile file "/Users/avashevko/Documents/library-hs.bib"
    texrepl <- bibtexSwaps
    cleanrepl <- cleanSwaps
    let localProcess = process (makeFieldProcessor texrepl procfields) (makeIdProcessor cleanrepl idfields)
    case result of 
        Left err -> print err
        Right xs -> writeFile "/Users/avashevko/Documents/library-hs.bib" (concat $ map entry $ localProcess xs)
    where
        bibtexSwaps = linkMapping <$> (loadMapping "latex-crappy.csv") <*> (loadMapping "latex-good.csv")
        cleanSwaps = linkMapping <$> (bibtexSwaps) <*> (loadMapping "latex-short.csv")
        {-fieldsfcns repls = map (sanitizeField repls) ["author","editor","institution","file"]-}
        idfields = ["author","editor"]
        procfields = ["author","editor","institution","file"]

-- | 
makeFieldProcessor :: [(String,String)] -> [String] -> (Map.Map String String -> Map.Map String String)
makeFieldProcessor repls fields = foldl' (.) id fieldProcessors
    where
        fieldProcessors = map (sanitizeField repls) fields

{-swap :: (a,b) -> (b,a)-}
{-swap (a,b) = (b,a)-}

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
    pre <- PS.many (noneOf (childChars root))
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

-- | Take a map (a's -> b's) and a map (a's -> c's).
-- Make a map (b's -> c's) by joining b's and c's produced by common a's.
linkMapping :: [(String, String)] -> [(String, String)] -> [(String, String)]
linkMapping froms tos = filter (\(a,b) -> a /= b) $ map (match tos) froms
    where
        match targs (uc,vc) = (vc, fromJust $ lookup uc targs)
        -- TODO: make this error toleranter

loadMapping :: String -> IO ([(String, String)])
loadMapping infile = do
    csvData <- BL.readFile infile
    case (CSV.decodeCSV CSV.defCSVSettings csvData) of
        Left err -> do
            putStrLn $ show err
            return []
        Right (v :: V.Vector (V.Vector BL.ByteString)) -> do
            {-putStrLn $ show v-}
            return (map fromJust (filter isJust (V.toList (V.map toTuple (V.drop 1 v)))))
    where
        toTuple v = do
            v1 <- v V.!? 0
            v2 <- v V.!? 1
            return (cleanstring $ unpack v1, cleanstring $ unpack v2)
        cleanstring = replace "\\\\" "\\"

{-process :: ([(String,String)] -> [(String,String)]) -> ([(String,String)] -> String) -> [T] -> [T]-}
process :: (Map.Map String String -> Map.Map String String) -> (Map.Map String String -> String) -> [T] -> [T]
process fieldf idf ts = disambiguateIds (map newT ts)
    where
        newT :: T -> T
        newT oldT = Cons {entryType = entryType oldT, identifier = idf newfields, fields = Map.assocs newfields}
            where   
                newfields :: Map.Map String String
                newfields = fieldf (Map.fromList $ fields oldT)
        disambiguateIds :: [T] -> [T]
        disambiguateIds ts = reverse $ fst $ foldl' disambiguateIdsSub ([],Map.empty) ts
        disambiguateIdsSub :: ([T], Map.Map String Int) -> T -> ([T], Map.Map String Int)
        disambiguateIdsSub (ts, counter) oldT | isNothing oldVal = (oldT:ts,newCounter)
                                              | otherwise = (newT:ts,newCounter)
            where
                updateCounter k n o = o + 1
                (oldVal, newCounter) = Map.insertLookupWithKey updateCounter (identifier oldT) 0 counter
                newT = Cons {entryType = entryType oldT, identifier = (identifier oldT) ++ [(['a'..'z'] !! (fromJust oldVal))], fields = fields oldT}
        -- TODO: make this preserve disambiguation across new record additions

sanitizeField :: [(String,String)] -> String -> Map.Map String String -> Map.Map String String 
sanitizeField repls key assoc = Map.update replfcn key assoc
    where
        {-replfcn base = Just ((foldr (\(from,to) f -> f . (replace from to)) id repls) base)-}
        replfcn base = Just (sanitize repls base)

sanitize :: [(String,String)] -> String -> String
sanitize repls input | '\\' `elem` input = sanitizeFcn input
                     | otherwise = input
    where sanitizeFcn = foldr (\(from,to) f -> f . (replace from to)) id repls

makeIdProcessor :: [(String,String)] -> [String] -> Map.Map String String -> String
makeIdProcessor repls idfields fields = idstring -- (fst . head . Map.assocs) fields
    where
        idstrings = map (\field -> (Map.lookup field fields >>= return . pullNames . (sanitize repls))) idfields
        namestring = fromJust $ head $ dropWhile (isNothing) idstrings
        -- TODO: make this error tolerant
        {-idstring = namestring ++ (fromJust (Map.lookup "year" fields))-}
        idstring | isJust year = namestring ++ (fromJust year)
                 | otherwise = namestring ++ "XXXX"
        year = Map.lookup "year" fields

pullNames :: String -> String
pullNames x = (etal . shortenNames . splitNames' . splitVerbatim) x
    where
        {-splitNames = split " and "-}
        {-splitLastNames = map ((map toLower) . head . (split ","))-}
        {-repl ' ' = '-'-}
        {-repl c = c-}
        {-filt = (`notElem` "{}")-}
        shortenNames names = [((join "-") . (take maxNameLength) . (split " ")) name | name <- names]
        splitNames' vbtNames = concat [if vbt then [name] else (splitRawNames name) | (name,vbt) <- vbtNames]
        splitRawNames name = trace name $ map (head . (split ",")) (split " and " name)
        maxNameLength = 3

splitVerbatim :: String -> [(String,Bool)]
splitVerbatim str = result (foldr vbtAccum ([],[],0) str)
    where
        vbtAccum :: Char -> (String,[(String,Bool)],Int) -> (String,[(String,Bool)],Int)
        vbtAccum '}' (word, words, n) | n == 0  = ([], (word,False):words, n+1)
                                      | n > 0   = ('}':word, words, n+1)
        vbtAccum '{' (word, words, n) | n == 1  = ([], (word,True):words, 0)
                                      | n > 1   = ('{':word, words, n-1)
                                      | n < 1   = error ("Verbatim parsing error (too many open brackets) in :: " ++ str)
        {-vbtAccum chr (word, words, n) = (chr:word, words, n)-}
        vbtAccum chr (word, words, n) = ((toLower chr):word, words, n)
        result (last,res,n) | n == 0 = (last,False):res
                            | otherwise = error ("Verbatim parsing error (too many closed brackets) in :: " ++ str)
        

pullAuthors :: T -> Maybe String
pullAuthors x = do 
    authors <- lookup "author" (fields x)
    return (pullNames authors)
    {-let authorsSplit = splitOn " and " authors-}
    {-let lastNames = map ((map toLower) . head . (splitOn ",")) authorsSplit-}
    {-return (etal lastNames)-}
    {-where-}
        {-etal xs | length xs > 2 = intercalate "." ((take 2 xs) ++ ["ea"])-}
                {-| otherwise = intercalate "." (take 2 xs)-}

etal :: [String] -> String
etal xs | length xs > maxAuthors = join "." ((take truncateTo xs) ++ ["ea"])
        | otherwise = join "." (take maxAuthors xs)
    where
        maxAuthors = 2
        truncateTo = 2

