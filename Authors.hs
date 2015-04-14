{-# LANGUAGE ScopedTypeVariables #-}

module Authors where

import Text.Parsec.String
import Text.Parsec as PS



-- name parser shit
{-eol = do {PS.try PS.eof; return []}-}
{-nameEnd = eol PS.<|> (PS.try (PS.lookAhead (PS.string ","))) PS.<|> PS.try (PS.lookAhead (PS.string " and "))-}

{-nameEnd' = do-}
    {-PS.try PS.eof-}

-- what can a name look like?
-- names are separated by " and "
-- within a name, you can potentially have escaped characters in braces
-- an entire name (pre comma) can be set in braces as a verbatim entry

-- parse entry enclosed in braces (assume initial brace has been matched)
-- allow internal braces (internal braced text preserves braces)
-- allow for return with optional brace enclosure
braced :: String -> String -> Parsec String () String
braced open close = do
    pre <- many (noneOf "{}")
    post <- closeMatch <|> openMatch
    return (open ++ pre ++ post)
    where
        closeMatch = char '}' >> return close
        openMatch = do
            char '{'
            res <- braced "{" "}"
            post' <- braced "" close
            return (res ++ post')

-- entry point for a name
-- first decision, is the entire name enclosed in braces (until comma or termination)?
-- terminates at " and ", eof
topLevel :: Parsec String () String
topLevel = do
    pre <- tlBraced <|> tlClean
    return pre
    where
        tlBraced = topLevelBraced
        tlClean = topLevelClean

-- braced name 
-- matches to final brace, followed by comma, " and ", eof
-- terminates at " and ", eof
topLevelBraced :: Parsec String () String
topLevelBraced = try $ do
    char '{'
    pre <- braced "" ""
    post <- topLevelComma <|> topLevelEnd
    return (pre ++ post)

-- un-braced name (tho can contain internal braces)
-- checks for trailing closing braces
-- reads through brace expressions and space expressions (i.e. spaces not followed by " and ")
-- matches through to comma
-- terminates at " and ", or eof
topLevelClean :: Parsec String () String
topLevelClean = do
    pre <- many (noneOf "{}, ")
    post <- tlcCloseBrace <|> tlcBraced <|> topLevelEnd <|> topLevelComma <|> tlcSpace
    return (pre ++ post)
    where
        tlcCloseBrace = do
            char '}'
            unexpected "closing brace"
        tlcBraced = do
            char '{'
            inpre <- braced "{" "}"
            inpost <- topLevelClean
            return (inpre ++ inpost)
        tlcSpace = do
            char ' '
            inner <- topLevelClean
            return (" " ++ inner)

-- matches text after a comma, dropping it
-- terminates in " and " or eof
topLevelComma = do
    char ','
    inpost <- topLevel
    return ""

-- matches the name break
-- i.e. eof or " and "
topLevelEnd = (eof >> return "") <|> (try (lookAhead (string " and ")) >> return "")

-- author name list is many top level names separated by " and " ending at eof
authorNames :: Parsec String () [String]
authorNames = sepBy topLevel (string " and ")

{-topLevelClean :: Parsec String () String-}
{-topLevelClean = do-}
    {-many (noneOf "{, ")-}
    {-inner <- braced-}
    {-return inner-}

-- cleaner, better version
-- parse till braces
--  in brace, parse till end brace
-- in top level, parse till end of name (comma, brace, " and ")
{-topLevelName :: PS.Parsec String () String-}
{-topLevelName = do-}
    {-pre <- PS.manyTill (PS.anyChar) nameEnd-}
    {-return pre-}
    {-where-}
        {-andMatch :: PS.Parsec String () String-}
        {-andMatch = PS.try (PS.lookAhead (PS.string " and ")) >> return ""-}
        {-commaMatch :: PS.Parsec String () String-}
        {-commaMatch = PS.char ',' >> topLevelName >> return ""-}


-- stuff after the comma
{-postComma = do {PS.char ','; PS.manyTill (PS.anyChar) nameEnd}-}

-- unbraced name
{-cleanName :: GenParser Char st String-}
{-cleanName = PS.manyTill (PS.noneOf "{},") (eol PS.<|> PS.try (PS.lookAhead (PS.string " and ")))-}
{-cleanName = PS.manyTill (PS.noneOf "{},") nameEnd-}

-- name in braces -- don't allow nested braces
{-braceName :: GenParser Char st String-}
{-braceName = PS.between (PS.char '{') (PS.char '}') (PS.many $ PS.noneOf "{}")-}

-- parses an author's name
{-mainName = braceName PS.<|> cleanName-}
{-nameChunk = do-}
    {-x <- mainName-}
    {-PS.many (PS.try postComma)-}
    {-return x-}

-- parse whole name
{-authorNames' = PS.sepBy nameChunk (PS.string " and ")-}

-- test cases
testNames :: IO ()
testNames = (sequence_ $ map testName testNames') >> putStrLn ""
    where
        testName :: (String,[String]) -> IO ()
        testName (x,y) = do
            let res = parse authorNames "" x
            case res of
                Left err -> putStrLn $ show err
                Right y' -> if y == y' then putStr "." else putStrLn ("\nIn " ++ (show x) ++ " expect " ++ (show y) ++ " but found " ++ (show y'))
        testNames' = [
            ("Bar",["Bar"]),
            ("B{\\'a}r",["B{\\'a}r"]),
            ("Bar, Foo and Baz, Ber",["Bar","Baz"]),
            ("{Bar, Foo}",["Bar, Foo"]),
            ("{Bar, Foo}, Baz",["Bar, Foo"]),
            ("{Bar, Foo} and Bert, Vert",["Bar, Foo","Bert"]),
            ("{Bar, Foo}, Baz and Bert, Vert",["Bar, Foo","Bert"])
            ]


