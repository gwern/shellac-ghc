{-
  Parsing functions.
  Copyright (C) 2005 Luis Francisco Araujo
-}

module Parse
    where

import Text.ParserCombinators.Parsec (Parser, char, (<|>), many, parse, manyTill,
                                     anyChar, noneOf, string)
import Text.ParserCombinators.Parsec.Prim as P (try)

----------------------------------------------------------------------------------
-- | Get command name.
getCmd :: String -> String
getCmd = concat . take 1 . words

-- | Get command line arguments.
getArg :: String -> [String]
getArg s = if all (== ' ') s then [] else tail $ hParse s

----------------------------------------------------------------------------------
-- | Main entry for parsing expressions (quote , special characters etc).
-- It takes the expression and returns it tokenized.
hParse :: String -> [String]
hParse = map (`esc` '\\')
         . filter (not . null)
         . concat
         . quotes

----------------------------------------------------------------------------------
-- | Use this function to restore a whitespace
-- in some expressions needed. (Those parsed with
-- splitRegex mainly).
restoreWhiteSpace :: (String -> String)
restoreWhiteSpace = (++ " ")

----------------------------------------------------------------------------------
-- | Split into a specific element.
splitInto :: (Ord a) => a -> [a] -> [[a]]
splitInto _ [] = []
splitInto c e = let (l , e') = break (== c) e
                    in
                    l : case e' of
                                [] -> []
                                (_:e'') -> splitInto c e''

----------------------------------------------------------------------------------
-- | Returns a boolean value if it finds
-- all the elements of a list.
findSubStr :: (Eq a) => [a] -> [a] -> Bool
findSubStr [] [] = False
findSubStr as bs = let f [] _ = True
                       f _ [] = False
                       f (a:as') (b:bs')
                           | a == b = f as' bs'
                           | a /= b = f as bs'
                       f _ _ = False
                       in
                       f as bs

----------------------------------------------------------------------------------
-- | Concatenate a list with a specifc delimiter.
joinWith :: [String] -> String -> String
joinWith xs y = foldr (\ a b -> a ++ (if null b then [] else y ++ b)) [] xs

----------------------------------------------------------------------------------
-- | Escape the special character 'c' in the expression.
esc :: String -> Char -> String
esc [] _ =  []
esc (x:y:xs) c
    | c == x = y : esc xs c
esc (x:xs) c = if x == c then [] else x : esc xs c

-- | Parse command haskell expressions.
type Terna = [String]

gParser :: Parser Terna -> String -> Terna
gParser p = f
    where
    f e = case (parse p "" e) of
              Left _ -> ["", "", (show e)]
              Right t -> t

parseCH :: String -> Terna
parseCH = gParser comHaskell

comHaskell :: Parser Terna
comHaskell = do{ b <- manyTill anyChar (P.try (string "(-"))
               ; p <- manyTill anyChar (P.try (string "-)"))
               ; a <- many anyChar
               ; return [b, p, a]
               }

-- | Parse quotes.
quotes :: String -> [[String]]
quotes [] = []
quotes e
    | null b && null q && null a = []
    | otherwise = ((words b) : [q] : quotes a)
         where
           [b, q, a] = parseQuote e
quotes' :: String -> [[String]]
quotes' [] = []
quotes' e
    | null b && null q && null a = []
    | otherwise = ((words b) : [q] : quotes' a)
         where
           [b, q, a] = parseQuote' e


parseQuote, parseQuote' :: String -> Terna
parseQuote = gParser quote
parseQuote' = gParser quote'

quote :: Parser Terna
quote = do b <- many (noneOf ['\"'])
           many (char '\"')
           s <- many $ (escapeseq <|> (noneOf "\""
                                        >>= (\x -> return [x])))
           many (char '\"')
           a <- many anyChar
           return [b, (concat s), a]

quote' :: Parser Terna
quote' = do b <- many (noneOf ['\''])
            many (char '\'')
            s <- many $ (escapeseq <|> (noneOf "'"
                                        >>= (\x -> return [x])))
            many (char '\'')
            a <- many anyChar
            return [b, (concat s), a]

escapeseq :: Parser String
escapeseq = (P.try $ string "''") <|>
            (P.try $ string "\\'")
