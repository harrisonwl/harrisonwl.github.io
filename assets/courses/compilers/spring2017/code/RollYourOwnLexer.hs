module RollYourOwnLexer where

--
-- https://hackage.haskell.org/package/base-4.9.1.0/docs/Data-Char.html
--
import Data.Char

-- In particular, we're using:
--   isAlpha    :: Char -> Bool
--   isDigit    :: Char -> Bool
--   isSpace    :: Char -> Bool
--   digitToInt :: Char -> Int


-- typedef enum token_types { BEGIN, END, READ, WRITE,
--                            ID, INTLITERAL, LPAREN,RPAREN,
--                            SEMICOLON, COMMA,ASSIGNOP, PLUSOP,
--                            MINUSOP,SCANEOF
--                          } token;

--
-- define what a token is
--
data Token = BEGIN | END | READ | WRITE | ID String
           | INTLITERAL Int | LPAREN | RPAREN
           | SEMICOLON | COMMA | ASSIGNOP | PLUSOP
           | MINUSOP | SCANEOF
           deriving Show

--
-- a test case
--

test = "begin\n x:=7+y;\n read(y,z);\n end"

scanName :: String -> String -> (String,String)
scanName s [] = (s,[])
scanName s (c:cs) | isAlpha c = scanName (s ++ [c]) cs
                  | otherwise = (s,c:cs)

-- Want something like this:
--   ghci> scanInt 0 "1965heypal"
--   (1965,"heypal")
--   ghci> scanInt 0 "heypal"
--   (0,"heypal")

scanInt :: Int -> String -> (Int,String)
scanInt n []     = (n,[])
scanInt n (c:cs) | isDigit c = scanInt (n * 10 + digitToInt c) cs
                 | otherwise = (n,c:cs)

scan str = case str of
--  []                 -> Just SCANEOF
  (c:cs) | isAlpha c -> Just 
           where (s,str') = scanName "" str

                 

--
-- define which strings constitute tokens
--

-- Recall Maybe type:
--   data Maybe a = Just a | Nothing

recognize str = case str of
  "begin" -> Just BEGIN
  "end"   -> Just END
  "read"  -> Just READ
  "write" -> Just WRITE
  "("     -> Just LPAREN
  ")"     -> Just RPAREN
  ";"     -> Just SEMICOLON
  ":="    -> Just ASSIGNOP
  "+"     -> Just PLUSOP
  
