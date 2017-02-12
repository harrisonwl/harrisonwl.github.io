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
data Token = BEGIN | END | READ | WRITE | ID String |
             INTLITERAL Int | LPAREN | RPAREN | SEMICOLON |
             COMMA | ASSIGNOP | PLUSOP | MINUSOP | SCANEOF
                 deriving Show

--
-- define which strings constitute identifiers and which are keywords
--

keyword :: String -> Token
keyword str = case str of
  "begin" -> BEGIN
  "read"  -> READ
  "write" -> WRITE
  "end"   -> END
  _       -> ID str
  

--
-- a test case
--

test = "begin\n x:=7+y;\n read(y,z);\n end"

-- Want something like this:
-- ghci> scanName "" "heypal ..."
-- (ID "heypal"," ...")
-- ghci> scanName "" "begin ..."
-- (BEGIN," ...")

scanName :: String -> String -> (Token,String)
scanName acc str = case str of
  []                 -> (keyword acc, "")
  (c:cs) | isAlpha c -> scanName (acc ++ [c]) cs
         | otherwise -> (keyword acc, str)

-- Want something like this:
--   ghci> scanInt 0 "1965heypal"
--   (1965,"heypal")
--   ghci> scanInt 0 "heypal"
--   (0,"heypal")

scanInt :: Int -> String -> (Int,String)
scanInt i str = case str of
  []                 -> (i,"")
  (c:cs) | isDigit c -> scanInt (10 * i + digitToInt c) cs
         | otherwise -> (i,str)

-- Want something like:
-- ghci> scan test
-- [BEGIN,ID "x",ASSIGNOP,INTLITERAL 7,PLUSOP,ID "y",SEMICOLON,READ,LPAREN,ID "y",COMMA,ID "z",RPAREN,SEMICOLON,END,SCANEOF]
--

scan :: String -> [Token]
scan str = case str of
  []     -> [SCANEOF]
  (c:cs) | isDigit c -> let
                           (i,str') = scanInt 0 str
                        in
                           INTLITERAL i : scan str'
         | c==';'    -> SEMICOLON : scan cs
                        

  
